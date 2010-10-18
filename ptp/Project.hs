
module Project (
	projectFromXml,
	placeSeqById,
	placeSeq,
	placeType,
	placeTypeById,
	placeTypeById',
	placeTypeByEdge,
	edgeNetwork,
	edgePlaceType,
	isNormalEdge,
	parameterTypeByName,
) where

import Declarations
import Parser
import ProjectTools
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Text.XML.Light as Xml

placeById :: Network -> ID -> Place
placeById network id = 
	lookup (places network) id
	where 
		lookup [] id = error $ "placeById: Place " ++ show id ++ " not found. Places: " ++ show (places network)
		lookup (x:xs) id 
			| placeId x == id = x
			| otherwise = lookup xs id

placeById' :: Project -> ID -> Place
placeById' project id = 
	lookup (concatMap places (networks project)) id
	where 
		lookup [] id = error $ "placeById': Place " ++ show id ++ " not found."
		lookup (x:xs) id 
			| placeId x == id = x
			| otherwise = lookup xs id

placeSeqById :: Network -> ID -> Int
placeSeqById network id =  placeSeq network $ placeById network id

placeSeq :: Network -> Place -> Int
placeSeq network place = 
	case List.elemIndex place (places network) of
		Just x -> x
		Nothing -> error $ "Place not found"

placeTypeById :: Network -> ID -> Type
placeTypeById network id = placeType $ placeById network id

placeTypeById' :: Project -> ID -> Type
placeTypeById' project id = placeType $ placeById' project id

placeTypeByEdge :: Project -> Edge -> Type
placeTypeByEdge project edge = placeTypeById' project (edgePlaceId edge)

qstr :: String -> Xml.QName
qstr str = Xml.QName str Nothing Nothing

xmlAttr' :: String -> Xml.Element -> String -> String
xmlAttr' str e defaultValue = 
	case Xml.findAttr (qstr str) e of
		Just x -> x
		Nothing -> defaultValue

xmlAttr :: String -> Xml.Element -> String
xmlAttr str e = xmlAttr' str e (error $ "Attribute '" ++ str ++ "' not found in element '" ++ Xml.qName (Xml.elName e) ++ "'.")

xmlAttrInt :: String -> Xml.Element -> Int
xmlAttrInt str e = read $ xmlAttr str e

codeContent :: Xml.Element -> String
codeContent e = case Xml.findElement (qstr "code") e of
					Just c -> Xml.strContent c
					Nothing -> ""

source :: Xml.Element -> String -> String
source element place = "*" ++ (xmlAttr "id" element) ++ "/" ++ place

placeFromElement :: TypeTable -> Xml.Element -> Place
placeFromElement types e = 
	Place { 
		placeId = xmlAttrInt "id" e, 
		placeName =  xmlAttr "name" e,
		placeType = parseType types (source e "type") $ xmlAttr "type" e, 
		placeInitCode = codeContent e, 
		placeInitExpr = parseExpr' (source e "init") $ xmlAttr' "init-expr" e "" 
	} 

edgeFromElement :: Xml.Element -> Edge
edgeFromElement e = 
	Edge { 
		edgePlaceId = xmlAttrInt "place-id" e, 
		edgeInscription = parseEdgeInscription (source e "inscription") $ xmlAttr "expr" e,
		edgeTarget = parseExpr' (source e "target") $ xmlAttr' "target" e ""
	}

transitionFromElement :: Xml.Element -> Transition
transitionFromElement e = 
	Transition { 
		transitionName = name, 
		transitionId = id, 
		edgesIn = orderEdgesByDependancy edgesIn, 
		edgesOut = edgesOut, 
		transitionCode = codeContent e,
		guard = parseGuard (source e "guard") $ xmlAttr' "guard" e ""
	} 
	where
		id = idFromElement e
		name = xmlAttr "name" e
		edgesIn = map edgeFromElement $ Xml.findElements (qstr "edge-in") e
		edgesOut = map edgeFromElement $ Xml.findElements (qstr "edge-out") e

placesFromElement :: TypeTable -> Xml.Element -> [Place]
placesFromElement types e = 
	map (placeFromElement types) (Xml.findElements (qstr "place") e)

transitionsFromElement :: Xml.Element -> [Transition]
transitionsFromElement e = 
	map transitionFromElement $ Xml.findElements (qstr "transition") e

networkFromElement :: TypeTable -> Expression -> Xml.Element -> Network
networkFromElement types addr e =
	Network {
		networkId = idFromElement e,
		places = placesFromElement types e,
		transitions = transitionsFromElement e,
		address = addr,
		instances = parseExpr (source e "instances") $ xmlAttr "instances" e
	}

addressesFromElement :: Xml.Element -> [Expression]
addressesFromElement e = 
	getAddress (Xml.findElements (qstr "net") e) (ExprInt 0)
	where 
		networkSize e = parseExpr (source e "instances") $ xmlAttr "instances" e
		getAddress [] _ = []
		getAddress (e:es) n = n:(getAddress es $ ExprCall "+" [ n,networkSize e])

parameterFromElement :: TypeTable -> Xml.Element -> Parameter
parameterFromElement types e = Parameter {
	parameterName = xmlAttr "name" e,
	parameterType = parseType types "" (xmlAttr "type" e), {- FIXME: Source of parameter -}
	parameterDescription = xmlAttr' "description" e ""
}

idFromElement :: Xml.Element -> ID
idFromElement = xmlAttrInt "id"

placeToNetworkList :: Project -> [(ID, Network)]
placeToNetworkList project = 
	concatMap extractPlaces (networks project)
	where extractPlaces network = [ (placeId y, network) | y <- places network ]

edgeNetwork :: Project -> Edge -> Network
edgeNetwork project edge = 
	case List.find edgeOfNetwork (networks project) of
		Just n -> n
		Nothing -> error "edgeNetwork: Network not found"
	where 
		edgeOfNetwork n = List.elem (edgePlaceId edge) (map placeId (places n))

edgePlaceType :: Project -> Edge -> Type
edgePlaceType project edge = placeTypeById' project (edgePlaceId edge)

parameterTypeByName :: Project -> String -> Type
parameterTypeByName project paramName = 
	case List.find (\p -> parameterName p == paramName) (projectParameters project) of
		Just x -> parameterType x
		Nothing -> error $ "Parameter '" ++ paramName ++ "' not defined"

externTypeFromElement :: Xml.Element -> (String, Type)
externTypeFromElement e = (name, TData name rawType transportMode codes)
	where 
		name = xmlAttr "name" e
		rawType = xmlAttr "raw-type" e
		codes = [ (xmlAttr "name" e, Xml.strContent e) | e <- Xml.findElements (qstr "code") e ]
		transportMode = case xmlAttr "transport-mode" e of
							"Disabled" -> TransportDisabled
							"Direct" -> TransportDirect
							"Custom" -> TransportCustom
							_ -> error "externTypeFromElement: Unknown transport mode"

externTypesFromElement :: Xml.Element -> TypeTable
externTypesFromElement e =
	Map.fromList $ map externTypeFromElement (Xml.findElements (qstr "extern-type") e)

projectTypesFromElement :: Xml.Element -> TypeTable
projectTypesFromElement e = Map.union (externTypesFromElement e) standardTypes

eventFromElement :: Xml.Element -> Event
eventFromElement e = Event {
	eventName = xmlAttr "name" e,
	eventCode = Xml.strContent e
}

projectFromXml :: String -> Project
projectFromXml xml =
	Project {
			projectName = "project",
			networks = networks,
			projectParameters = params,
			typeTable = types,
			events = events
		 }
	where
		root = head $ Xml.onlyElems (Xml.parseXML xml)
		addresses = addressesFromElement root
		networkElements = Xml.findElements (qstr "net") root
		networks = map loadNet $ zip addresses networkElements
		loadNet (a,e) = networkFromElement types a e
		configuration = Maybe.fromJust $ Xml.findElement (qstr "configuration") root
		params = map (parameterFromElement types) $ Xml.findElements (qstr "parameter") configuration
		events = map eventFromElement $ Xml.findElements (qstr "event") configuration
		types = projectTypesFromElement configuration
