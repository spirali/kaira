{-
    Copyright (C) 2010 Stanislav Bohm

    This file is part of Kaira.

    Kaira is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, version 3 of the License, or
    (at your option) any later version.

    Kaira is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
-}

module Project (
	projectFromXml,
	placeSeqById,
	placeSeq,
	placeType,
	placeTypeById,
	placeTypeById',
	transitionById,
	placeTypeByEdge,
	edgeNetwork,
	edgePlaceType,
	isNormalEdge,
	parameterTypeByName,
) where

import Declarations
import Parser
import ProjectTools
import Base
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Text.XML.Light as Xml

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

xmlAttrBool :: String -> Xml.Element -> Bool
xmlAttrBool str e = case xmlAttr str e of
		"true" -> True
		"True" -> True
		"TRUE" -> True
		_ -> False

codeContent :: Xml.Element -> Maybe String
codeContent e = Xml.findElement (qstr "code") e >>= return . Xml.strContent

source :: Xml.Element -> String -> String
source element place = "*" ++ (xmlAttr "id" element) ++ "/" ++ place

placeFromElement :: TypeTable -> Xml.Element -> Place
placeFromElement types e =
	Place {
		placeId = xmlAttrInt "id" e,
		placeName =  xmlAttr "name" e,
		placeType = parseType types (source e "type") $ xmlAttr "type" e,
		placeInitCode = codeContent e,
		placeInitExprs = parseInitExpr (source e "init") $ xmlAttr' "init-expr" e ""
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

networkFromElement :: TypeTable -> NelExpression -> Xml.Element -> Network
networkFromElement types addr e =
	Network {
		networkId = idFromElement e,
		places = placesFromElement types e,
		transitions = transitionsFromElement e,
		address = addr,
		instances = parseExpr (source e "instances") $ xmlAttr "instances" e
	}

addressesFromElement :: Xml.Element -> [NelExpression]
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

externTypeFromElement :: Xml.Element -> NelVarDeclaration
externTypeFromElement e = (name, TypeData name rawType transportMode codes)
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

userFunctionFromElement :: TypeTable -> Xml.Element -> UserFunction
userFunctionFromElement types e = UserFunction {
	ufunctionId = read (xmlAttr "id" e), ufunctionName = xmlAttr "name" e,
	ufunctionReturnType = parseType types source $ xmlAttr "return-type" e,
	ufunctionParameters = parseParameters types source $ xmlAttr "parameters" e,
	ufunctionWithContext = xmlAttrBool "with-context" e,
	ufunctionCode = Xml.strContent e
} where source = "Function " ++ xmlAttr "name" e

projectFromXml :: String -> Project
projectFromXml xml =
	Project {
			projectName = "project",
			networks = networks,
			projectParameters = params,
			typeTable = types,
			events = events,
			userFunctions = ufunctions,
			projectDescription = Xml.strContent description
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
		ufunctions = map (userFunctionFromElement types) $ Xml.findElements (qstr "function") configuration
		description = Maybe.fromJust $ Xml.findElement (qstr "description") root
