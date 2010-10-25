
module ProjectTools where

import Declarations
import Utils
import Base
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

isUserFunction :: Project -> String -> Bool
isUserFunction project name =
	any (\f -> (ufunctionName f == name)) (userFunctions project)

hasEvent :: Project -> String -> Bool
hasEvent project name =
	List.elem name $ map eventName (events project)

-- |Return all variables in edge (main expression, target, limit)
extractEdgeVariables :: Project -> Edge -> [Map.Map String NelType]
extractEdgeVariables project edge =
	case edgeInscription edge of
		EdgeExpression x -> (variableTypes project x (edgePlaceType project edge)) : targetExpr
		EdgePacking name (Just x) -> [ Map.singleton name $ TypeArray (edgePlaceType project edge), (variableTypes project x TypeInt) ] ++ targetExpr
		EdgePacking name Nothing -> (Map.singleton name $ TypeArray (edgePlaceType project edge)) : targetExpr
	where targetExpr = case edgeTarget edge of
		Just x -> [(variableTypes project x TypeInt)]
		Nothing -> []

edgesFreeVariables :: Project -> [Edge] -> [NelVarDeclaration]
edgesFreeVariables project edges =
	Map.toList declsMap
	where
		declsMap = unionsVariableTypes $ concatMap (extractEdgeVariables project) edges

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

placeTypeById :: Network -> ID -> NelType
placeTypeById network id = placeType $ placeById network id

placeTypeById' :: Project -> ID -> NelType
placeTypeById' project id = placeType $ placeById' project id

placeTypeByEdge :: Project -> Edge -> NelType
placeTypeByEdge project edge = placeTypeById' project (edgePlaceId edge)

edgePlaceType :: Project -> Edge -> NelType
edgePlaceType project edge = placeTypeById' project (edgePlaceId edge)
