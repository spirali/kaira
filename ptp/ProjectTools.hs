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

module ProjectTools where

import Declarations
import Utils
import Base
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

isUserFunction :: Project -> String -> Bool
isUserFunction project name =
	any (\f -> ufunctionName f == name) (userFunctions project)

isUserFunctionWithContext :: Project -> String -> Bool
isUserFunctionWithContext project name =
	any (\f -> ufunctionName f == name && ufunctionWithContext f) (userFunctions project)

isUserFunctionWithoutContext :: Project -> String -> Bool
isUserFunctionWithoutContext project name =
	any (\f -> ufunctionName f == name && not (ufunctionWithContext f)) (userFunctions project)

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

lookupBy :: (Eq b) => String -> (a -> b) -> b -> [a] -> a
lookupBy errString fn id [] = error errString
lookupBy errString fn id (x:xs)
	| fn x == id = x
	| otherwise = lookupBy errString fn id xs

placeById :: Network -> ID -> Place
placeById network id =
	lookupBy ("placeById: Place " ++ show id) placeId id (places network)

placeById' :: Project -> ID -> Place
placeById' project id =
	lookupBy ("placeById': Place " ++ show id) placeId id (concatMap places (networks project))

placeSeqById :: Network -> ID -> Int
placeSeqById network id =  placeSeq network $ placeById network id

transitionById :: Project -> ID -> Transition
transitionById project id =
	lookupBy "TransitionById: Not found" transitionId id (concatMap transitions (networks project))

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
