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
import qualified Data.Maybe as Maybe

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

hasCode :: Transition -> Bool
hasCode = Maybe.isJust . transitionCode

hasInitCode :: Place -> Bool
hasInitCode = Maybe.isJust . placeInitCode

pathFreeVariables :: Path -> Map.Map String NelType
pathFreeVariables path = case path of
		(AbsPath es) -> listVariables es
		(RelPath 0 es) -> listVariables es
	where
		listVariables [] = Map.empty
		listVariables (ExprVar x:rest) = unionVariableTypes (Map.singleton x TypeInt) (listVariables rest)
		listVariables (_:rest) = listVariables rest

-- |Return all variables in edge (main expression, target, limit)
extractEdgeVariables :: Project -> Edge -> Map.Map String NelType
extractEdgeVariables project edge =
	unionVariableTypes (pathFreeVariables (edgeTarget edge)) (case edgeInscription edge of
		EdgeExpression x -> (variableTypes project x (edgePlaceType project edge))
		EdgePacking name (Just x) -> unionVariableTypes (Map.singleton name $ TypeArray (edgePlaceType project edge)) (variableTypes project x TypeInt)
		EdgePacking name Nothing -> Map.singleton name $ TypeArray (edgePlaceType project edge))

edgesFreeVariables :: Project -> [Edge] -> [NelVarDeclaration]
edgesFreeVariables project edges =
	Map.toList $ unionsVariableTypes $ map (extractEdgeVariables project) edges

{- On edges in & out -}
transitionFreeVariables :: Project -> Transition -> [NelVarDeclaration]
transitionFreeVariables project transition =
	edgesFreeVariables project $ (edgesIn transition) ++ (edgesOut transition)

transitionFreeVariablesIn :: Project-> Transition -> [NelVarDeclaration]
transitionFreeVariablesIn project transition =
	edgesFreeVariables project (edgesIn transition)

transitionFreeVariablesOut :: Project -> Transition -> [NelVarDeclaration]
transitionFreeVariablesOut project transition =
	edgesFreeVariables project (edgesOut transition)

lookupBy :: (Eq b) => String -> (a -> b) -> b -> [a] -> a
lookupBy errString fn id [] = error errString
lookupBy errString fn id (x:xs)
	| fn x == id = x
	| otherwise = lookupBy errString fn id xs

placeById :: Project -> ID -> Place
placeById project id = lookupBy ("placeById: Place " ++ show id) placeId id (places project)

{-
placeSeqById :: Project -> ID -> Int
placeSeqById project id =  placeSeq project $ placeById project id
-}

transitionById :: Project -> ID -> Transition
transitionById project id =
	lookupBy "TransitionById: Not found" transitionId id (transitions project)

{-
placeSeq :: Project -> Place -> Int
placeSeq project place =
	case List.elemIndex place (places project) of
		Just x -> x
		Nothing -> error $ "Place not found"
-}

edgePlace :: Project -> Edge -> Place
edgePlace project = (placeById project) . edgePlaceId


placeTypeById :: Project -> ID -> NelType
placeTypeById project = placeType . (placeById project)

edgePlaceType :: Project -> Edge -> NelType
edgePlaceType project edge = placeTypeById project (edgePlaceId edge)

inputPlaces :: Project -> Transition -> [Place]
inputPlaces project transition = List.nub $ map ((placeById project) . edgePlaceId) (edgesIn transition)

outputTransitions :: Project -> Place -> [Transition]
outputTransitions project place =
	filter (\t -> place `elem` inputPlaces project t) (transitions project)

projectUnits :: Project -> [Unit]
projectUnits project = countedMap unit $ joinOverlapped initSets
	where
		initSets = (map (inputPlaces project) $ transitions project) ++ orphans
		orphans = case (places project) List.\\ (concatMap (inputPlaces project) $ transitions project) of
			[] -> []
			x -> [x]
		unit n places = Unit { unitPlaces = places,
			unitId = n,
			unitTransitions = List.foldr List.union [] $ map (outputTransitions project) places }
