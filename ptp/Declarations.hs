{-
    Copyright (C) 2010, 2011 Stanislav Bohm

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

module Declarations where

import qualified Data.Map as Map
import qualified Data.Set as Set

data NelType =
	TypeInt |
	TypeString |
	TypeBool |
	TypeFloat |
	TypeDouble |
	TypeTuple [NelType] |
	TypeArray NelType |
	TypeData String String TransportMode [ (String, String) ] {- name, rawName, transportMode, functions -}
	deriving (Show, Eq, Ord)

type VarSet = Set.Set String
type NelTypeSet = Set.Set NelType
type ID = Int
type TypeTable = Map.Map String NelType
type NelVarDeclaration = (String, NelType)

data NelExpression =
	ExprCall String [NelExpression] |
	ExprVar String |
	ExprParam String |
	ExprInt Int |
	ExprString String |
	ExprTuple [NelExpression] |
	ExprTrue | ExprFalse |
	ExprPath Path
	deriving (Show, Eq, Ord)

data PathItem = PathSingleton NelExpression | PathRange NelExpression NelExpression
	deriving (Show, Eq, Ord)

data Path = AbsPath [ PathItem ] | RelPath Int [ PathItem ]
	deriving (Show, Eq, Ord)

data Place = Place {
	placeId :: ID,
	placeName :: String,
	placeType :: NelType,
	placeInitCode :: Maybe String,
	placeInitExprs :: [NelExpression],
	placePaths :: Maybe Path
} deriving (Show)

instance Eq Place where p1 == p2 = placeId p1 == placeId p2

data Edge = Edge {
	edgePlaceId :: ID,
	edgeInscription :: EdgeInscription,
	edgeTarget :: Path
} deriving (Show,Eq)

data EdgeInscription = EdgeExpression NelExpression | EdgePacking String (Maybe NelExpression) deriving (Show, Eq)

data Transition = Transition {
	transitionId :: ID,
	transitionName :: String,
	edgesIn :: [Edge],
	edgesOut :: [Edge],
	transitionCode :: Maybe String,
	guard :: NelExpression
} deriving (Show)

instance Eq Transition where t1 == t2 = transitionId t1 == transitionId t2

data Project = Project {
	projectName :: String,
	places :: [Place],
	transitions :: [Transition],
	projectParameters :: [Parameter],
	typeTable :: TypeTable,
	events :: [Event],
	userFunctions :: [UserFunction],
	projectDescription :: String,
	forcePackers :: Bool
} deriving (Show)

data Event = Event {
	eventName :: String,
	eventCode :: String
} deriving (Show)

data Parameter = Parameter {
	parameterName :: String,
	parameterType :: NelType,
	parameterDescription :: String
} deriving (Show)

data UserFunction = UserFunction {
	ufunctionId :: ID,
	ufunctionName :: String,
	ufunctionParameters :: [NelVarDeclaration],
	ufunctionReturnType :: NelType,
	ufunctionCode :: String,
	ufunctionWithContext :: Bool
} deriving (Show)

data Unit = Unit {
	unitId :: ID,
	unitPlaces :: [Place],
	unitTransitions :: [Transition]
} deriving (Show)

data TransportMode = TransportDisabled | TransportDirect | TransportCustom
	deriving (Show, Eq, Ord)

data ParamType = ParamNormal | ParamConst | ParamRef
	deriving (Show, Eq, Ord)
