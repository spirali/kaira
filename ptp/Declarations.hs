
module Declarations where

import qualified Data.Map as Map
import qualified Data.Set as Set

data NelType =
	TypeInt |
	TypeString |
	TypeBool |
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
	ExprTrue | ExprFalse
	deriving (Show, Eq, Ord)

data Place = Place {
	placeId :: ID,
	placeName :: String,
	placeType :: NelType,
	placeInitCode :: String,
	placeInitExprs :: [NelExpression]
} deriving (Show)

instance Eq Place where p1 == p2 = placeId p1 == placeId p2

data Edge = Edge {
	edgePlaceId :: ID,
	edgeInscription :: EdgeInscription,
	edgeTarget :: Maybe NelExpression
} deriving (Show,Eq)

data EdgeInscription = EdgeExpression NelExpression | EdgePacking String (Maybe NelExpression) deriving (Show, Eq)

data Transition = Transition {
	transitionId :: ID,
	transitionName :: String,
	edgesIn :: [Edge],
	edgesOut :: [Edge],
	transitionCode :: String,
	guard :: NelExpression
} deriving (Show)

data Network = Network {
	networkId :: ID,
	places :: [Place],
	transitions :: [Transition],
	address :: NelExpression, {- address is considered as input expression, but in fact it is computed from "instances" and calling "+" -}
	instances :: NelExpression
} deriving (Show)

instance Eq Network where n1 == n2 = networkId n1 == networkId n2

data Project = Project {
	projectName :: String,
	networks :: [Network],
	projectParameters :: [Parameter],
	typeTable :: TypeTable,
	events :: [Event],
	userFunctions :: [UserFunction]
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
	ufunctionName :: String,
	ufunctionParameters :: [NelVarDeclaration],
	ufunctionReturnType :: NelType,
	ufunctionCode :: String
} deriving (Show)

data TransportMode = TransportDisabled | TransportDirect | TransportCustom
	deriving (Show, Eq, Ord)

data ParamType = ParamNormal | ParamConst
	deriving (Show, Eq, Ord)
