
module Declarations where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Type = TUndefined | 
			TVoid | 
			TInt | 
			TString | 
			TBool |
			TTuple [Type] | 
			TArray Type | 
			TRaw String |
			TPointer Type |
			TData String String TransportMode |
			TStruct String [VarDeclaration]
	deriving (Show, Eq, Ord)

type VarDeclaration = (String, Type) 
type FunDeclaration = (String, Type, [Type])
type VarSet = Set.Set String
type TypeSet = Set.Set Type
type ID = Int
type TypeTable = Map.Map String Type

data Declarations = Declarations {
	varDeclarations :: [VarDeclaration],
	funDeclarations :: [FunDeclaration] 
} deriving (Show, Eq)

data Expression = 
	ExprCall String [Expression] |
	ExprVar String |
	ExprParam String | 
	ExprInt Int |
	ExprString String |
	ExprTuple [Expression] |
	ExprAddr Expression |
	ExprAt Expression Expression {- index container -} |
	ExprDeref Expression |
	ExprTrue | ExprFalse
	deriving (Show, Eq, Ord)
	
data Instruction = 
	IExpr Expression |
	ISet Expression Expression |
	IIf Expression Instruction Instruction |
	IForeach String String Expression [Instruction] | {- var counterVar array body -}
	IStatement [VarDeclaration] [(String,Expression)] [Instruction] |
	IReturn Expression |
	IContinue |
	IInline String |
	INoop 
	deriving (Show, Eq)

data Function = Function {
	functionName :: String,
	instructions :: [Instruction],
	parameters :: [VarDeclaration],
	declarations :: [VarDeclaration],
	extraCode :: String,
	returnType :: Type
} deriving (Show)


data Place = Place { 
	placeId :: ID,
	placeName :: String, 
	placeType :: Type,
	placeInitCode :: String,
	placeInitExpr :: Maybe Expression
} deriving (Show)

instance Eq Place where p1 == p2 = placeId p1 == placeId p2

data Edge = Edge { 
	edgePlaceId :: ID,
	edgeInscription :: EdgeInscription, 
	edgeTarget :: Maybe Expression
} deriving (Show,Eq)

data EdgeInscription = EdgeExpression Expression | EdgePacking String (Maybe Expression) deriving (Show, Eq)

data Transition = Transition { 
	transitionId :: ID, 
	transitionName :: String,  
	edgesIn :: [Edge], 
	edgesOut :: [Edge],
	transitionCode :: String,
	guard :: Expression
} deriving (Show)

data Network = Network {
	networkId :: ID,
	places :: [Place],
	transitions :: [Transition],
	address :: Expression, {- address is considered as input expression, but in fact it is computed from "instances" and calling "+" -}
	instances :: Expression 
} deriving (Show) 

instance Eq Network where n1 == n2 = networkId n1 == networkId n2

data Project = Project {
	projectName :: String,
	networks :: [Network],
	projectParameters :: [Parameter]
} deriving (Show)

data Parameter = Parameter {
	parameterName :: String,
	parameterType :: Type,
	parameterDescription :: String
} deriving (Show)

data TransportMode = TransportDisabled | TransportDirect 
	deriving (Show, Eq, Ord)
