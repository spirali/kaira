
module Declarations where

import qualified Data.Map as Map
import qualified Data.Set as Set

data Type = TUndefined | 
			TVoid | 
			TInt | 
			TString | 
			TTuple [Type] | 
			TArray Type | 
			TData String |
			TPointer Type |
			TStruct String [VarDeclaration]
	deriving (Show, Eq, Ord)

type VarDeclaration = (String, Type) 
type VarSet = Set.Set String
type TypeSet = Set.Set Type
type ID = Int

data Expression = 
	ExprCall String [Expression] |
	ExprVar String |
	ExprInt Int |
	ExprString String |
	ExprTuple [Expression] |
	ExprAddr Expression |
	ExprAt Expression Expression {- index container -} |
	ExprDeref Expression
	deriving (Show, Eq, Ord)
	
data Instruction = 
	IExpr Expression |
	ISet Expression Expression |
	IIf Expression Instruction Instruction |
	IForeach String String Expression [Instruction] | {- var counterVar array body -}
	IStatement [VarDeclaration] [Instruction] |
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
	edgeExpr :: Expression, 
	edgeTarget :: Maybe Expression
} deriving (Show,Eq)

data Transition = Transition { 
	transitionId :: ID, 
	transitionName :: String,  
	edgesIn :: [Edge], 
	edgesOut :: [Edge],
	transitionCode :: String,
	target :: Expression
} deriving (Show)

data Network = Network {
	networkId :: ID,
	places :: [Place],
	transitions :: [Transition],
	address :: Int,
	instances :: Int
} deriving (Show) 

instance Eq Network where n1 == n2 = networkId n1 == networkId n2

data Project = Project {
	projectName :: String,
	networks :: [Network]
} deriving (Show)
