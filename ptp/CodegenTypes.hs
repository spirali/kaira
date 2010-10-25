module CodegenTypes where

import Declarations

import qualified Data.Set as Set
import qualified Data.Map as Map

data Type = TUndefined |
			TVoid |
			TInt |
			TString |
			TBool |
			TTuple [Type] |
			TArray Type |
			TRaw String |
			TPointer Type |
			TData String String TransportMode [ (String, String) ] | {- name, rawName, transportMode, functions -}
			TStruct String [VarDeclaration]
	deriving (Show, Eq, Ord)

type VarDeclaration = (String, Type)
type ParamDeclaration = (String, Type, ParamType)
type FunDeclaration = (String, Type, [Type])
type TypeSet = Set.Set Type

data Declarations = Declarations {
	varDeclarations :: [VarDeclaration],
	funDeclarations :: [FunDeclaration]
} deriving (Show, Eq)


data Expression =
	ECall String [Expression] |
	EVar String |
	EType String |
	EInt Int |
	EString String |
	ETuple [Expression] |
	EAddr Expression |
	EAt Expression Expression {- index container -} |
	EDeref Expression |
	ETrue | ExprFalse
	deriving (Show, Eq, Ord)

data Instruction =
	IExpr Expression |
	ISet Expression Expression |
	IIf Expression Instruction Instruction |
	IForeach String String Expression [Instruction] | {- var counterVar array body -}
	IStatement [VarDeclaration] [Instruction] |
	IDefine String Type (Maybe Expression) |
	IReturn Expression |
	IContinue |
	IInline String |
	INoop
	deriving (Show, Eq)


data Function = Function {
	functionName :: String,
	instructions :: [Instruction],
	parameters :: [ParamDeclaration],
	declarations :: [VarDeclaration],
	extraCode :: String,
	returnType :: Type
} deriving (Show)

