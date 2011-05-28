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

module CodegenTools where

import Declarations
import CodegenTypes
import Utils

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map


-- | Shortcut for instruction calling fuction
icall name params = IExpr $ ECall name params

idefine :: String -> Type -> Expression -> Instruction
idefine name t expr = IDefine name t (Just expr)

idefineEmpty :: String -> Type -> Instruction
idefineEmpty name t = IDefine name t Nothing

sizeType = TRaw "size_t"

-- | Makes expression that call function if there is more then one expression, otherwise expression is returned
callIfMore :: String -> [Expression] -> Expression
callIfMore _ [] = error "callIfMore called with empty list"
callIfMore _ [x] = x
callIfMore name xs = ECall name xs

-- | Return all expressions in instruction
instructionExprs :: Instruction -> [Expression]
instructionExprs (IExpr expr) = [expr]
instructionExprs (ISet expr1 expr2) = [expr1, expr2]
instructionExprs (IIf expr i1 i2) = [ expr ] ++ instructionExprs i1 ++ instructionExprs i2
instructionExprs (IStatement instrs) = concatMap instructionExprs instrs
instructionExprs (IForeach _ _ _ expr instrs) = concatMap instructionExprs instrs
instructionExprs INoop = []
instructionExprs _ = []

-- |Returns expression that computes size of memory footprint of result of expr
exprMemSize :: Type -> Expression -> Expression
exprMemSize TInt expr = ECall "sizeof" [EVar "int"]
exprMemSize TFloat expr = ECall "sizeof" [EVar "float"]
exprMemSize TDouble expr = ECall "sizeof" [EVar "double"]
exprMemSize TString expr = ECall "+" [ ECall "sizeof" [ EType "size_t" ], ECall ".size" [ expr ] ]
-- exprMemSize (TTuple types) expr = ECall "+" [ exprMemSize t (EAt (EInt x) expr) | (x, t) <- zip [0..] types ]
exprMemSize (TData _ rawType TransportDirect _) expr = ECall "sizeof" [ EType rawType ]
exprMemSize (TData name _ TransportCustom _) expr = ECall (name ++ "_getsize") [ expr ]
exprMemSize t expr = error $ "exprMemSize: " ++ show t

canBeDirectlyPacked :: Type -> Bool
canBeDirectlyPacked TInt = True
canBeDirectlyPacked TDouble = True
canBeDirectlyPacked TFloat = True
canBeDirectlyPacked (TData _ _ TransportDirect _) = True
canBeDirectlyPacked (TStruct _ _) = error "canBeDirectlyPacked: Struct not implemented"
canBeDirectlyPacked _ = False

-- |Return declarations in instructions (ie, it search for IDefine instructions)

typeString :: Type -> String
typeString (TArray t) = "std::vector<" ++ typeString t ++ " >"
typeString (TTemplate t1 t2) = typeString t1 ++ "<" ++ typeString t2 ++ " >"
typeString (TRaw d) = d
typeString (TPointer t) = typeString t ++ "*"
typeString TString = "std::string"
typeString TBool = "bool"
typeString (TData _ rawType _ _) = rawType
typeString x = typeSafeString x

-- |Converts type to string in way that string can be used as part of identifier
typeSafeString :: Type -> String
typeSafeString TVoid = "void"
typeSafeString TInt = "int"
typeSafeString TFloat = "float"
typeSafeString TDouble = "double"
typeSafeString TString = "string"
typeSafeString (TTemplate t1 t2) = typeSafeString t1 ++ "_" ++ typeSafeString t2
typeSafeString TBool = "bool"
typeSafeString TUndefined = "<undefined>"
typeSafeString (TArray t) = "Array_" ++ typeSafeString t
typeSafeString (TRaw d) = d
typeSafeString (TPointer t) = "Ptr_" ++ typeSafeString t
typeSafeString (TStruct name _) = name
typeSafeString (TClass name _ _ _) = name
typeSafeString (TData name _ _ _) = name

fromNelType :: NelType -> Type
fromNelType TypeInt = TInt
fromNelType TypeFloat = TFloat
fromNelType TypeDouble = TDouble
fromNelType TypeString = TString
fromNelType TypeBool = TBool
fromNelType (TypeArray t) = TArray (fromNelType t)
fromNelType (TypeData a b c d) = TData a b c d
fromNelType (TypeTuple ts) = TStruct name decls
	where
		types = map fromNelType ts
		decls = zip [ "t_" ++ show i | i <- [0..]] types
		name = "Tuple" ++ show (length ts) ++ "_" ++ addDelimiter "_" (map typeSafeString types)

fromNelVarDeclaration :: NelVarDeclaration -> VarDeclaration
fromNelVarDeclaration (n, t) = (n, fromNelType t)

fromNelVarDeclarations :: [NelVarDeclaration] -> [VarDeclaration]
fromNelVarDeclarations = map fromNelVarDeclaration

escapeString :: String -> String
escapeString str = concatMap escapeChar str
	where
		escapeChar '"' = "\\\""
		escapeChar '\n' = "\\n"
		escapeChar '\t' = "\\t"
		escapeChar x = [x]
