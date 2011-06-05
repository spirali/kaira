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

escapeString :: String -> String
escapeString str = concatMap escapeChar str
	where
		escapeChar '"' = "\\\""
		escapeChar '\n' = "\\n"
		escapeChar '\t' = "\\t"
		escapeChar x = [x]
