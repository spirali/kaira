{-
    Copyright (C) 2011 Stanislav Bohm

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

module BuilderTools where

import Declarations
import Codegen
import CodegenTypes
import CodegenTools
import Utils
import ProjectTools
import Base

tupleMember :: Int -> Expression -> Expression
tupleMember i expr = EMember ("t_" ++ show i) expr

parameterGlobalName :: String -> String
parameterGlobalName x = "__parameter_" ++ x

fromNelType :: NelType -> Type
fromNelType TypeInt = TInt
fromNelType TypeFloat = TFloat
fromNelType TypeDouble = TDouble
fromNelType TypeString = TString
fromNelType TypeBool = TBool
fromNelType (TypeArray t) = TArray (fromNelType t)
fromNelType (TypeData a b c d) = TData a b c d
fromNelType tt@(TypeTuple ts) = TClass name Nothing [ c0, c1, c2 ] decls
	where
		types = map fromNelType ts
		decls = zip [ "t_" ++ show i | i <- [0..]] types
		name = "Tuple" ++ show (length ts) ++ "_" ++ addDelimiter "_" (map typeSafeString types)
		c0 = constructor { functionName = name }
		c1 = constructor {
			functionName = name,
			parameters = [ (n, t, ParamConst) | (n, t) <- decls ],
			initCalls = [ ECall n [ EVar n ] | (n, t) <- decls ]
		}
		c2 | isDirectlyPackable tt = constructor {
				functionName = name,
				parameters = [ ("unpacker", TRaw "CaUnpacker", ParamRef) ],
				instructions = [ icall "memcpy" [ EVar "this", ECall ".unpack" [ EVar "unpacker", sizeOfType (TRaw name) ], sizeOfType (TRaw name) ]]
			}
			| otherwise = constructor {
				functionName = name,
				parameters = [ ("unpacker", TRaw "CaUnpacker", ParamRef) ],
				initCalls = [ ECall n [ unpack (EVar "unpacker") nt ] | ((n, t), nt) <- zip decls ts ]
			}

fromNelVarDeclaration :: NelVarDeclaration -> VarDeclaration
fromNelVarDeclaration (n, t) = (n, fromNelType t)

fromNelVarDeclarations :: [NelVarDeclaration] -> [VarDeclaration]
fromNelVarDeclarations = map fromNelVarDeclaration

toExpression :: Project -> (String -> Expression) -> NelType -> NelExpression -> Expression
toExpression project fn (TypeInt) (ExprInt x) = EInt x
toExpression project fn (TypeString) (ExprString x) = EString x
toExpression project fn _ (ExprVar x) = fn x
toExpression project fn _ (ExprParam x) = EVar $ parameterGlobalName x
toExpression project fn TypeInt (ExprCall "iid" []) = ECall ".iid" [ EVar "ctx" ]
toExpression project fn _ (ExprCall name exprs)
	| isBasicNelFunction name = ECall name params
	| isUserFunctionWithoutContext project name = ECall name params
	| isUserFunctionWithContext project name = ECall name (EVar "ctx":params)
		where params = [ toExpression project fn t expr | (t, expr) <- zip (nelFunctionParams project name) exprs ]
toExpression project fn t@(TypeTuple ts) (ExprTuple exprs) = ECall constructor [ toExpression project fn t expr | (t, expr) <- zip ts exprs ]
	where constructor = case fromNelType t of
			TClass name _ _ _ -> name
toExpression project fn t x = error $ "Derivation failed: " ++ show x ++ "/" ++ show t

varNotAllowed x = error "Variable not allowed"

toExpressionWithoutVars :: Project -> NelType -> NelExpression -> Expression
toExpressionWithoutVars project = toExpression project varNotAllowed

sizeOfType :: Type -> Expression
sizeOfType t = ECall "sizeof" [ EType t ]

-- |Returns expression that computes size of memory footprint of result of expr
exprMemSize :: NelType -> Expression -> Expression
exprMemSize TypeInt expr = sizeOfType TInt
exprMemSize TypeFloat expr = sizeOfType TFloat
exprMemSize TypeDouble expr = sizeOfType TDouble
exprMemSize TypeString (EString s) = ECall "+" [ sizeOfType sizeType, EInt (length s) ]
exprMemSize TypeString expr = ECall "+" [ sizeOfType sizeType, ECall ".size" [ expr ] ]
exprMemSize (TypeData _ rawType TransportDirect _) expr = ECall "sizeof" [ EType (TRaw rawType) ]
exprMemSize (TypeData name _ TransportCustom _) expr = ECall (name ++ "_getsize") [ expr ]
exprMemSize (TypeTuple types) expr = ECall "+" [ exprMemSize t (tupleMember x expr) | (x, t) <- zip [0..] types ]
exprMemSize t expr = error $ "exprMemSize: " ++ show t

isDirectlyPackable :: NelType -> Bool
isDirectlyPackable (TypeData _ _ TransportCustom _) = False
isDirectlyPackable TypeString = False
isDirectlyPackable (TypeData name _ TransportDisabled _) = error $ "Type '" ++ name ++ "' transport disabled"
isDirectlyPackable (TypeTuple ts) = all isDirectlyPackable ts
isDirectlyPackable _ = True

pack :: NelType -> Expression -> Expression -> Instruction
pack TypeString packer source = icall ".pack_string" [ packer, source ]
pack t packer source | isDirectlyPackable t = icall ".pack" [ packer, EAddr source, exprMemSize t source ]
pack (TypeTuple ts) packer source = IStatement [ pack t packer (expr i) | (t, i) <- zip ts [0..] ]
	where expr i = tupleMember i source
pack (TypeData name _ TransportCustom _) packer source = icall (name ++ "_pack") [ packer, source ]

unpack :: Expression -> NelType -> Expression
unpack packer TypeInt = ECall ".unpack_int" [ packer ]
unpack packer TypeFloat = ECall ".unpack_float" [ packer ]
unpack packer TypeDouble = ECall ".unpack_double" [ packer ]
unpack packer TypeString = ECall ".unpack_string" [ packer ]
unpack packer (TypeData name raw TransportDirect _) = EDeref $ ECast (ECall ".unpack" [ packer, sizeOfType (TRaw raw) ]) (TPointer $ TRaw raw)
unpack packer (TypeData name _ _ _) = ECall (name ++ "_unpack") [ packer ]
unpack packer t@(TypeTuple _) = ECall (typeString (fromNelType t)) [ packer ]

asStringCall :: NelType -> Expression -> Expression
asStringCall TypeString expr = expr
asStringCall TypeInt expr = ECall "ca_int_to_string" [expr]
asStringCall TypeFloat expr = ECall "ca_float_to_string" [expr]
asStringCall TypeDouble expr = ECall "ca_double_to_string" [expr]
asStringCall (TypeTuple []) expr = EString "()"
asStringCall (TypeTuple (t:types)) expr = ECall "+" $ [ ECall "std::string" [ EString "(" ], asStringCall t (tupleMember 0 expr)]
			++ concat (countedMap code types) ++ [ EString ")" ]
	where code i t = [ EString ",", asStringCall t (tupleMember (i + 1) expr) ]
asStringCall (TypeData name _ _ functions) expr
	| hasKey "getstring" functions = ECall (name ++ "_getstring") [expr]
	| otherwise = ECall "std::string" [ EString name ]
