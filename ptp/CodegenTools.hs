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

module CodegenTools where

import Declarations
import CodegenTypes

import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map


-- | Shortcut for instruction calling fuction
icall name params = IExpr $ ECall name params

makeStatement :: [VarDeclaration] -> [Instruction] -> Instruction
makeStatement decls = IStatement decls

makeStatement' :: [VarDeclaration] -> [(String, Type, Expression)] -> [Instruction] -> Instruction
makeStatement' decls declsWithInit instructions =
	makeStatement decls $ [ idefine name t init | (name, t, init) <- declsWithInit ] ++ instructions

idefine :: String -> Type -> Expression -> Instruction
idefine name t expr = IDefine name t (Just expr)

idefineEmpty :: String -> Type -> Instruction
idefineEmpty name t = IDefine name t Nothing

declarationsJoin :: Declarations -> Declarations -> Declarations
declarationsJoin da db = Declarations {
	varDeclarations = varDeclarations da ++ varDeclarations db,
	funDeclarations = funDeclarations da ++ funDeclarations db
}

returnTypeOfFunction :: Declarations -> String -> Type
returnTypeOfFunction declarations name =
	case List.find (\(n, r, p) -> n == name) (funDeclarations declarations) of
		Just (n, r, p) -> r
		Nothing -> TUndefined

makeDeclarations :: [VarDeclaration] -> [FunDeclaration] -> Declarations
makeDeclarations vars funs = Declarations {
	varDeclarations = vars,
	funDeclarations = funs
}

declarationsFromVarList :: [VarDeclaration] -> Declarations
declarationsFromVarList vars = makeDeclarations vars []

emptyDeclarations = makeDeclarations [] []

varType :: Declarations -> String -> Type
varType decls vname = case List.lookup vname (varDeclarations decls) of
	Just x -> x
	Nothing -> TUndefined

exprType :: Declarations -> Expression -> Type
exprType declarations (EInt _) = TInt
exprType declarations (EString _) = TString
exprType declarations (EVar x) = varType declarations x
exprType declarations (ETuple exprs) = TTuple $ map (exprType declarations) exprs
exprType declarations (EAt (EInt i) place) =
	case exprType declarations place of
		TArray t -> t
		TPointer (TTuple elements) -> elements !! i
		TTuple elements -> elements !! i
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt (int): " ++ show place

exprType declarations (EAt (EString s) place) =
	case exprType declarations place of
		TStruct _ ds -> case List.lookup s ds of
						Nothing -> error "exprType, item of struct not found"
						Just y -> y
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt (string): " ++ show x

exprType declarations (EAt index place) =
	case exprType declarations place of
		TArray t -> t
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt: " ++ show x ++ "/" ++ show index
exprType declarations (EAddr t) = TPointer $ exprType declarations t
exprType declarations (ECall name _) = returnTypeOfFunction declarations name
exprType declarations x = error $ "exprType unimplemented: " ++ show x

mapVars :: (String -> Expression) -> Expression -> Expression
mapVars fn (EVar x) = fn x
mapVars fn (ETuple exprs) = ETuple [ mapVars fn e | e <- exprs ]
mapVars fn (ECall name exprs) = ECall name [ mapVars fn e | e <- exprs ]
mapVars fn e = e

isUndefined :: Type -> Bool
isUndefined TUndefined = True
isUndefined (TTuple types) = any isUndefined types
isUndefined (TArray t) = isUndefined t
isUndefined (TStruct _ decls) = any (isUndefined . snd) decls
isUndefined _ = False

isDefined :: Type -> Bool
isDefined = not . isUndefined

freeVarsFromStruct :: Expression -> Expression -> Expression
freeVarsFromStruct structExpr = mapVars (\varName -> EAt (EString varName) structExpr)

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
instructionExprs (IStatement decls instrs) = concatMap instructionExprs instrs
instructionExprs (IForeach _ _ expr instrs) = concatMap instructionExprs instrs
instructionExprs INoop = []
instructionExprs _ = []

mapExprs :: (Expression -> Expression) -> Instruction -> Instruction
mapExprs fn (IExpr e) = IExpr $ fn e
mapExprs fn (ISet e1 e2) = ISet (fn e1) (fn e2)
mapExprs fn (IIf e i1 i2) = IIf (fn e) (mapExprs fn i1) (mapExprs fn i2)
mapExprs fn (IStatement decls is) = IStatement decls $ map (mapExprs fn) is
mapExprs fn (IDefine name t (Just expr)) = IDefine name t $ Just (fn expr)
mapExprs fn (IForeach a b e is) = IForeach a b (fn e) $ map (mapExprs fn) is
mapExprs fn x = x

mapExprs' :: (Declarations -> Expression -> Expression) -> Declarations -> Instruction -> Instruction
mapExprs' fn decls (IExpr e) = IExpr $ fn decls e
mapExprs' fn decls (ISet e1 e2) = ISet (fn decls e1) (fn decls e2)
mapExprs' fn decls (IIf e i1 i2) = IIf (fn decls e) (mapExprs' fn decls i1) (mapExprs' fn decls i2)
mapExprs' fn decls (IStatement d is) =
	IStatement d $ map (mapExprs' fn newDecls) is
	where newDecls = statementDeclarations d is `declarationsJoin` decls
mapExprs' fn decls (IDefine name t (Just expr)) = IDefine name t $ Just (fn decls expr)
mapExprs' fn decls (IForeach a b e is) = IForeach a b (fn decls e) $ map (mapExprs' fn decls) is
mapExprs' fn decls x = x

-- |Returns expression that computes size of memory footprint of result of expr
exprMemSize :: Type -> Expression -> Expression
exprMemSize TInt expr = ECall "sizeof" [EVar "int"]
exprMemSize TString expr = ECall "+" [ ECall "sizeof" [ EType "size_t" ], ECall ".size" [ expr ] ]
exprMemSize (TTuple types) expr = ECall "+" [ exprMemSize t (EAt (EInt x) expr) | (x, t) <- zip [0..] types ]
exprMemSize (TData _ rawType TransportDirect _) expr = ECall "sizeof" [ EType rawType ]
exprMemSize (TData name _ TransportCustom _) expr = ECall (name ++ "_getsize") [ expr ]
exprMemSize t expr = error $ "exprMemSize: " ++ show t

canBeDirectlyPacked :: Type -> Bool
canBeDirectlyPacked TInt = True
canBeDirectlyPacked (TTuple types) = all canBeDirectlyPacked types
canBeDirectlyPacked (TData _ _ TransportDirect _) = True
canBeDirectlyPacked _ = False

statementVarList :: [VarDeclaration] -> [Instruction] -> [VarDeclaration]
statementVarList decls instructions =
	decls ++ foldl idefs [] instructions
	where
		idefs lst (IDefine name t _) = (name, t) : lst
		idefs lst _ = lst

statementDeclarations :: [VarDeclaration] -> [Instruction] -> Declarations
statementDeclarations decls instructions =
	declarationsFromVarList (statementVarList decls instructions)

paramFromVar :: ParamType -> [VarDeclaration] -> [ParamDeclaration]
paramFromVar pt decls = [ (name, t, pt) | (name, t) <- decls ]

varFromParam :: [ParamDeclaration] -> [VarDeclaration]
varFromParam decls = [ (name, t) | (name, t, _) <- decls ]

fromNelType :: NelType -> Type
fromNelType TypeInt = TInt
fromNelType TypeString = TString
fromNelType TypeBool = TBool
fromNelType (TypeArray t) = TArray (fromNelType t)
fromNelType (TypeData a b c d) = TData a b c d
fromNelType (TypeTuple ts) = TTuple $ map fromNelType ts

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
