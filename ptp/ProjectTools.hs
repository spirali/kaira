
module ProjectTools where

import Declarations
import Utils
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

-- | Shortcut for instruction calling fuction
icall name params = IExpr $ ExprCall name params

makeStatement decls instructions = IStatement decls [] instructions
makeStatement' decls declsWithInit instructions = IStatement 
	(decls ++ [ (name, t) | (name, t, init) <- declsWithInit ]) 
	[ (name, init) | (name, t, init) <- declsWithInit ]
	instructions


-- | Returns name of variables that is directly mapped
directVariables :: Expression -> VarSet
directVariables (ExprVar x) = Set.singleton x
directVariables (ExprTuple exprs) = Set.unions $ map directVariables exprs
directVariables _ = Set.empty

-- | Return name of free variables
freeVariables :: Expression -> VarSet
freeVariables (ExprVar x) = Set.singleton x
freeVariables (ExprTuple exprs) = Set.unions $ map freeVariables exprs
freeVariables (ExprCall _ exprs) = Set.unions $ map freeVariables exprs
freeVariables _ = Set.empty

-- | Return variables that is not directed
undirectVariables :: Expression -> VarSet
undirectVariables expr = Set.difference (freeVariables expr) (directVariables expr)

-- | Same as freeVariables but returns list
freeVariablesList :: Expression -> [String]
freeVariablesList = Set.toList . freeVariables

-- | Correctly orders input edges by dependancy of variables
orderEdgesByDependancy :: [Edge] -> [Edge]
orderEdgesByDependancy edges = 
	let (normal, packing) = List.partition isNormalEdge edges in
		(orderNormalEdgesByDependancy normal) ++ packing

-- | Returns true if all free variables in expression is in set of vars
isCovered :: VarSet -> Expression -> Bool
isCovered vars expr = Set.empty == Set.difference (freeVariables expr) vars

isNormalEdge :: Edge -> Bool
isNormalEdge edge = case edgeInscription edge of
	EdgeExpression _ -> True
	_ -> False

orderNormalEdgesByDependancy :: [Edge] -> [Edge]
orderNormalEdgesByDependancy edges =
	process edges [] Set.empty
	where 
		process [] ordered vars = ordered
		process edges ordered vars = let (okEdges, notOkEdges) = List.partition (edgeIsOk vars) edges in
			case okEdges of
				[] -> error $ "Edges cannot be ordered"
				_ -> process notOkEdges (ordered ++ okEdges) (Set.unions (vars : (map (directVariables . edgeExpr) okEdges)))
		edgeIsOk vars edge = Set.empty == Set.difference (undirectVariables (edgeExpr edge)) vars
		edgeExpr edge = let EdgeExpression expr = edgeInscription edge in expr

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
exprType declarations (ExprInt _) = TInt
exprType declarations (ExprString _) = TString
exprType declarations (ExprVar x) = varType declarations x
exprType declarations (ExprTuple exprs) = TTuple $ map (exprType declarations) exprs
exprType declarations (ExprAt (ExprInt i) place) = 
	case exprType declarations place of
		TArray t -> t
		TPointer (TTuple elements) -> elements !! i
		TTuple elements -> elements !! i
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt (int): " ++ show place

exprType declarations (ExprAt (ExprString s) place) =
	case exprType declarations place of
		TStruct _ ds -> case List.lookup s ds of
						Nothing -> error $ "exprType, item of struct not found"
						Just y -> y
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt (string): " ++ show x

exprType declarations (ExprAt index place) = 
	case exprType declarations place of
		TArray t -> t
		TUndefined -> TUndefined
		x -> error $ "exprType: Unsuported type for ExprAt: " ++ show x ++ "/" ++ show index
exprType declarations (ExprAddr t) = TPointer $ exprType declarations t
exprType declarations (ExprCall name _) = returnTypeOfFunction declarations name
exprType declarations x = error $ "exprType unimplemented: " ++ show x

newVars' :: String -> [String]
newVars' prefix = map (\x -> prefix ++ "_" ++ show x) [1 .. ]

mapVars :: (String -> Expression) -> Expression -> Expression
mapVars fn (ExprVar x) = fn x
mapVars fn (ExprTuple exprs) = ExprTuple [ mapVars fn e | e <- exprs ]
mapVars fn (ExprCall name exprs) = ExprCall name [ mapVars fn e | e <- exprs ]
mapVars fn e = e

isUndefined :: Type -> Bool
isUndefined TUndefined = True
isUndefined (TTuple types) = any isUndefined types
isUndefined (TArray t) = isUndefined t
isUndefined (TStruct _ decls) = any (isUndefined . snd) decls
isUndefined _ = False

isDefined :: Type -> Bool
isDefined t = not $ isUndefined t

freeVarsFromStruct :: Expression -> Expression -> Expression
freeVarsFromStruct structExpr expr =
	mapVars (\varName -> ExprAt (ExprString varName) structExpr) expr

-- | Makes expression that call function if there is more then one expression, otherwise expression is returned
callIfMore :: String -> [Expression] -> Expression
callIfMore _ [] = error "callIfMore called with empty list"
callIfMore _ [x] = x
callIfMore name xs = ExprCall name xs

-- | Return all expressions in instruction
instructionExprs :: Instruction -> [Expression]
instructionExprs (IExpr expr) = [expr]
instructionExprs (ISet expr1 expr2) = [expr1, expr2]
instructionExprs (IIf expr i1 i2) = [ expr ] ++ instructionExprs i1 ++ instructionExprs i2
instructionExprs (IStatement decls inits instrs) = concatMap instructionExprs instrs ++ [ init | (name, init) <- inits ]
instructionExprs (IForeach _ _ expr instrs) = concatMap instructionExprs instrs
instructionExprs INoop = []
instructionExprs _ = []

mapExprs :: (Expression -> Expression) -> Instruction -> Instruction
mapExprs fn (IExpr e) = IExpr $ fn e
mapExprs fn (ISet e1 e2) = ISet (fn e1) (fn e2)
mapExprs fn (IIf e i1 i2) = IIf (fn e) (mapExprs fn i1) (mapExprs fn i2)
mapExprs fn (IStatement decls inits is) = IStatement decls [ (name, fn init) | (name, init) <- inits ] $ map (mapExprs fn) is
mapExprs fn (IForeach a b e is) = IForeach a b (fn e) $ map (mapExprs fn) is
mapExprs fn x = x

mapExprs' :: (Declarations -> Expression -> Expression) -> Declarations -> Instruction -> Instruction
mapExprs' fn decls (IExpr e) = IExpr $ fn decls e
mapExprs' fn decls (ISet e1 e2) = ISet (fn decls e1) (fn decls e2)
mapExprs' fn decls (IIf e i1 i2) = IIf (fn decls e) (mapExprs' fn decls i1) (mapExprs' fn decls i2)
mapExprs' fn decls (IStatement d inits is) = 
	IStatement d [ (name, fn newDecls init) | (name, init) <- inits ] $ map (mapExprs' fn newDecls) is
	where newDecls = (declarationsFromVarList d) `declarationsJoin` decls
mapExprs' fn decls (IForeach a b e is) = IForeach a b (fn decls e) $ map (mapExprs' fn decls) is
mapExprs' fn decls x = x

standardTypes = 
	Map.fromList [ ("Int", TInt), ("String", TString) ]

-- |Returns expression that computes size of memory footprint of result of expr
exprMemSize :: Type -> Expression -> Expression
exprMemSize TInt expr = ExprCall "sizeof" [ExprVar "int"]
exprMemSize TString expr = ExprCall "+" [ ExprCall "sizeof" [ ExprType "size_t" ], ExprCall ".size" [ expr ] ]
exprMemSize (TTuple types) expr = ExprCall "+" $ [ exprMemSize t (ExprAt (ExprInt x) expr) | (x, t) <- zip [0..] types ]
exprMemSize (TData _ rawType TransportDirect _) expr = ExprCall "sizeof" [ ExprType rawType ]
exprMemSize t expr = error $ "exprMemSize: " ++ (show t)

canBeDirectlyPacked :: Type -> Bool
canBeDirectlyPacked TInt = True
canBeDirectlyPacked (TTuple types) = all canBeDirectlyPacked types
canBeDirectlyPacked (TData _ _ TransportDirect _) = True
canBeDirectlyPacked _ = False

isTransportable :: Type -> Bool
isTransportable (TData _ _ TransportDisabled _) = False
isTransportable (TTuple types) = all isTransportable types
isTransportable _ = True
