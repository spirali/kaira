
module Utils where

import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Declarations

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

freeVariables :: Expression -> VarSet
freeVariables (ExprVar x) = Set.singleton x
freeVariables (ExprInt _) = Set.empty
freeVariables (ExprTuple exprs) = Set.unions $ map freeVariables exprs
freeVariables (ExprCall _ exprs) = Set.unions $ map freeVariables exprs

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

freeVariablesList :: Expression -> [String]
freeVariablesList = Set.toList . freeVariables

callIfMore :: String -> [Expression] -> Expression
callIfMore _ [] = error "callIfMore called with empty list"
callIfMore _ [x] = x
callIfMore name xs = ExprCall name xs

addDelimiter :: String -> [String] -> String
addDelimiter d xs = List.concat $ List.intersperse d xs

countedMap :: (Int -> a -> b) -> [a] -> [b]
countedMap f list  = [ f x y | (x, y) <- zip [0..] list ]

indexOf :: (a -> Bool) -> [a] -> Maybe Int
indexOf fn list = 
	indexOf' fn list 0
	where 
		indexOf' _ [] _ = Nothing
		indexOf' fn (a:as) n 
			| fn a = Just n 
			| otherwise = indexOf' fn as (n+1)


divide :: (Ord b) => (a -> b) -> [a] -> [[a]]
divide fn list = 
	map snd (Map.toList (Map.unionsWith (++) [ Map.singleton (fn x) [x] | x <- list ]))

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
mapExprs fn (IForeach a b e is) = IForeach a b (fn e) $ map (mapExprs fn) is
mapExprs fn x = x

mapExprs' :: (Declarations -> Expression -> Expression) -> Declarations -> Instruction -> Instruction
mapExprs' fn decls (IExpr e) = IExpr $ fn decls e
mapExprs' fn decls (ISet e1 e2) = ISet (fn decls e1) (fn decls e2)
mapExprs' fn decls (IIf e i1 i2) = IIf (fn decls e) (mapExprs' fn decls i1) (mapExprs' fn decls i2)
mapExprs' fn decls (IStatement d is) = 
	IStatement d $ map (mapExprs' fn ((declarationsFromVarList d) `declarationsJoin` decls)) is
mapExprs' fn decls (IForeach a b e is) = IForeach a b (fn decls e) $ map (mapExprs' fn decls) is
mapExprs' fn decls x = x

triangleDependancy :: (a -> a -> Bool) -> [a] -> [(a, [a])]
triangleDependancy fn list =
	makeTriangle list []
	where 
		makeTriangle [] _ = []
		makeTriangle (a:as) b = (a, filter (fn a) b) : (makeTriangle as (a:b))
