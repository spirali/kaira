
module ProjectTools where

import Declarations
import Utils
import qualified Data.Set as Set
import qualified Data.List as List

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
