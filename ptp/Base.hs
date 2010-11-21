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

module Base where

import Declarations
import Utils
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map

-- |Build-in function in NeL
basicNelFunctions = [
	( "+", TypeInt, [TypeInt, TypeInt]),
	( "*", TypeInt, [TypeInt, TypeInt ]),
	( "-", TypeInt, [TypeInt, TypeInt ]),
	( ">", TypeBool, [TypeInt, TypeInt ]),
	( "<", TypeBool, [TypeInt, TypeInt ]),
	( ">=", TypeBool, [TypeInt, TypeInt ]),
	( "<=", TypeBool, [TypeInt, TypeInt ]),
	( "==", TypeBool, [TypeInt, TypeInt ]),
	( "!=", TypeBool, [TypeInt, TypeInt ]),
	( "&&", TypeBool, [TypeBool, TypeBool ]),
	( "||", TypeBool, [TypeBool, TypeBool ]),
	( "iid", TypeInt, [])]

isBasicNelFunction :: String -> Bool
isBasicNelFunction name =
	any (\(n, r, p) -> n == name) basicNelFunctions

nelFunctions :: Project -> [(String, NelType, [NelType])]
nelFunctions project = basicNelFunctions
	 ++ [ (ufunctionName f, ufunctionReturnType f, map snd (ufunctionParameters f)) | f <- userFunctions project ]

nelFunctionReturnType :: Project -> String -> NelType
nelFunctionReturnType project name =
	case List.find (\(n, r, p) -> n == name) (nelFunctions project) of
		Just (n, r, p) -> r
		Nothing -> error $ "Unknown function: " ++ name

nelFunctionParams :: Project -> String -> [NelType]
nelFunctionParams project name =
	case List.find (\(n, r, p) -> n == name) (nelFunctions project) of
		Just (n, r, p) -> p
		Nothing -> error $ "Unknown function: " ++ name

-- | Returns name of variables that is directly mapped
directVariables :: NelExpression -> VarSet
directVariables (ExprVar x) = Set.singleton x
directVariables (ExprTuple exprs) = Set.unions $ map directVariables exprs
directVariables _ = Set.empty

-- | Return name of free variables
freeVariables :: NelExpression -> VarSet
freeVariables (ExprVar x) = Set.singleton x
freeVariables (ExprTuple exprs) = Set.unions $ map freeVariables exprs
freeVariables (ExprCall _ exprs) = Set.unions $ map freeVariables exprs
freeVariables _ = Set.empty

-- | Return variables that is not directed
undirectVariables :: NelExpression -> VarSet
undirectVariables expr = Set.difference (freeVariables expr) (directVariables expr)

-- | Same as freeVariables but returns list
freeVariablesList :: NelExpression -> [String]
freeVariablesList = Set.toList . freeVariables

-- | Correctly orders input edges by dependancy of variables
orderEdgesByDependancy :: [Edge] -> [Edge]
orderEdgesByDependancy edges =
	let (normal, packing) = List.partition isNormalEdge edges in
		orderNormalEdgesByDependancy normal ++ packing

-- | Returns true if all free variables in expression is in set of vars
isCovered :: VarSet -> NelExpression -> Bool
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
				[] -> error "Edges cannot be ordered"
				_ -> process notOkEdges (ordered ++ okEdges) (Set.unions (vars : map (directVariables . edgeExpr) okEdges))
		edgeIsOk vars edge = Set.empty == Set.difference (undirectVariables (edgeExpr edge)) vars
		edgeExpr edge = let EdgeExpression expr = edgeInscription edge in expr

newVars' :: String -> [String]
newVars' prefix = map (\x -> prefix ++ "_" ++ show x) [1 .. ]

standardTypes =
	Map.fromList [ ("Int", TypeInt), ("String", TypeString), ("Bool", TypeBool) ]

isTransportable :: NelType -> Bool
isTransportable (TypeData _ _ TransportDisabled _) = False
isTransportable (TypeTuple types) = all isTransportable types
isTransportable _ = True

unionsVariableTypes :: [Map.Map String NelType] -> Map.Map String NelType
unionsVariableTypes decls =
	Map.unionsWith unionFn decls
	where
		unionFn a b = if a == b then a else error $ "Type inference failed " ++ show a ++ "/" ++ show b

parameterTypeByName :: Project -> String -> NelType
parameterTypeByName project paramName =
	case List.find (\p -> parameterName p == paramName) (projectParameters project) of
		Just x -> parameterType x
		Nothing -> error $ "Parameter '" ++ paramName ++ "' not defined"

variableTypes :: Project -> NelExpression -> NelType -> Map.Map String NelType
variableTypes project (ExprVar string) ctype = Map.singleton string ctype
variableTypes project (ExprParam paramName) t | (parameterTypeByName project paramName) == t = Map.empty
variableTypes project (ExprInt _) (TypeInt) = Map.empty
variableTypes project (ExprString _) (TypeString) = Map.empty
variableTypes project (ExprTuple []) (TypeTuple []) = Map.empty
variableTypes project (ExprTuple exprs) (TypeTuple types)
	| length exprs == length types =
		unionsVariableTypes $ zipWith (variableTypes project) exprs types
{- THIS REALLY NEED FIX! Function call result has to be checked -}
variableTypes project (ExprCall name params) t
	| (nelFunctionReturnType project name == t) && (length params == length (nelFunctionParams project name)) =
		unionsVariableTypes $ zipWith (variableTypes project) params (nelFunctionParams project name)
variableTypes project x y = error $ "Type inference failed: " ++ show x ++ "/" ++ show y


