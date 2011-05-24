
{-
    Copyright (C) 2010,2011 Stanislav Bohm

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

module Builder (
	createProgram,
	varStruct
) where

import Declarations
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified System.FilePath as FilePath
import Parser
import Project
import ProjectTools
import Base
import Codegen
import CodegenTypes
import CodegenTools
import Utils

caUnit = TPointer $ TRaw "CaUnit"
caUnitDef = TPointer $ TRaw "CaUnitDef"
caContext = TRaw "CaContext"
caPath = TRaw "CaPath"
caThread = TPointer $ TRaw "CaThread"

transitionVarType = undefined

parameterGlobalName :: String -> String
parameterGlobalName x = "__parameter_" ++ x

function = Function {
	functionName = "",
	parameters = [],
	returnType = TVoid,
	instructions = [],
	extraCode = "",
	functionSource = Nothing,
	initCall = Nothing
}

constructor = function { returnType = TRaw "" }

nameFromId :: String -> ID -> String
nameFromId name id = name ++ "_" ++ (show id)

unitType :: Unit -> Type
unitType unit = TClass className (Just "CaUnit") [ constr, reportFunction unit ] attributes
	where
	className = nameFromId "Unit" $ unitId unit
	placeVar place = (nameFromId "place" (placeId place), (TPlace . fromNelType . placeType) place)
	attributes = map placeVar (unitPlaces unit)
	constr = constructor {
		functionName = className,
		parameters = [ ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
		initCall = Just $ ECall "CaUnit" [ EVar "def", EVar "path" ]
	}

placeAttr :: Place -> Expression -> Expression
placeAttr place expr = EAt (EString $ nameFromId "place" (placeId place)) expr

initPlace :: Project -> Expression -> Place -> [Instruction]
initPlace project expr place = initConsts ++ callInitFn
	where
	callInitFn | hasInitCode place =
		[ icall (nameFromId "place_init" (placeId place)) [  EVar "ctx", expr ] ]
				| otherwise = []
	initConsts = [ icall ".add" [ expr, toExpressionWithoutVars project nelExpr ] | nelExpr <- placeInitExprs place ]

initCaUnit :: Project -> Unit -> Function
initCaUnit project unit = function {
	functionName = nameFromId "init_unit" (unitId unit),
	returnType = caUnit,
	parameters = [ ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
	instructions = [ idefine "unit" (TPointer $ unitType unit) $
		ENew (ECall (nameFromId "Unit" (unitId unit)) [ EVar "def", EVar "path" ]),
		idefine "ctx" caContext (ECall "CaContext" [])
	] ++
		initPlaces ++ [ IReturn $ EVar "unit" ]
} where
	initPlaces = concat [ initPlace project (placeInUnit (EVar "unit") place) place | place <- unitPlaces unit ]

unit :: Project -> Transition -> Unit
unit project transition =
	just "unit" $ List.find ((List.elem transition) . unitTransitions) (projectUnits project)

placeUnit :: Project -> Place -> Unit
placeUnit project place =
	just "placeUnit" $ List.find ((List.elem place) . unitPlaces) (projectUnits project)

edgeUnit :: Project -> Edge -> Unit
edgeUnit project edge = placeUnit project $ edgePlace project edge

transitionFnName :: Transition -> String
transitionFnName t = nameFromId "transition" (transitionId t)

fireFnName :: Transition -> String
fireFnName t = nameFromId "fire" (transitionId t)

initCaUnitDef :: Project -> Unit -> [Instruction]
initCaUnitDef project unit = [
	idefine varName caUnitDef $
		ENew (ECall "CaUnitDef" [ EVar $ "init_unit_" ++ show (unitId unit), EInt (length transitions)])
	] ++ countedMap registerTransition transitions ++ map registerInit (unitInitPaths unit)
	where
		transitions = unitTransitions unit
		varName = nameFromId "unitdef" (unitId unit)
		registerTransition n t = icall "->register_transition" [ EVar varName, EInt n, EInt (transitionId t), arg1 t, arg2 t, arg3 t]
		arg1 t = EVar $ "(CaEnableFn*) " ++ transitionFnName t
		arg2 t = EVar $ "(CaFireFn*) " ++ fireFnName t
		arg3 t = ECall "sizeof" [ EVar (nameFromId "Vars" (transitionId t)) ]
		registerInit p = icall "->register_init" [ EVar varName, multiPathCode project varNotAllowed p ]


varStruct :: Project -> Transition -> Type
varStruct project transition =
	TStruct (nameFromId "Vars" (transitionId transition)) $ visible ++ intern
	where
		visible = fromNelVarDeclarations $ transitionFreeVariables project transition
		intern = [ ("__token_" ++ show i, TInt) | i <- [0 .. length (normalInEdges transition) - 1] ]

transitionOfEdge :: Project -> Edge -> Transition
transitionOfEdge project edge = Maybe.fromJust $ List.find hasEdge (transitions project)
	where hasEdge t = List.elem edge $ edgesIn t ++ edgesOut t

{- There is bug in expression like: (x, x) or (x+1, x) if x is not binded -}
patternCheck :: Project -> Expression -> NelExpression -> Set.Set String -> Instruction
patternCheck project source expr binded =
	case expr of
		ExprVar x | not (x `Set.member` binded) -> ISet (EAt (EString x) (EVar "vars")) source
		ExprTuple xs -> IStatement $ countedMap checkTupleItem xs
		_ -> IIf (ECall "!=" [ source, (toExpression project varFn expr)]) IContinue INoop
	where
		varFn varName = EAt (EString varName) (EVar "vars")
		checkTupleItem i expr = patternCheck project (EAt (EInt i) source) expr binded

placeInUnit :: Expression -> Place -> Expression
placeInUnit expr place = EAt (EString $ nameFromId "place" (placeId place)) expr

matchTest :: Project -> Int -> [Edge] -> Set.Set String -> Instruction -> Instruction
matchTest project level [] binded instruction = instruction
matchTest project level edges@(edge@(Edge placeId (EdgeExpression expr) _):rest) binded instruction =
	IForeach elementType varName varCounter placeExpr body
	where
		varName = "c_" ++ show level
		varCounter = varName ++ "_i"
		newBinded = Set.union binded $ freeVariables expr
		body = [ tokenCheck, tokenSet, patternCheck project (EVar varName) expr binded,
			matchTest project (level + 1) rest newBinded instruction ]
		processed = zip [0..] $ (List.\\) (edgesIn (transitionOfEdge project edge)) edges
		fromSamePlace = filter (\(i, (Edge pId _ _)) -> pId == placeId) processed
		placeExpr = placeAttr place (EVar "u")
		place = placeById project placeId
		elementType = fromNelType $ placeType place
		tokenCheck | fromSamePlace == [] = INoop
			   | otherwise = IIf (callIfMore "||"
						[ ECall "==" [EVar ("c_" ++ show i ++ "_i"), EVar varCounter] | (i, e) <- fromSamePlace ])
					IContinue INoop
		tokenSet = ISet (EAt (EString $ "__token_" ++ show level) (EVar "vars")) (EVar varCounter)

toExpression :: Project -> (String -> Expression) -> NelExpression -> Expression
toExpression project fn (ExprInt x) = EInt x
toExpression project fn (ExprString x) = EString x
toExpression project fn (ExprVar x) = fn x
toExpression project fn (ExprParam x) = EVar $ parameterGlobalName x
toExpression project fn (ExprCall "iid" []) = ECall ".iid" [ EVar "ctx" ]
toExpression project fn (ExprCall name exprs)
	| isBasicNelFunction name = ECall name [ toExpression project fn expr | expr <- exprs ]
	| isUserFunctionWithoutContext project name = ECall name [ toExpression project fn expr | expr <- exprs ]
	| isUserFunctionWithContext project name = ECall name (EVar "ctx":[ toExpression project fn expr | expr <- exprs ])
toExpression project fn (ExprTuple exprs) = ETuple [ toExpression project fn expr | expr <- exprs ]
toExpression project fn x = error $ "Input expression contains: " ++ show x

varNotAllowed x = error "Variable not allowed"

toExpressionWithoutVars :: Project -> NelExpression -> Expression
toExpressionWithoutVars project expr = toExpression project varNotAllowed expr

pathItemExpression :: PathItem -> NelExpression
pathItemExpression (PathSingleton x) = x
pathItemExpression _ = error $ "Path is multipath"

pathToExpression :: Project -> (String -> Expression) -> Path -> Expression
pathToExpression project fn (AbsPath es) = ECall "caPathAbs" $ map (toExpression project fn . pathItemExpression) es
pathToExpression project fn (RelPath n es) = ECall "caPathRel" $ (EInt n) : map (toExpression project fn . pathItemExpression) es

absPathToExpression :: Project -> (String -> Expression) -> Expression -> Path -> Expression
absPathToExpression project varfn abspath (RelPath n es) =
	ECall ".apply" $ abspath : (EInt n) : (EInt $ length es) : map (toExpression project varfn . pathItemExpression) es
absPathToExpression project varfn abspath (AbsPath es) =
	ECall "CaPath" $ EInt (length es) : map (toExpression project varfn . pathItemExpression) es

multiPathCode :: Project -> (String -> Expression) -> Path -> Expression
multiPathCode project varfn (AbsPath es) =
	ECall "CaMultiPath" $ (EString $ map definition es) : (map (toExpression project varfn) $ concatMap args es)
	where
		definition (PathSingleton _) = 's'
		definition (PathRange _ _) = 'r'
		args (PathSingleton x) = [x]
		args (PathRange x y) = [x, y]

multiPathCode project varfn _ = error $ "multiPathCode: relative path"

asStringCall :: NelType -> Expression -> Expression
asStringCall TypeString expr = expr
asStringCall TypeInt expr = ECall "ca_int_to_string" [expr]
asStringCall TypeFloat expr = ECall "ca_float_to_string" [expr]
asStringCall TypeDouble expr = ECall "ca_double_to_string" [expr]
asStringCall (TypeTuple []) expr = EString "()"
asStringCall (TypeTuple (t:types)) expr = ECall "+" $ [ ECall "std::string" [ EString "(" ], asStringCall t (EAt (EInt 0) expr)]
			++ concat (countedMap code types) ++ [ EString ")" ]
	where code i t = [ EString ",", asStringCall t (EAt (EInt (i + 1)) expr) ]
asStringCall (TypeData name _ _ functions) expr | hasKey "getstring" functions = ECall (name ++ "_getstring") [expr]
asStringCall (TypeData name _ _ _) expr = ECall "std::string" [ EString name ]

reportFunction :: Unit -> Function
reportFunction unit = function {
	functionName = "report_places",
	parameters = [ ("out", TRaw "CaOutput", ParamRef) ],
	instructions = concatMap reportPlace (unitPlaces unit)
	}
	where
		reportPlace place = [
			icall ".child" [ EVar "out", EString "place" ],
			icall ".set" [ EVar "out", EString "id", EInt (placeId place) ],
			IForeach (fromNelType $ placeType place) "x" "x_c" (placeAttr place (EVar "this")) [
				icall ".child" [ EVar "out", EString "token" ],
				icall ".set" [ EVar "out", EString "value", asStringCall (placeType place) (EVar "x") ],
				icall ".back" [ EVar "out" ]
			],
			icall ".back" [ EVar "out"]]

normalInEdges transition = filter isNormalEdge (edgesIn transition)

fireFn :: Project -> Transition -> Function
fireFn project transition = function {
	functionName = fireFnName transition,
	parameters = [
		("thread", caThread, ParamNormal),
		("unit" , caUnit, ParamNormal),
		("vars", TPointer $ varStruct project transition, ParamNormal)
	],
	instructions = [
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		IStatement $ countedMap removeToken (normalInEdges transition),
		callWorker ] ++ map processOutput (edgesOut transition)
} where
	unitT = unitType (unit project transition)
	removeToken i edge = icall ".remove_at" [ EAt (EString $ nameFromId "place" (edgePlaceId edge)) (EVar "u"),
		 EAt (EString $ "__token_" ++ show i) (EVar "vars") ]
	callWorker | hasCode transition = IStatement [
		icall "->unlock" [ EVar "u" ],
		idefine "ctx" caContext (ECall "CaContext" []),
		icall (nameFromId "worker" $ transitionId transition) [ EVar "ctx", EDeref $ EVar "vars" ] ]
			| otherwise = icall "->unlock" [ EVar "u" ]
	processOutput edge = IStatement [
		idefine "target" (TPointer $ unitType u) (ECast (ECall "->get_unit" [ (EVar "thread"),
				absPathToExpression project varFn (EAt (EString "path") (EVar "u")) (edgeTarget edge),
				EInt (unitId u)]) (TPointer $ unitType u)),
		icall "->lock" [ EVar "target" ],
		putInstruction edge,
		icall "->unlock" [ EVar "target" ] ] where u = edgeUnit project edge
	putInstruction edge = case edgeInscription edge of
		EdgeExpression expr -> icall ".add" [ EAt (EString (nameFromId "place" (edgePlaceId edge))) (EVar "target"),
			toExpression project varFn expr ]

	varFn varName = EAt (EString varName) (EVar "vars")



transitionFn :: Project -> Transition -> Function
transitionFn project transition = function {
	functionName = transitionFnName transition,
	returnType = TInt,
	parameters = [
		("thread", caThread, ParamNormal),
		("unit", caUnit, ParamNormal),
		("firefn", TPointer (TRaw "CaFireFn"), ParamNormal) ],
	instructions = [
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		idefineEmpty "vars" (varStruct project transition),
		matchTest project 0 (edgesIn transition) Set.empty matched,
		IReturn (EInt 0)
	]
} where
	unitT = unitType (unit project transition)
	matched = IStatement $ map tokenSet [0 .. length (normalInEdges transition) - 1] ++
		[ icall "firefn" [ EVar "thread", EVar "u", EAddr (EVar "vars") ], IReturn (EInt 1) ]
	tokenSet i = ISet (EAt (EString $ "__token_" ++ show i) (EVar "vars")) (EVar $ "c_" ++ show i ++ "_i")

workerFunction :: Project -> Transition -> Function
workerFunction project transition = function {
	functionName = nameFromId "worker" (transitionId transition),
	parameters = [ ("ctx", caContext, ParamRef), ("var", varStruct project transition, ParamRef) ],
	extraCode = Maybe.fromJust $ transitionCode transition,
	functionSource = Just ("*" ++ show (transitionId transition) ++ "/function", 1)
}

placeInitFunction :: Place -> Function
placeInitFunction place = function {
		functionName = nameFromId "place_init" $ placeId place,
		parameters = [ ("ctx", caContext, ParamRef),
			("place", TPlace (fromNelType (placeType place)), ParamRef)],
		extraCode = Maybe.fromJust $ placeInitCode place,
		returnType = TVoid,
		functionSource = Just ("*" ++ show (placeId place) ++ "/init_function", 1)
}

mainFunction :: Project -> Function
mainFunction project = function {
	functionName = "main",
	parameters = [ ("argc", TRaw "int", ParamNormal), ("argv", (TPointer . TPointer . TRaw) "char", ParamNormal) ],
	instructions = parseArgs ++ initUnits ++ startCailie,
	returnType = TInt
} where
	startCailie = [ defsArray, icall "ca_main" [ EInt (length units), EVar "defs" ] ]
	parameters = projectParameters project
	units = projectUnits project
	initUnits = concatMap (initCaUnitDef project) units
	defsArray =	IInline $ "CaUnitDef *defs[] = {" ++ addDelimiter "," [ nameFromId "unitdef" i | i <- [ 0 .. length units - 1 ] ] ++ "};"
	parseArgs = [
		IInline $ "const char *p_names[] = {" ++ addDelimiter "," [ "\"" ++ parameterName p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "const char *p_descs[] = {" ++ addDelimiter "," [ "\"" ++ parameterDescription p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "int *p_data[] = {" ++ addDelimiter "," [ "&" ++ parameterGlobalName (parameterName p) | p <- parameters ] ++ "};",
		icall "ca_project_description" [ EString (projectDescription project) ],
		icall "ca_parse_args" [ EVar "argc", EVar "argv", EInt (length parameters), EVar "p_names", EVar "p_data", EVar "p_descs" ]
	 ]

createProgram :: String -> Project -> String
createProgram filename project =
	emitProgram (FilePath.takeFileName filename) prologue types globals functions
	where
		functions = placeInitFs ++ workerFs ++ fireFs ++ transitionFs ++ unitFs ++ [mainF]
		mainF = mainFunction project
		unitFs = map (initCaUnit project) units
		transitionFs = map (transitionFn project) $ transitions project
		fireFs = map (fireFn project) $ transitions project
		workerFs = map (workerFunction project) $ filter hasCode $ transitions project
		placeInitFs = map placeInitFunction $ filter hasInitCode $ places project
		types = Set.fromList $ varStructs ++ unitTypes
		varStructs = map (varStruct project) $ transitions project
		unitTypes = map unitType units
		units = projectUnits project
		prologue = "#include <stdio.h>\n#include <stdlib.h>\n#include <vector>\n#include <cailie.h>\n\n#include \"head.cpp\"\n\n"
		globals = [ (parameterGlobalName $ parameterName p, fromNelType $ parameterType p) | p <- projectParameters project ]

test =
	readFile "../vtwo/vtwo.xml" >>= return . (createProgram "testxyz") .
		projectFromXml >>= writeFile "../vtwo/vtwo.cpp"

test2 = do
	file <- readFile "../vtwo/vtwo.xml"
	return $ map (length . unitPlaces) $ projectUnits $ projectFromXml file
