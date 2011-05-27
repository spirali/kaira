
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
	placeVar place = (nameFromId "place" (placeId place), (caPlace . fromNelType . placeType) place)
	attributes = map placeVar (unitPlaces unit)
	constr = constructor {
		functionName = className,
		parameters = [ ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
		initCall = Just $ ECall "CaUnit" [ EVar "def", EVar "path" ]
	}

placeAttr :: Place -> Expression -> Expression
placeAttr place expr = EMemberPtr (nameFromId "place" (placeId place)) expr

initPlace :: Project -> Expression -> Place -> [Instruction]
initPlace project expr place = initConsts ++ callInitFn
	where
	callInitFn | hasInitCode place =
		[ icall (nameFromId "place_init" (placeId place)) [  EVar "ctx", expr ] ]
				| otherwise = []
	initConsts = [ icall ".add" [ expr, toExpressionWithoutVars project (placeType place) nelExpr ] | nelExpr <- placeInitExprs place ]

defineContext :: Expression -> Expression -> Instruction
defineContext thread unit = idefine "ctx" caContext (ECall "CaContext" [ thread, unit ])


initCaUnit :: Project -> Unit -> Function
initCaUnit project unit = function {
	functionName = nameFromId "init_unit" (unitId unit),
	returnType = caUnit,
	parameters = [ ("thread", caThread, ParamNormal), ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
	instructions = [ idefine "unit" (TPointer $ unitType unit) $
		ENew (ECall (nameFromId "Unit" (unitId unit)) [ EVar "def", EVar "path" ]),
		defineContext (EVar "thread") (EVar "unit")
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
		intern = countedMap internDecl (normalInEdges transition)
		internDecl n edge = ("__token_" ++ show n, TPointer $ caToken $ fromNelType $ placeTypeById project (edgePlaceId edge))

transitionOfEdge :: Project -> Edge -> Transition
transitionOfEdge project edge = Maybe.fromJust $ List.find hasEdge (transitions project)
	where hasEdge t = List.elem edge $ edgesIn t ++ edgesOut t

{- There is bug in expression like: (x, x) or (x+1, x) if x is not binded -}
patternCheck :: Project -> Expression -> NelType -> NelExpression -> Set.Set String -> Instruction -> Instruction
patternCheck project source t expr binded failed =
	case expr of
		ExprVar x | not (x `Set.member` binded) -> ISet (EMember x (EVar "vars")) source
		ExprTuple xs -> checkTuple xs
		_ -> IIf (ECall "!=" [ source, (toExpression project varFn t expr)]) failed INoop
	where
		varFn varName = EMember varName (EVar "vars")
		checkTupleItem i (t, expr) = patternCheck project (EMember ("t_" ++ show i) source) t expr binded failed
		checkTuple xs =
			case t of
				TypeTuple ts -> IStatement $ countedMap checkTupleItem (zip ts xs)
				t -> error $ "Tuple expected but got " ++ show t

placeInUnit :: Expression -> Place -> Expression
placeInUnit expr place = EMemberPtr (nameFromId "place" (placeId place)) expr

matchTest :: Project -> Int -> [Edge] -> Set.Set String -> Instruction -> Instruction
matchTest project level [] binded instruction = instruction
matchTest project level edges@(edge@(Edge placeId (EdgeExpression expr) _):rest) binded instruction =
	IStatement $ [ idefine varName (TPointer $ caToken elementType) (ECall ".begin" [ placeExpr ]),
		IDo (ECall "!=" [ EVar varName, ECall ".begin" [ placeExpr ]]) (IStatement $ body ++ [ failedI ] )
	]
	where
		varName = "c_" ++ show level
		newBinded = Set.union binded $ freeVariables expr
		failedI = IStatement [ ISet (EVar varName) (EMemberPtr "next" (EVar varName)), IContinue ]
		body = [ tokenCheck, patternCheck project (EMemberPtr "element" (EVar varName)) (placeType place) expr binded failedI,
			matchTest project (level + 1) rest newBinded instruction ]
		processed = zip [0..] $ (List.\\) (edgesIn (transitionOfEdge project edge)) edges
		fromSamePlace = filter (\(i, (Edge pId _ _)) -> pId == placeId) processed
		placeExpr = placeAttr place (EVar "u")
		place = placeById project placeId
		elementType = fromNelType $ placeType place
		tokenCheck | fromSamePlace == [] = INoop
			   | otherwise = IIf (callIfMore "||"
						[ ECall "==" [EVar ("c_" ++ show i), EVar varName] | (i, e) <- fromSamePlace ])
					failedI INoop

matchTest project level (edge@(Edge placeId (EdgePacking name (Just limit)) _):rest) binded instruction =
	IIf (ECall ">=" [ ECall ".size" [ placeExpr ], ECall "+" [ limitExpr, EInt (length fromSamePlace - 1)]]) body INoop
	where
		place = placeById project placeId
		placeExpr = placeAttr place (EVar "u")
		limitExpr = toExpression project varFn TypeInt limit
		varFn varName = EMember varName (EVar "vars")
		body = matchTest project (level + 1) rest binded instruction
		fromSamePlace = filter (\(Edge pId _ _) -> pId == placeId) (edgesIn (transitionOfEdge project edge))


matchTest project level edges@(edge@(Edge placeId (EdgePacking name (Nothing)) _):rest) binded instruction =
	error "Input packing edge without limit"

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
			TStruct name _ -> name
toExpression project fn t x = error $ "Derivation failed: " ++ show x ++ "/" ++ show t

varNotAllowed x = error "Variable not allowed"

toExpressionWithoutVars :: Project -> NelType -> NelExpression -> Expression
toExpressionWithoutVars project = toExpression project varNotAllowed

pathItemExpression :: PathItem -> NelExpression
pathItemExpression (PathSingleton x) = x
pathItemExpression _ = error $ "Path is multipath"

pathToExpression :: Project -> (String -> Expression) -> Path -> Expression
pathToExpression project fn (AbsPath es) = ECall "caPathAbs" $ map (toExpression project fn TypeInt . pathItemExpression) es
pathToExpression project fn (RelPath n es) = ECall "caPathRel" $ (EInt n) : map (toExpression project fn TypeInt . pathItemExpression) es

absPathToExpression :: Project -> (String -> Expression) -> Expression -> Path -> Expression
absPathToExpression project varfn abspath (RelPath n es) =
	ECall ".apply" $ abspath : (EInt n) : (EInt $ length es) : map (toExpression project varfn TypeInt . pathItemExpression) es
absPathToExpression project varfn abspath (AbsPath es) =
	ECall "CaPath" $ EInt (length es) : map (toExpression project varfn TypeInt . pathItemExpression) es

multiPathCode :: Project -> (String -> Expression) -> Path -> Expression
multiPathCode project varfn (AbsPath es) =
	ECall "CaMultiPath" $ (EString $ map definition es) : (map (toExpression project varfn TypeInt) $ concatMap args es)
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
asStringCall (TypeTuple (t:types)) expr = ECall "+" $ [ ECall "std::string" [ EString "(" ], asStringCall t (EMember "t_0" expr)]
			++ concat (countedMap code types) ++ [ EString ")" ]
	where code i t = [ EString ",", asStringCall t (EMember ("t_" ++ show (i + 1)) expr) ]
asStringCall (TypeData name _ _ functions) expr | hasKey "getstring" functions = ECall (name ++ "_getstring") [expr]
asStringCall (TypeData name _ _ _) expr = ECall "std::string" [ EString name ]

reportFunction :: Unit -> Function
reportFunction unit = function {
	functionName = "report_places",
	parameters = [ ("out", TRaw "CaOutput", ParamRef) ],
	instructions = map reportPlace (unitPlaces unit)
	}
	where
		beginCall place = ECall ".begin" [ placeAttr place (EVar "this") ]
		reportPlace place = IStatement [
			icall ".child" [ EVar "out", EString "place" ],
			icall ".set" [ EVar "out", EString "id", EInt (placeId place) ],
			idefine "t" (TPointer $ caToken $ fromNelType (placeType place)) (beginCall place),
			IIf (ECall "!=" [ ENull, (EVar "t") ]) (IStatement [
				IDo (ECall "!=" [ EVar "t", beginCall place ]) $ IStatement [
					icall ".child" [ EVar "out", EString "token" ],
					icall ".set" [ EVar "out", EString "value", asStringCall (placeType place) (EMemberPtr "element" (EVar "t")) ],
					icall ".back" [ EVar "out" ],
					ISet (EVar "t") (EMemberPtr "next" (EVar "t"))
				]
			]) INoop,
			icall ".back" [ EVar "out"]]

normalInEdges transition = filter isNormalEdge (edgesIn transition)
packingInEdges transition = filter isPackingEdge (edgesIn transition)

fireFn :: Project -> Transition -> Function
fireFn project transition = function {
	functionName = fireFnName transition,
	parameters = [
		("thread", caThread, ParamNormal),
		("unit" , caUnit, ParamNormal),
		("vars", TPointer $ varStruct project transition, ParamNormal)
	],
	instructions = [
		defineContext (EVar "thread") (EVar "unit"),
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		IStatement $ countedMap removeToken (normalInEdges transition) ++ map setPacking (packingInEdges transition),
		callWorker ] ++ map processOutput (edgesOut transition)
} where
	unitT = unitType (unit project transition)
	ePlace edge var = EMemberPtr (nameFromId "place" (edgePlaceId edge)) (EVar var)
	removeToken i edge = icall ".remove" [ ePlace edge "u", EMemberPtr ("__token_" ++ show i) (EVar "vars") ]
	setPacking edge = case edgeInscription edge of
		(EdgePacking name _) -> ISet (EMemberPtr name (EVar "vars")) $ ECall ".to_vector_and_clear" [ ePlace edge "u" ]
	callWorker | hasCode transition = IStatement [
		icall "->unlock" [ EVar "u" ],
		icall (nameFromId "worker" $ transitionId transition) [ EVar "ctx", EDeref $ EVar "vars" ] ]
			| otherwise = icall "->unlock" [ EVar "u" ]
	processOutput edge = IStatement [
		idefine "target" (TPointer $ unitType u) (ECast (ECall "->get_unit" [ (EVar "thread"),
				absPathToExpression project varFn (EMemberPtr "path" (EVar "u")) (edgeTarget edge),
				EInt (unitId u)]) (TPointer $ unitType u)),
		icall "->lock" [ EVar "target" ],
		putInstruction edge,
		icall "->unlock" [ EVar "target" ] ] where u = edgeUnit project edge
	putInstruction edge = case edgeInscription edge of
		EdgeExpression expr -> icall ".add" [ ePlace edge "target",
			toExpression project varFn (placeTypeById project (edgePlaceId edge)) expr ]
		EdgePacking str Nothing -> icall ".add_all" [ ePlace edge "target", varFn str ]
		EdgePacking _ (Just _) -> error "Packing expression with limit on output edge"
	varFn varName = EMemberPtr varName (EVar "vars")

transitionFn :: Project -> Transition -> Function
transitionFn project transition = function {
	functionName = transitionFnName transition,
	returnType = TInt,
	parameters = [
		("thread", caThread, ParamNormal),
		("unit", caUnit, ParamNormal),
		("firefn", TPointer (TRaw "CaFireFn"), ParamNormal) ],
	instructions = [
		defineContext (EVar "thread") (EVar "unit"),
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		checkPlaces,
		idefineEmpty "vars" (varStruct project transition),
		matchTest project 0 (edgesIn transition) Set.empty matched,
		IReturn (EInt 0)
	]
} where
	unitT = unitType (unit project transition)
	matched = IStatement $ map tokenSet [0 .. length (normalInEdges transition) - 1] ++
		[ icall "firefn" [ EVar "thread", EVar "u", EAddr (EVar "vars") ], IReturn (EInt 1) ]
	tokenSet i = ISet (EMember ("__token_" ++ show i) (EVar "vars")) (EVar $ "c_" ++ show i)
	checkPlaces = IStatement $ map checkPlace (places project)
	checkPlace place = let n = needTokens' (placeId place) (edgesIn transition) in
		if n == 0 then
			INoop
		else
			IIf (ECall "<" [ ECall ".size" [ placeAttr place (EVar "u")], EInt n ]) (IReturn $ EInt 0) INoop
	needTokens placeId edge | edgePlaceId edge == placeId = case edgeInscription edge of
		EdgeExpression _ -> 1
		EdgePacking _ _ -> 0
	needTokens placeId edge = 0
	needTokens' placeId edges = sum $ map (needTokens placeId) edges

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
			("place", caPlace (fromNelType (placeType place)), ParamRef)],
		extraCode = Maybe.fromJust $ placeInitCode place,
		returnType = TVoid,
		functionSource = Just ("*" ++ show (placeId place) ++ "/init_function", 1)
}

makeUserFunction :: UserFunction -> Function
makeUserFunction ufunction = function {
		functionName = ufunctionName ufunction,
		returnType = fromNelType (ufunctionReturnType ufunction),
		parameters = if ufunctionWithContext ufunction then ("ctx", caContext, ParamNormal):params else params,
		extraCode = ufunctionCode ufunction,
		functionSource = Just ("*" ++ show (ufunctionId ufunction) ++ "/user_function", 1)
	} where params = [ (name, t, ParamConst) | (name, t) <- fromNelVarDeclarations (ufunctionParameters ufunction) ]

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
		functions = userFs ++ placeInitFs ++ workerFs ++ fireFs ++ transitionFs ++ unitFs ++ [mainF]
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
		userFs = map makeUserFunction (userFunctions project)
		prologue = "#include <stdio.h>\n#include <stdlib.h>\n#include <vector>\n#include <cailie.h>\n\n#include \"head.cpp\"\n\n"
		globals = [ (parameterGlobalName $ parameterName p, fromNelType $ parameterType p) | p <- projectParameters project ]

test =
	readFile "../vtwo/vtwo.xml" >>= return . (createProgram "testxyz") .
		projectFromXml >>= writeFile "../vtwo/vtwo.cpp"

test2 = do
	file <- readFile "../vtwo/vtwo.xml"
	return $ map (length . unitPlaces) $ projectUnits $ projectFromXml file
