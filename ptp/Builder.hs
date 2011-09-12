
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
import BuilderTools

caUnit = TPointer $ TRaw "CaUnit"
caNetworkDef = TPointer $ TRaw "CaNetworkDef"
caNetwork = TPointer $ TRaw "CaNetwork"
caUnitDef = TPointer $ TRaw "CaUnitDef"
caContext = TRaw "CaContext"
caPath = TRaw "CaPath"
caMultiPath = TRaw "CaMultiPath"
caMultiPathIter = TRaw "CaMultiPath::Iterator"
caThread = TPointer $ TRaw "CaThread"

nameFromId :: String -> ID -> String
nameFromId name id = name ++ "_" ++ show id

nameFromId2 :: String -> ID -> ID -> String
nameFromId2 name id1 id2 = name ++ "_" ++ show id1 ++ "_" ++ show id2

unitType :: Unit -> Type
unitType unit = TClass className (Just "CaUnit") [ constr, reportFunction unit, receiveFunction unit ] attributes
	where
	className = nameFromId2 "Unit" ((networkId . unitNetwork) unit) (unitId unit)
	placeVar place = (nameFromId "place" (placeId place), (caPlace . fromNelType . placeType) place)
	attributes = map placeVar (unitPlaces unit)
	constr = constructor {
		functionName = className,
		parameters = [ ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
		initCalls = [ ECall "CaUnit" [ EVar "def", EVar "path" ] ]
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

initCaUnitName :: Unit -> String
initCaUnitName unit = nameFromId2 "init_unit" (networkId (unitNetwork unit)) (unitId unit)

initCaUnit :: Project -> Unit -> Function
initCaUnit project unit = function {
	functionName = initCaUnitName unit,
	returnType = caUnit,
	parameters = [ ("thread", caThread, ParamNormal), ("def", caUnitDef, ParamNormal), ("path", caPath, ParamConst) ],
	instructions = [ idefine "unit" (TPointer $ unitType unit) $
		ENew (ECall unitName [ EVar "def", EVar "path" ]),
		defineContext (EVar "thread") (EVar "unit")
	] ++
		initPlaces ++ [ IReturn $ EVar "unit" ]
} where
	initPlaces = concat [ initPlace project (placeInUnit (EVar "unit") place) place | place <- unitPlaces unit ]
	unitName = case unitType unit of TClass name _ _ _ -> name

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

initCaNetworkDef :: Project -> Network -> [Instruction]
initCaNetworkDef project network = [
		idefine varName caNetworkDef $ ENew $
			ECall "CaNetworkDef" [ EInt (networkId network), EInt (length units) ]
		] ++ countedMap (initCaUnitDef project (EVar varName)) units
	where
		units = networkUnits project network
		varName = nameFromId "network" (networkId network)

initCaUnitDef :: Project -> Expression -> Int -> Unit -> Instruction
initCaUnitDef project netexpr n unit =
	IStatement $ [
		idefine varName caUnitDef $ ENew
			(ECall "CaUnitDef" [ EInt $ unitId unit, EVar
				$ initCaUnitName unit, EInt (length transitions)])
	] ++ countedMap registerTransition transitions ++ map registerInit (unitInitPaths project unit) ++ [
		icall "->register_unit" [ netexpr, EInt n, EVar varName ]
	]
	where
		transitions = unitTransitions unit
		varName = nameFromId "unitdef" (unitId unit)
		registerTransition n t = icall "->register_transition"
			[ EVar varName, EInt n, EInt (transitionId t), arg1 t, arg2 t]
		arg1 t = EVar $ "(CaEnableFn*) " ++ transitionFnName t
		arg2 t = EVar $ "(CaFireFn*) " ++ fireFnName t
		registerInit p = icall "->register_init"
			[ EVar varName, multiPathCode project varNotAllowed p undefined ]
			-- We can use udefined as last argument because it has to be absolute path

varStruct :: Project -> Transition -> Type
varStruct project transition =
	struct (nameFromId "Vars" (transitionId transition)) $ visible ++ intern
	where
		visible = fromNelVarDeclarations $ transitionFreeVariables project transition
		intern = countedMap internDecl (normalInEdges transition)
		internDecl n edge = ("__token_" ++ show n, TPointer $ caToken $ fromNelType $ placeTypeById project (edgePlaceId edge))

transitionOfEdge :: Project -> Edge -> Transition
transitionOfEdge project edge = Maybe.fromJust $ List.find hasEdge (allTransitions project)
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
		checkTupleItem i (t, expr) = patternCheck project (tupleMember i source) t expr binded failed
		checkTuple xs =
			case t of
				TypeTuple ts -> IStatement $ countedMap checkTupleItem (zip ts xs)
				t -> error $ "Tuple expected but got " ++ show t

placeInUnit :: Expression -> Place -> Expression
placeInUnit expr place = EMemberPtr (nameFromId "place" (placeId place)) expr

coveredExpr :: NelExpression -> VarSet -> Bool
coveredExpr expr binded = freeVariables expr `Set.isSubsetOf` binded

guardInstruction :: Project -> Transition -> (String -> Expression) -> Instruction -> Instruction
guardInstruction project transition varFn failedI
	| guard transition == ExprTrue = INoop
	| otherwise = IIf (ECall "!" [ toExpression project varFn TypeBool (guard transition) ]) failedI INoop


matchTest :: Project -> Int -> [Edge] -> Set.Set String -> Instruction -> Instruction
matchTest project level [] binded instruction = instruction
matchTest project level edges@(edge@(Edge _ placeId (EdgeExpression expr) _ _):rest) binded instruction =
	IStatement $ [ idefine varName (TPointer $ caToken elementType) (ECall ".begin" [ placeExpr ]),
		IDo (ECall "!=" [ EVar varName, ECall ".begin" [ placeExpr ]]) (IStatement $ body ++ [ failedI ] )
	]
	where
		varName = "c_" ++ show level
		newBinded = Set.union binded $ freeVariables expr
		failedI = IStatement [ ISet (EVar varName) (EMemberPtr "next" (EVar varName)), IContinue ]
		body = [ tokenCheck, patternCheck project (EMemberPtr "element" (EVar varName)) (placeType place) expr binded failedI, guardI,
			matchTest project (level + 1) rest newBinded instruction ]
		transition = transitionOfEdge project edge
		processed = zip [0..] $ (List.\\) (edgesIn transition) edges
		fromSamePlace = filter (\(i, e) -> edgePlaceId e == placeId) processed
		placeExpr = placeAttr place (EVar "u")
		place = placeById project placeId
		elementType = fromNelType $ placeType place
		guardI | coveredExpr (guard transition) newBinded && not (coveredExpr (guard transition) binded) =
				guardInstruction project transition varFn failedI
				| otherwise = INoop
		tokenCheck | fromSamePlace == [] = INoop
			   | otherwise = IIf (callIfMore "||"
						[ ECall "==" [EVar ("c_" ++ show i), EVar varName] | (i, e) <- fromSamePlace ])
					failedI INoop
		varFn varName = EMember varName (EVar "vars")

matchTest project level (edge@(Edge _ placeId (EdgePacking name (Just limit)) _ _):rest) binded instruction =
	IIf (ECall ">=" [ ECall ".size" [ placeExpr ], ECall "+" [ limitExpr, EInt (length fromSamePlace - 1)]]) body INoop
	where
		place = placeById project placeId
		placeExpr = placeAttr place (EVar "u")
		limitExpr = toExpression project varFn TypeInt limit
		varFn varName = EMember varName (EVar "vars")
		body = matchTest project (level + 1) rest binded instruction
		fromSamePlace = filter (\e -> edgePlaceId e == placeId) (edgesIn (transitionOfEdge project edge))

matchTest project level edges@(edge@(Edge _ placeId (EdgePacking name (Nothing)) _ _):rest) binded instruction =
	error "Input packing edge without limit"

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

multiPathCode :: Project -> (String -> Expression) -> Path -> Expression -> Expression
multiPathCode project varfn path absPath =
	ECall "CaMultiPath" $ prefix ++ (EString $ map definition es) : (map (toExpression project varfn TypeInt) $ concatMap args es)
	where
		(prefix, es) = case path of
			(AbsPath es) -> ([], es)
			(RelPath lvlup es) -> ([ absPath, EInt lvlup ], es)
		definition (PathSingleton _) = 's'
		definition (PathRange _ _) = 'r'
		args (PathSingleton x) = [x]
		args (PathRange x y) = [x, y]

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

receiveFunction :: Unit -> Function
receiveFunction unit = function {
	functionName = "receive",
	parameters = [ ("thread", caThread, ParamNormal), ("place_pos", TInt, ParamNormal), ("unpacker", TRaw "CaUnpacker", ParamRef) ],
	instructions = countedMap processPlace (unitPlaces unit)
} where
	processPlace i place = IIf (ECall "==" [ EVar "place_pos", EInt i ]) (add place $ unpack (EVar "unpacker") (placeType place)) INoop
	add place expr = IStatement [
		icall ".add" [ placeAttr place (EVar "this"), expr ],
		icall "CA_LOG_TOKEN_ADD" [ EVar "thread", EVar "this", EInt (placeId place),
			asStringCall (placeType place) (EMemberPtr "element" (ECall ".last" [ placeAttr place (EVar "this") ]))]]

normalInEdges transition = filter isNormalEdge (edgesIn transition)
packingInEdges transition = filter isPackingEdge (edgesIn transition)

sendToken :: Expression -> Int -> Int -> NelType -> Expression -> Instruction
sendToken targetPath unitId placeNumber t source = IStatement [
	idefine "size" sizeType (exprMemSize t source),
	idefine "packer" (TRaw "CaPacker") $ ECall "CaPacker" [ EVar "size", ECall "CA_RESERVED_PREFIX" [ EVar "path" ] ],
	idefine "item" (fromNelType t) source,
	pack t (EVar "packer") (EVar "item"),
	icall "->send" [ EVar "thread", EVar "network", targetPath, EInt unitId, EInt placeNumber, EVar "packer" ]]

sendTokens :: Expression -> Int -> Int -> NelType -> Expression -> Instruction
sendTokens targetPath unitId placeNumber t source = IStatement $ sizeExpr ++ [
	idefine "packer" (TRaw "CaPacker") $ ECall "CaPacker" [ EVar "size", ECall "CA_RESERVED_PREFIX" [ EVar "path" ] ],
	IForeach (fromNelType t) "i" source [ pack t (EVar "packer") (EDeref (EVar "i")) ],
	icall "->multisend" [ EVar "thread", EVar "network", targetPath, EInt unitId, EInt placeNumber, ECall ".size" [ source ], EVar "packer" ]]
	where
		sizeExpr
			| isDirectlyPackable t = [ idefine "size" sizeType $ ECall "*" [ ECall ".size" [ source ], exprMemSize t source ] ]
			| otherwise = [ idefine "size" sizeType (EInt 0),
				IForeach (fromNelType t) "i" source [ ISet (EVar "size") $ ECall "+" [ EVar "size", exprMemSize t (EDeref (EVar "i")) ]]]

fireFn :: Project -> Transition -> Function
fireFn project transition = function {
	functionName = fireFnName transition,
	parameters = [
		("thread", caThread, ParamNormal),
		("network", caNetwork, ParamNormal),
		("unit" , caUnit, ParamNormal),
		("vars", TPointer $ varStruct project transition, ParamNormal)
	],
	instructions = [
		icall "CA_LOG_TRANSITION_START" [ EVar "thread", EVar "unit", EInt (transitionId transition) ],
		icall "->lock_active" [ EVar "network"],
		icall "->add_to_active_units" [ EVar "network", EVar "unit" ],
		icall "->unlock_active" [ EVar "network"],
		defineContext (EVar "thread") (EVar "unit"),
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		IStatement $ countedMap removeToken (normalInEdges transition) ++ map setPacking (packingInEdges transition),
		logStatus "unit" (unit project transition),
		callWorker, icall "CA_LOG_TRANSITION_END" [ EVar "thread", EVar "unit", EInt (transitionId transition) ] ]
			++ map processOutput (edgesOut transition)
	} where
	logStatus var unit = icall "CA_LOG_UNIT_STATUS" [ EVar "thread", EVar var, EInt (unitId unit) ]
	unitT = unitType (unit project transition)
	ePlace edge var = EMemberPtr (nameFromId "place" (edgePlaceId edge)) (EVar var)
	removeToken i edge = IStatement [
		icall "CA_LOG_TOKEN_REMOVE" [
			EVar "thread", EVar "unit", EInt (edgePlaceId edge), asStringCall (edgePlaceType project edge) (EMemberPtr "element" (tokenExpr i)) ],
		icall ".remove" [ ePlace edge "u", tokenExpr i ] ]
	tokenExpr i = EMemberPtr ("__token_" ++ show i) (EVar "vars")
	setPacking edge = case edgeInscription edge of
		(EdgePacking name _) -> ISet (EMemberPtr name (EVar "vars")) $ ECall ".to_vector_and_clear" [ ePlace edge "u" ]
	callWorker | hasCode transition = IStatement [
		icall "->unlock" [ EVar "u" ],
		icall (nameFromId "worker" $ transitionId transition) [ EVar "ctx", EDeref $ EVar "vars" ] ]
			| otherwise = icall "->unlock" [ EVar "u" ]
	processOutput edge
		| edgeGuard edge == ExprTrue = putTokens edge
		| otherwise = IIf (toExpression project varFn TypeBool (edgeGuard edge)) (putTokens edge) INoop
	putTokens edge
		| isSimplepath (edgeTarget edge) = IStatement $ [
			idefine "path" caPath $ absPathToExpression project varFn (EMemberPtr "path" (EVar "u")) (edgeTarget edge) ]
				++ putTokensToUnit edge
		| otherwise = IStatement $ [
				idefine "mpath" caMultiPath $ multiPathCode project varFn (edgeTarget edge) (EMemberPtr "path" (EVar "u")),
				idefine "iter" caMultiPathIter $ ECall ".get_iterator" [ EVar "mpath" ],
				IWhile (ECall ".has_next" [ EVar "iter" ]) $ IStatement
					(idefine "path" caPath (ECall ".next" [ EVar "iter" ]) : putTokensToUnit edge) ]
	putTokensToUnit edge
		| forcePackers project = [ sendInstruction edge ]
		| otherwise = [
			idefine "target" (TPointer $ unitType u) (ECast (ECall "->get_unit" [ (EVar "thread"),
					EVar "network",
					EVar "path",
					EInt (unitId u)]) (TPointer $ unitType u)),
			IIf (EVar "target") (IStatement [
				icall "->lock" [ EVar "target" ],
				putInstruction edge,
				icall "->activate" [ EVar "target", EVar "network" ],
				icall "->unlock" [ EVar "target" ]
			]) (sendInstruction edge) ] where u = edgeUnit project edge
	putInstruction edge = IStatement $ case edgeInscription edge of
		EdgeExpression expr -> [
			icall ".add" [ ePlace edge "target", toExpression project varFn (edgePlaceType project edge) expr ],
			icall "CA_LOG_TOKEN_ADD" [ EVar "thread", EVar "target", EInt (edgePlaceId edge),
				asStringCall (edgePlaceType project edge) (EMemberPtr "element" (ECall ".last" [ePlace edge "target"]))],
			logStatus "target" (placeUnit project (edgePlace project edge)) ]
		EdgePacking str Nothing -> [
			icall "CA_LOG_TOKEN_ADD_MORE" [ EVar "thread", EVar "target", EInt (edgePlaceId edge), varFn str,
				EType $ fromNelType (edgePlaceType project edge), asStringCall (edgePlaceType project edge) (EVar "CA_LOG_ITEM") ],
			icall ".add_all" [ ePlace edge "target", varFn str ],
			logStatus "target" (placeUnit project (edgePlace project edge)) ]
		EdgePacking _ (Just _) -> error "Packing expression with limit on output edge"
	sendInstruction edge = case edgeInscription edge of
		EdgeExpression expr -> sendToken (EVar "path") (uId edge) (pPos edge) (tokenType edge) $ toExpression project varFn (tokenType edge) expr
		EdgePacking str Nothing -> sendTokens (EVar "path") (uId edge) (pPos edge) (tokenType edge) (EMemberPtr str (EVar "vars"))
		EdgePacking _ (Just _) -> error "Packing expression with limit on output edge"
	tokenType edge = placeTypeById project (edgePlaceId edge)
	varFn varName = EMemberPtr varName (EVar "vars")
	uId edge = unitId $ edgeUnit project edge
	pPos edge = placePos (edgeUnit project edge) (edgePlace project edge)


transitionFn :: Project -> Transition -> Function
transitionFn project transition = function {
	functionName = transitionFnName transition,
	returnType = TInt,
	parameters = [
		("thread", caThread, ParamNormal),
		("network", caNetwork, ParamNormal),
		("unit", caUnit, ParamNormal),
		("firefn", TPointer (TRaw "CaFireFn"), ParamNormal) ],
	instructions = [
		defineContext (EVar "thread") (EVar "unit"),
		idefine "u" (TPointer unitT) $ ECast (EVar "unit") (TPointer unitT),
		checkPlaces,
		guardI,
		idefineEmpty "vars" (varStruct project transition),
		matchTest project 0 (edgesIn transition) Set.empty matched,
		IReturn (EInt 0)
	]
} where
	unitT = unitType (unit project transition)
	matched = IStatement $ map tokenSet [0 .. length (normalInEdges transition) - 1] ++
		[ icall "firefn" [ EVar "thread", EVar "network", EVar "u", EAddr (EVar "vars") ], IReturn (EInt 1) ]
	tokenSet i = ISet (EMember ("__token_" ++ show i) (EVar "vars")) (EVar $ "c_" ++ show i)
	checkPlaces = IStatement $ map checkPlace (allPlaces project)
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
	guardI | coveredExpr (guard transition) Set.empty =
			guardInstruction project transition varFn (IReturn $ EInt 0)
			| otherwise = INoop
	varFn varName = EMember varName (EVar "vars")


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
	parameters = [ ("argc", TRaw "int", ParamNormal),
		("argv", (TPointer . TPointer . TRaw) "char", ParamNormal) ],
	instructions = parseArgs ++ initNets ++ startCailie,
	returnType = TInt
} where
	startCailie = [ defsArray, IReturn $ ECall "ca_main" [ EInt (length nets), EVar "nets" ] ]
	parameters = projectParameters project
	nets = networks project
	initNets = concatMap (initCaNetworkDef project) nets
	defsArray =	IInline $ "CaNetworkDef *nets[] = {" ++
		addDelimiter "," (map ((nameFromId "network") . networkId) (networks project)) ++ "};"
	parseArgs = [
		IInline $ "const char *p_names[] = {" ++
			addDelimiter "," [ "\"" ++ parameterName p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "const char *p_descs[] = {" ++
			addDelimiter "," [ "\"" ++ parameterDescription p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "int *p_data[] = {" ++
			addDelimiter "," [ "&" ++ parameterGlobalName (parameterName p) | p <- parameters ] ++ "};",
		icall "ca_project_description" [ EString (projectDescription project) ],
		icall "ca_init" [ EVar "argc",
			EVar "argv",
			EInt (length parameters),
			EVar "p_names",
			EVar "p_data",
			EVar "p_descs" ] ]

parameterAccessFunction :: Parameter -> Function
parameterAccessFunction parameter = function {
	functionName = "parameter_" ++ parameterName parameter,
	instructions = [ (IReturn . EVar . parameterGlobalName . parameterName) parameter ],
	returnType = fromNelType $ parameterType parameter
}

knownTypeFunctions :: [(String, Type -> (Type, [ParamDeclaration]))]
knownTypeFunctions = [
	("getstring", \t -> (TRaw "std::string", [ ("obj", t, ParamConst) ])),
	("getsize", \t -> (sizeType, [ ("obj", t, ParamConst) ])),
	("pack", \t -> (TVoid, [ ("packer", TRaw "CaPacker", ParamRef), ("obj", t, ParamConst) ])),
	("unpack", \t -> (t, [ ("unpacker", TRaw "CaUnpacker", ParamRef) ])) ]


typeFunctions :: NelType -> [Function]
typeFunctions (TypeData typeName rawType _ functions) =
	map typeFunction functions
	where
		typeFunction (fname, code) = function {
			functionName = (typeName ++ "_" ++ fname),
			returnType = rtype,
			parameters = params,
			functionSource = Just ("*" ++ typeName ++ "/" ++ fname, 1),
			extraCode = code
		} where
			(rtype, params) = case List.lookup fname knownTypeFunctions of
				Just x -> x (TRaw rawType)
				Nothing -> error $ "typeFunctions: Unknown function: " ++ fname
typeFunctions _ = []

createProgram :: String -> Project -> String
createProgram filename project =
	emitProgram (FilePath.takeFileName filename) prologue types globals functions
	where
		functions = paramFs ++ userFs ++ typeFs ++ placeInitFs ++ workerFs
			++ fireFs ++ transitionFs ++ unitFs ++ [mainF]
		mainF = mainFunction project
		unitFs = map (initCaUnit project) units
		transitionFs = map (transitionFn project) $ allTransitions project
		fireFs = map (fireFn project) $ allTransitions project
		workerFs = map (workerFunction project) $ filter hasCode $ allTransitions project
		placeInitFs = map placeInitFunction $ filter hasInitCode $ allPlaces project
		types = Set.fromList $ varStructs ++ unitTypes
		varStructs = map (varStruct project) $ allTransitions project
		unitTypes = map unitType units
		units = projectUnits project
		userFs = map makeUserFunction (userFunctions project)
		typeFs = concatMap typeFunctions (Map.elems (typeTable project))
		paramFs = map parameterAccessFunction (projectParameters project)
		prologue = "#include <stdio.h>\n#include <string.h>\n#include <stdlib.h>\n#include <vector>\n#include <cailie.h>\n\n#include \"head.cpp\"\n\n"
		globals = [ (parameterGlobalName $ parameterName p, fromNelType $ parameterType p)
			| p <- projectParameters project ]

test =
	readFile "../vtwo/vtwo.xml" >>= return . (createProgram "testxyz") .
		projectFromXml >>= writeFile "../vtwo/vtwo.cpp"

test2 = do
	file <- readFile "../vtwo/vtwo.xml"
	return $ map (length . unitPlaces) $ projectUnits $ projectFromXml file
