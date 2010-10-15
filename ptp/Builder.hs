module Builder (
	createProgram
) where

import Declarations
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Parser
import Project
import ProjectTools
import Codegen
import Utils

-- |Function allowed in inscription language
nelFunctions = [
	( "+", TInt, [TInt, TInt]),
	( "*", TInt, [TInt, TInt ]),
	( "-", TInt, [TInt, TInt ]),
	( ">", TBool, [TInt, TInt ]),
	( "<", TBool, [TInt, TInt ]),
	( ">=", TBool, [TInt, TInt ]),
	( "<=", TBool, [TInt, TInt ]),
	( "==", TBool, [TInt, TInt ]),
	( "!=", TBool, [TInt, TInt ]),
	( "&&", TBool, [TBool, TBool ]),
	( "||", TBool, [TBool, TBool ]),
	( "iid", TInt, [])]

nelFunctionReturnType :: String -> Type
nelFunctionReturnType name =
	case List.find (\(n, r, p) -> n == name) nelFunctions of
		Just (n, r, p) -> r
		Nothing -> error $ "Unknown function: " ++ name

nelFunctionParams :: String -> [Type]
nelFunctionParams name =
	case List.find (\(n, r, p) -> n == name) nelFunctions of
		Just (n, r, p) -> p
		Nothing -> error $ "Unknown function: " ++ name

tupleToVars ::
	[VarDeclaration] ->
	VarSet ->
	String ->
	[Expression] ->
	(VarSet -> String -> Expression -> ([VarDeclaration], [Instruction])) ->
	([VarDeclaration], [Instruction])
tupleToVars vdecls binded var exprs fn =
	( declarations' ++ declarations, instructions )
	where
		(declarations, instructions) = processExpr varNames exprs binded 0
		declarations' = zip varNames $ map (exprType (makeDeclarations vdecls [])) exprs
		varNames = take (length exprs) (newVars' var)
		processExpr :: [String] -> [Expression] -> VarSet -> Int -> ([VarDeclaration], [Instruction])
		processExpr [] _ _ _ = ([], [])
		processExpr (v:names) (e:exprs) b n =
			let
				(d', i') = processExpr names exprs (Set.union b (freeVariables e)) (n + 1)
				(d, i) = fn b v e in
				(d ++ d', [ (ISet (ExprVar v) (ExprAt (ExprInt n) (ExprVar var))) ] ++ i ++ i')

caContext = TPointer (TRaw "CaContext")

patternCheck :: [VarDeclaration] -> VarSet -> String -> Expression -> Instruction -> ([VarDeclaration], [Instruction])
patternCheck decls binded var (ExprTuple exprs) errEvent =
	tupleToVars decls binded var exprs (\b v e -> patternCheck decls b v e errEvent)
patternCheck decls binded var (ExprVar v) errEvent | not (Set.member v binded) = ([], [ISet (ExprVar v) (ExprVar var)])
patternCheck decls binded var (ExprVar v) errEvent = 
	case List.lookup v decls of
		Just (TData _ _ _ _) -> error "Extern types cannot be compared"
		Just _ -> ([], [(IIf (ExprCall "!=" [(ExprVar v), (ExprVar var)]) errEvent INoop)])
		Nothing -> error "patternCheck: This cannot happend"
patternCheck decls binded var x errEvent = ([], [(IIf (ExprCall "!=" [x, (ExprVar var)]) errEvent INoop)])

patternCheckStatement :: [VarDeclaration] -> VarSet -> String -> Expression -> Instruction -> Instruction
patternCheckStatement decls binded var expr errEvent =
	makeStatement decl instructions
	where
		(decl, instructions) = patternCheck decls binded var expr errEvent

unionsVariableTypes :: [Map.Map String Type] -> Map.Map String Type
unionsVariableTypes decls =
	Map.unionsWith unionFn decls
	where
		unionFn a b = if a == b then a else error "Type inference failed"


variableTypes :: Project -> Expression -> Type -> Map.Map String Type
variableTypes project (ExprVar string) ctype = Map.singleton string ctype
variableTypes project (ExprParam paramName) t | (parameterTypeByName project paramName) == t = Map.empty
variableTypes project (ExprInt _) (TInt) = Map.empty
variableTypes project (ExprString _) (TString) = Map.empty
variableTypes project (ExprTuple []) (TTuple []) = Map.empty
variableTypes project (ExprTuple exprs) (TTuple types)
	| length exprs == length types =
		unionsVariableTypes $ zipWith (variableTypes project) exprs types
{- THIS REALLY NEED FIX! Function call result has to be checked -}
variableTypes project (ExprCall name params) t 
	| (nelFunctionReturnType name == t) && (length params == length (nelFunctionParams name)) = 
		unionsVariableTypes $ zipWith (variableTypes project) params (nelFunctionParams name)
variableTypes project x y = error $ "Type inference failed: " ++ show x ++ "/" ++ show y

placesTuple :: Network -> Type
placesTuple network = TTuple $ map (TArray . placeType) (places network)

processInputExpr :: (String -> Expression) -> Expression -> Expression
processInputExpr fn (ExprInt x) = ExprInt x
processInputExpr fn (ExprString x) = ExprString x
processInputExpr fn (ExprVar x) = fn x
processInputExpr fn (ExprParam x) = ExprVar $ parameterGlobalName x
processInputExpr fn (ExprCall "iid" []) = ExprCall ".iid" [ ExprVar "ctx" ]
processInputExpr fn (ExprCall "+" exprs) = ExprCall "+" [ processInputExpr fn expr | expr <- exprs ]
processInputExpr fn (ExprCall "-" exprs) = ExprCall "-" [ processInputExpr fn expr | expr <- exprs ]
processInputExpr fn (ExprCall "*" exprs) = ExprCall "*" [ processInputExpr fn expr | expr <- exprs ]
processInputExpr fn (ExprTuple exprs) = ExprTuple [ processInputExpr fn expr | expr <- exprs ]
processInputExpr fn x = error $ "Input expression contains: " ++ show x

{- FIXME: Forbid calling iid() etc -}
processInputExprParamsOnly :: Expression -> Expression
processInputExprParamsOnly = processInputExpr (\x -> error "Variables are not allowed in this expression")

parameterGlobalName :: String -> String
parameterGlobalName x = "__parameter_" ++ x

transitionVarType :: Project -> Transition -> Type
transitionVarType project transition = TStruct ("Vars_t" ++ show (transitionId transition)) $ transitionFreeVariables project transition

transportType :: Project -> Transition -> [Edge] -> Int -> Type
transportType project transition edges helpId =
	TStruct ("Transport_" ++ show (transitionId transition) ++ "_" ++ show helpId) types
	where
		types = edgesFreeVariables project edges

processEdge ::  Network -> Edge -> String -> [String] -> [Instruction] -> Instruction
processEdge network (Edge placeId expr _) var restrictions body =
		IForeach var counterVar (ExprAt (ExprInt seq) (ExprVar "places")) (prefix:body)
	where 	
		counterVar = var ++ "_i"
		seq = placeSeqById network placeId
		t = placeTypeById network placeId
		prefix = if restrictions == [] then INoop else
			IIf (callIfMore "||" [ ExprCall "==" [(ExprVar v), (ExprVar counterVar)] | v <- restrictions ]) IContinue INoop


checkEdges :: Network -> [VarDeclaration] -> VarSet -> [Edge] -> [Edge] -> Int -> Instruction -> [Expression] -> Instruction
checkEdges network decls binded processedEdges [] level okEvent guards = okEvent
{- Variant for normal edges -}
checkEdges network decls binded processedEdges (edge:rest) level okEvent guards | isNormalEdge edge =
	processEdge network edge var (compRestrictions processedEdges 0) $ [
		 	patternCheckStatement decls binded var (processInputExpr ExprVar expr) IContinue ] ++
			map guardCode coveredGuards ++ 
			[ checkEdges network decls newVars (edge:processedEdges) rest (level + 1) okEvent uncoveredGuards ]
	where
		newVars = Set.union binded $ freeVariables expr
		(coveredGuards, uncoveredGuards) = List.partition (isCovered newVars) guards
		guardCode guard = if guard == ExprTrue then INoop else IIf (ExprCall "!" [ guard ]) IContinue INoop
		EdgeExpression expr = edgeInscription edge
		varCounterName level = "c_" ++ show level ++ "_i"
		var = "c_" ++ show level
		compRestrictions :: [Edge] -> Int -> [String]
		compRestrictions [] _ = []
		compRestrictions (e:es) level
			| edgePlaceId edge == edgePlaceId e = (varCounterName level):compRestrictions es (level + 1)
			| otherwise = compRestrictions es (level + 1)

{- Variant for packing edges -}
checkEdges network decls binded processedEdges (edge:rest) level okEvent guards =
	makeStatement [] [
		IIf (ExprCall "<" [ ExprCall "List.size" [ placeExpr ], ExprCall "+" [ limitExpr, ExprInt (length edgesWithSamePlace)]])
			operation INoop,
		(checkEdges network decls binded (edge:processedEdges) rest (level + 1) okEvent guards)
	]
	where
		operation = if any isNormalEdge processedEdges then IContinue else IReturn (ExprInt 0)
		edgesWithSamePlace = [ e | e <- processedEdges, edgePlaceId e == edgePlaceId edge ]
		EdgePacking name limit = edgeInscription edge
		limitExpr = case limit of
			Just x -> x
			Nothing -> error "Limit on input edge is not defined"
		placeExpr = ExprAt (ExprInt (placeSeqById network (edgePlaceId edge))) (ExprVar "places")

transitionFunctionName :: Transition -> String
transitionFunctionName transition = "transition_" ++ show (transitionId transition)

-- |Return all variables in edge (main expression, target, limit)
extractEdgeVariables :: Project -> Edge -> [Map.Map String Type]
extractEdgeVariables project edge =
	case edgeInscription edge of
		EdgeExpression x -> (variableTypes project x (edgePlaceType project edge)) : targetExpr
		EdgePacking name (Just x) -> [ Map.singleton name $ TArray (edgePlaceType project edge), (variableTypes project x TInt) ] ++ targetExpr
		EdgePacking name Nothing -> (Map.singleton name $ TArray (edgePlaceType project edge)) : targetExpr
	where targetExpr = case edgeTarget edge of
		Just x -> [(variableTypes project x TInt)]
		Nothing -> []

edgesFreeVariables :: Project -> [Edge] -> [VarDeclaration]
edgesFreeVariables project edges =
	Map.toList declsMap
	where
		declsMap = unionsVariableTypes $ concatMap (extractEdgeVariables project) edges

{- On edges in & out -}
transitionFreeVariables :: Project -> Transition -> [VarDeclaration]
transitionFreeVariables project transition =
	edgesFreeVariables project $ (edgesIn transition) ++ (edgesOut transition)

transitionFreeVariablesIn :: Project-> Transition -> [VarDeclaration]
transitionFreeVariablesIn project transition =
	edgesFreeVariables project (edgesIn transition)

transitionFreeVariablesOut :: Project -> Transition -> [VarDeclaration]
transitionFreeVariablesOut project transition =
	edgesFreeVariables project (edgesOut transition)

-- |Remove edges that has not place in network
transitionFilterEdges :: Network -> [Edge] -> [Edge]
transitionFilterEdges network edges =
	filter edgeFromNetwork edges
	where edgeFromNetwork edge = List.elem (edgePlaceId edge) (map placeId (places network))

reportFunctionName :: Network -> String
reportFunctionName network = "report_" ++ show (networkId network)

reportFunction :: Project -> Network -> Function
reportFunction project network = Function {
	functionName = reportFunctionName network,
	parameters = [ ("ctx", caContext), ("places", TPointer $ (placesTuple network)), ("out", TPointer $ TRaw "CaOutput") ],
	declarations = [],
	instructions = header ++ concat (countedMap reportPlace (places network)) ++ concatMap reportTransition (transitions network),
	extraCode = "",
	returnType = TVoid
	}
	where
		header = [ 
			icall ".set" [ ExprVar "out", ExprString "node", ExprCall ".node" [ ExprVar "ctx" ] ],
			icall ".set" [ ExprVar "out", ExprString "iid", ExprCall ".iid" [ ExprVar "ctx" ] ],
			icall ".set" [ ExprVar "out", ExprString "network-id", ExprInt (networkId network) ],
			IIf (ExprCall "._check_halt_flag" [ ExprVar "ctx" ])
				(icall ".set" [ ExprVar "out", ExprString "running", ExprString "false" ])
				(icall ".set" [ ExprVar "out", ExprString "running", ExprString "true"])]
		reportPlace i p = [
			icall ".child" [ ExprVar "out", ExprString "place" ],
			icall ".set" [ ExprVar "out", ExprString "id", ExprInt (placeId p) ],
			IForeach "x" "x_c" (ExprAt (ExprInt i) (ExprVar "places")) [
				icall ".child" [ ExprVar "out", ExprString "token" ],
				icall ".set" [ ExprVar "out", ExprString "value", ExprCall "Base.asString" [ ExprVar "x" ] ],
				icall ".back" [ ExprVar "out" ]
			],
			icall ".back" [ ExprVar "out"]] 
		reportTransition t = [
			icall ".child" [ ExprVar "out", ExprString "transition" ],
			icall ".set" [ ExprVar "out", ExprString "id", ExprInt (transitionId t) ],
			IIf (ExprCall (transitionEnableTestFunctionName t) [ ExprVar "ctx", ExprVar "places" ])
				(icall ".set" [ ExprVar "out", ExprString "enable", ExprString "true" ])
				(icall ".set" [ ExprVar "out", ExprString "enable", ExprString "false"]),
			icall ".back" [ ExprVar "out" ]
			]


transitionFunction :: Project -> Network -> Transition -> Function
transitionFunction project network transition = Function {
		functionName = transitionFunctionName transition,
		parameters = [ ("ctx", caContext), ("places", TPointer $ placesTuple network)],
		declarations = decls,
		instructions = [instructions, IReturn (ExprInt 0)],
		extraCode = "",
		returnType = TInt
	}
	where
		decls = transitionFreeVariablesIn project transition
		instructions = checkEdges network decls Set.empty [] (edgesIn transition) 0 (transitionOkEvent project network transition) [ guard transition ]

transitionEnableTestFunctionName :: Transition -> String
transitionEnableTestFunctionName transition = "transition_enable_" ++ show (transitionId transition)

transitionEnableTestFunction :: Project -> Network -> Transition -> Function
transitionEnableTestFunction project network transition = Function {
	functionName = transitionEnableTestFunctionName transition,
	parameters = [ ("ctx", caContext), ("places", TPointer $ placesTuple network)],
	declarations = decls,
	instructions = [ instructions, IReturn (ExprInt 0) ],
	extraCode = "",
	returnType = TInt
	} 
	where
		decls = transitionFreeVariablesIn project transition
		instructions = checkEdges network decls Set.empty [] (edgesIn transition) 0 (IReturn $ ExprInt 1) [ guard transition ]

{-
	Erasing dependancy is added for reason that { List.eraseAt(l, x); List.eraseAt(l, y); } is problem if x < y
-}
transitionOkEvent project network transition = makeStatement [ ("var", transitionVarType project transition) ] body
	where
		body = map erase eraseDependancy ++ packing ++ map setVar decls ++ [ call ] ++ applyResult ++
				(sendInstructions project network transition) ++ [ IReturn (ExprInt 1) ]
		localOutEdges = filter (Maybe.isNothing . edgeTarget) $ transitionFilterEdges network (edgesOut transition)
		setVar (name, _) = ISet (ExprAt (ExprString name) (ExprVar "var")) (ExprVar name)
		decls = transitionFreeVariablesIn project transition
		counterName i = "c_" ++ show i ++ "_i"
		placeExprOfEdge edge = ExprAt (ExprInt (placeSeqById network (edgePlaceId edge))) (ExprVar "places")
		erase ((i, edge), dep) = safeErase (placeExprOfEdge edge) (counterName i) (map (counterName . fst) dep)
		eraseDependancy = triangleDependancy (\(i1,e1) (i2,e2) -> edgePlaceId e1 == edgePlaceId e2) (zip [0..] [ e | e <- edgesIn transition, isNormalEdge e ])
		call = icall (workerFunctionName transition) [ ExprVar "ctx", ExprVar "var" ]
		applyResult = map addToPlace localOutEdges
		addToPlace edge = case edgeInscription edge of
			EdgeExpression expr -> addToPlaceOne edge expr
			EdgePacking name _ -> addToPlaceMany edge name
		addToPlaceOne edge expr = icall "List.append" [ placeExpr edge, (preprocess expr) ]
		addToPlaceMany edge name = IForeach "token" "token_c" (ExprAt (ExprString name) (ExprVar "var")) 
			[ icall "List.append" [ placeExpr edge, ExprVar "token" ]]
		placeExpr edge = ExprAt (ExprInt (placeSeqById network (edgePlaceId edge))) (ExprVar "places")
		preprocess e = processInputExpr (\x -> (ExprAt (ExprString x) (ExprVar "var"))) e
		packing = concat [ packingEdge e | e <- edgesIn transition, not (isNormalEdge e) ]
		packingEdge e = let EdgePacking name _ = edgeInscription e in
			[ ISet (ExprVar name) (placeExprOfEdge e),
			  icall "List.clear" [ placeExprOfEdge e ] ]


sortBySendingPriority :: [Edge] -> [Edge]
sortBySendingPriority edges = List.sortBy sortFn edges
	where
		sortFn x y | isNormalEdge x == isNormalEdge y = EQ
				| isNormalEdge x = GT
				| otherwise = LT

sendInstructions :: Project -> Network -> Transition -> [Instruction]
sendInstructions project network transition =
	[ sendStatement project network (edgeNetwork project edge) transition edge | edge <- sortBySendingPriority foreignEdges ]
	where
		foreignEdges = filter (Maybe.isJust . edgeTarget) (edgesOut transition)
		{- Disabled as premature optimization
		networkAndEdgesAll = concat [ [ (n, e, helpId) | (e, helpId) <- zip (divide edgeTarget (transitionFilterEdges n edges)) [1..] ] | n <- networks project ]
		networkAndEdges = filter (\(_, x, _) -> x /= []) networkAndEdgesAll -}

packCode :: Expression -> Type -> Expression -> Instruction
packCode packer t expr | canBeDirectlyPacked t = 
	makeStatement [ ("data", t) ] [ 
		ISet (ExprVar "data") expr,
		icall ".pack" [ packer, ExprAddr (ExprVar "data"), exprMemSize t expr ]
	]
packCode packer TString expr = makeStatement [ ("data", TString), ("size", TRaw "size_t") ] [ 
			ISet (ExprVar "data") expr,
			ISet (ExprVar "size") $ ExprCall ".size" [ExprVar "data"],
			icall ".pack_size" [ packer, ExprVar "size" ],
			icall ".pack" [ packer, ExprCall ".c_str" [ ExprVar "data" ], ExprVar "size" ]]
packCode packer (TTuple types) expr = makeStatement [] [ packCode packer t (ExprAt (ExprInt x) expr) | (x, t) <- zip [0..] types ]
packCode packer t expr = error "packCode: Type cannot be packed"

unpackCode :: Expression -> Type -> String -> Instruction
unpackCode unpacker t var | canBeDirectlyPacked t =
	makeStatement [ ("p", TPointer TVoid) ] [ 
		ISet (ExprVar "p") (ExprCall ".unpack" [ unpacker, exprMemSize t undefined ]),
		IInline (var ++ " = *(" ++ typeString (TPointer t) ++ ") p;")]
unpackCode unpacker TString var = 
	makeStatement [ ("size", TRaw "size_t"), ("sdata", TRaw "char *") ] [
		ISet (ExprVar "size") (ExprCall ".unpack_size" [ unpacker ]),
		IInline "sdata = (char*) unpacker.unpack(size);",
		ISet (ExprVar var) (ExprCall "std::string" [ ExprVar "sdata", ExprVar "size" ])
	]
unpackCode unpacker (TTuple types) var = 
	makeStatement vars $ [ unpackCode unpacker t name | (name, t) <- vars ] ++ 
		[ ISet (ExprVar var) (ExprTuple [ ExprVar name | (name, t) <- vars ])]
	where vars = [ (var ++ show i, t) | (i, t) <- zip [0..] types ]

unpackCode unpacker t var = error $ "unpackCode: Type cannot be unpacked"

sendStatement :: Project -> Network -> Network -> Transition -> Edge -> Instruction
{- Version for normal edges -}
sendStatement project fromNetwork toNetwork transition edge | isNormalEdge edge =
	makeStatement' [] [ ("item", etype, preprocess expr), 
		("packer", TRaw "CaPacker", ExprCall "CaPacker" [exprMemSize etype (ExprVar "item")]) ] [
		packCode packer etype (ExprVar "item"),
		IExpr (ExprCall "ca_send" [
			ExprVar "ctx",
			target,
			dataId,
			packer ])
	]
	where
		etype = edgePlaceType project edge
		packer = ExprVar "packer"
		EdgeExpression expr = edgeInscription edge
		dataId = ExprInt $ edgePlaceId edge
		preprocess e = processInputExpr (\x -> (ExprAt (ExprString x) (ExprVar "var"))) e
		target =  ExprCall "+" [ (processedAddress toNetwork),
			(preprocess . Maybe.fromJust . edgeTarget) edge ]
{- Version for packing edge -}
sendStatement project fromNetwork toNetwork transition edge =
	makeStatement' [] [ ("target", TInt, target) ]
	 [
	IForeach "item" "i" (ExprAt (ExprString name) (ExprVar "var")) [
	makeStatement' [] [ ("packer", TRaw "CaPacker", ExprCall "CaPacker" [exprMemSize etype (ExprVar "item")]) ] [
		packCode packer etype (ExprVar "item"),
		IExpr (ExprCall "ca_send" [
			ExprVar "ctx",
			ExprVar "target",
			dataId,
			packer ])
	]]]
	{-icall "ca_send" [


		ExprVar "ctx",
		ExprVar "target",
		dataId,
		ExprAddr $ ExprVar "data",
		ExprCall "sizeof" [ExprVar (typeString (edgePlaceType project edge))]
	]]]-}
	where
		packer = ExprVar "packer"
		etype = edgePlaceType project edge
		EdgePacking name limit = edgeInscription edge
		dataId = ExprInt $ edgePlaceId edge
		preprocess e = processInputExpr (\x -> (ExprAt (ExprString x) (ExprVar "var"))) e
		target =  ExprCall "+" [ (processedAddress toNetwork),
			(preprocess . Maybe.fromJust . edgeTarget) edge ]

{- Disables as premature optimization
sendStatement :: Project -> Network -> Network -> Transition -> [Edge] -> Int -> Instruction
sendStatement project fromNetwork toNetwork transition edges helpId =
	IStatement [ ("transport", transportType project transition edges helpId) ] ((map addToTransport filteredEdges) ++ [callSend])
	where
		filteredEdges = List.nubBy (\x y -> edgeExpr x == edgeExpr y) edges
		addToTransport edge = ISet (ExprAt (varStrFromEdge edge) (ExprVar "transport")) (ExprAt (varStrFromEdge edge) (ExprVar "var"))
		varStrFromEdge edge = let ExprVar x = edgeExpr edge in ExprString x {- Ugly hack, need flexibile code -}
		callSend = IExpr (ExprCall "ca_send" [ target, dataId, ExprAddr (ExprVar "transport"), ExprCall "sizeof" [ExprVar "transport"]])
		dataId = ExprInt $ (transitionId transition) * 1000 + helpId {- cheap little hack -}
		target = ExprCall "+" [ ExprInt (address toNetwork), (Maybe.fromJust . edgeTarget . head) edges ]
-}


recvFunctionName :: Network -> String
recvFunctionName network = "recv_callback" ++ show (networkId network)

allTransitions :: Project -> [Transition]
allTransitions project = concatMap transitions (networks project)

{- !!
   In fact this is next little cheap hack, dataId of packet is directly place,
   there shoule be more grupped sending
-}
recvFunction :: Project -> Network -> Function
recvFunction project network = Function {
	functionName = recvFunctionName network,
	parameters = [ ("places", TPointer (placesTuple network)), ("data_id", TRaw "int"), ("data", TRaw "void*"), ("data_size", TRaw "int") ],
	declarations = [],
	extraCode = [],
	returnType = TVoid,
	instructions = [ makeStatement' [] [ ("unpacker", TRaw "CaUnpacker", (ExprCall "CaUnpacker" [ ExprVar "data" ])) ] 
		[ recvStatement network place | place <- (places network), isTransportable (placeType place) ]]
}

recvStatement :: Network -> Place -> Instruction
recvStatement network place =
	IIf condition ifStatement INoop
	where
		condition = ExprCall "==" [ ExprVar "data_id", ExprInt (placeId place) ]
		ifStatement = makeStatement [ ("item", placeType place) ] [
			unpackCode (ExprVar "unpacker") (placeType place) "item",
			IExpr (ExprCall "List.append" [ ExprAt (ExprInt (placeSeq network place)) (ExprVar "places"),  (ExprVar "item") ])]

{- This is not good aproach if there are more vars, but it now works -}
safeErase :: Expression -> String -> [String] -> Instruction
safeErase list v [] = icall "List.eraseAt" [ list, ExprVar v ]
safeErase list v deps = makeStatement [ ("tmp", TInt) ] $ [ ISet (ExprVar "tmp") (ExprVar v) ] ++ erase deps
	where 
		erase [] = [ safeErase list "tmp" [] ]
		erase (d:ds) = 
			(IIf (ExprCall "<" [ ExprVar d, ExprVar v ]) (ISet (ExprVar "tmp") (ExprCall "-" [ ExprVar "tmp", ExprInt 1 ])) INoop):(erase ds)

workerFunctionName :: Transition -> String
workerFunctionName transition = "worker_" ++ show (transitionId transition)

workerFunction :: Project -> Transition -> Function
workerFunction project transition = Function {
	functionName = workerFunctionName transition,
	parameters = [ ("ctx", caContext), ("var", transitionVarType project transition) ],
	declarations = [],
	instructions = [],
	extraCode = transitionCode transition,
	returnType = TVoid
}

initFunctionName :: Place -> String
initFunctionName place = "init_place_" ++ show (placeId place)

initFunction :: Place -> Function
initFunction place = Function {
		functionName = initFunctionName place,
		parameters = [ ("ctx", caContext), ("place", TPointer $ TArray (placeType place))],
		declarations = [],
		instructions = [],
		extraCode = placeInitCode place,
		returnType = TVoid
	}

placesWithInit :: Network -> [Place]
placesWithInit network = [ p | p <- places network, placeInitCode p /= "" ]

startFunctionName :: Network -> String
startFunctionName network = "init_network_" ++ show (networkId network)

startFunction :: Network -> Function
startFunction network = Function {
	functionName = startFunctionName network,
	parameters = [ ("ctx", caContext) ],
	declarations = [ ("places", TPointer $ placesTuple network) ],
	instructions = [ allocPlaces, initCtx ] ++ registerTransitions ++ initPlaces,
	extraCode = "",
	returnType = TVoid
	} where
	{-	allocPlaces = ISet (ExprVar "places") $ ExprCall "new" [ ExprCall (typeString (TPointer $ placesTuple network))  [] ]-}
		allocPlaces = IInline $ "places = new " ++ (typeSafeString (placesTuple network)) ++ "();"
		nodeExpr = ExprCall ".node" [ ExprVar "ctx" ]
		initCtx = icall "._init" [
			(ExprVar "ctx"), (ExprCall "-" [nodeExpr, (processedAddress network)]),
			(processedInstances network), ExprVar "(void*) places", 
			{- This is ugly hack -} ExprVar ("(RecvFn*)" ++ (recvFunctionName network)),  ExprVar ("(ReportFn*)" ++ reportFunctionName network) ]
		ps p = placeSeq network p
		initPlaces = (map initPlaceFromExpr (places network)) ++ (map callInitPlace (placesWithInit network))
		initPlace p = [	initPlaceFromExpr p, callInitPlace p ]
		registerTransitions = [ icall "._register_transition" [ 
			ExprVar "ctx", ExprInt (transitionId t), ExprVar ("(TransitionFn*)" ++ transitionFunctionName t) ] | t <- transitions network ]
		placeVar p = ExprAt (ExprInt (ps p)) (ExprVar "places")
		callInitPlace p =
			 icall (initFunctionName p) [ ExprVar "ctx", ExprAddr (placeVar p) ]
		initPlaceFromExpr p =
			case placeInitExpr p of
				Nothing -> INoop
				Just x -> icall "List.append" [ placeVar p, x ]
	{-	startCode = \n\t(ctx, &places, tf, (RecvFn*) " ++ recvFunctionName network ++ ");"-}
	
createNetworkFunctions :: Project -> Network -> [Function]
createNetworkFunctions project network =
	workerF ++ transitionF ++ transitionTestF ++ reportF ++ initF ++ [ recvFunction project network, startFunction network ]
	where
		transitionF = [ transitionFunction project network t | t <- transitions network ]
		transitionTestF = [ transitionEnableTestFunction project network t | t <- transitions network ]
		initF = map initFunction (placesWithInit network)
		reportF = [ reportFunction project network ]
		workerF =  [ workerFunction project t | t <- transitions network ] {- workerFunction -}

instancesCount :: Project -> Expression
instancesCount project = ExprCall "+" $ map (processedInstances) (networks project)

createMainFunction :: Project -> Function
createMainFunction project = Function {
	functionName = "main",
	parameters = [ ("argc", TRaw "int"), ("argv", (TPointer . TPointer . TRaw) "char") ],
	declarations = [ ("nodes", TInt) ],
	instructions = parseArgs ++ [ i1, i2 ],
	extraCode = [],
	returnType = TInt
} where
	i1 = ISet (ExprVar "nodes") (instancesCount project)
	i2 = IInline "ca_main(nodes, main_init);"
	parameters = projectParameters project
	parseArgs = [ 
		IInline $ "const char *p_names[] = {" ++ addDelimiter "," [ "\"" ++ parameterName p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "const char *p_descs[] = {" ++ addDelimiter "," [ "\"" ++ parameterDescription p ++ "\"" | p <- parameters ] ++ "};",
		IInline $ "int *p_data[] = {" ++ addDelimiter "," [ "&" ++ (parameterGlobalName . parameterName) p | p <- parameters ] ++ "};",
		icall "ca_parse_args" [ ExprVar "argc", ExprVar "argv", ExprInt (length parameters), ExprVar "p_names", ExprVar "p_data", ExprVar "p_descs" ]
	 ]

processedInstances :: Network -> Expression
processedInstances = processInputExprParamsOnly . instances

processedAddress :: Network -> Expression
processedAddress = processInputExprParamsOnly . address

createMainInitFunction :: Project -> Function
createMainInitFunction project = Function {
	functionName = "main_init",
	parameters = [("ctx", caContext)],
	declarations = [],
	instructions = startNetworks,
	extraCode = [],
	returnType = TVoid
	}
	where
		node = ExprCall ".node" [ ExprVar "ctx" ]
		startNetworks = map startNetwork (networks project)
		test1 n = ExprCall ">=" [ node, processedAddress n]
		test2 n = ExprCall "<" [ node, ExprCall "+" [processedInstances n, processedAddress n]]
		startI n = icall (startFunctionName n) [ ExprVar "ctx" ]
		startNetwork n = IIf (ExprCall "&&" [ test1 n, test2 n ]) (startI n) INoop


functionWithCode :: String -> Type -> [VarDeclaration] -> String -> Function
functionWithCode name returnType params code = Function {
	functionName = name,
	parameters = params,
	declarations = [],
	instructions = [],
	extraCode = code,
	returnType = returnType
}

knownTypeFunctions = [
	("getstring", (TRaw "std::string", \raw -> [ ("obj", TRaw $ raw ++ "&") ]))]

typeFunctions :: Type -> [Function]
typeFunctions (TData typeName rawType transportMode ((fname, code):rest)) = 
	(functionWithCode (typeName ++ "_" ++ fname) returnType (params rawType) code) 
		: typeFunctions (TData typeName rawType transportMode rest)
	where (returnType, params) = case List.lookup fname knownTypeFunctions of
		Just x -> x
		Nothing -> error $ "typeFunctions: Unknown function " ++ fname
typeFunctions _ = []
		
createProgram :: Project -> String
createProgram project =
	emitProgram prologue globals $ typeF ++ netF ++ [mainInitF, mainF]
	where
		globals = [ (parameterGlobalName $ parameterName p, parameterType p) | p <- projectParameters project ]
		typeF = concatMap typeFunctions $ Map.elems (typeTable project)
		netF = concat [ createNetworkFunctions project n | n <- networks project ]
		mainInitF = createMainInitFunction project
		mainF = createMainFunction project
		prologue = "#include <stdio.h>\n#include <stdlib.h>\n#include <vector>\n#include <cailie.h>\n\n#include \"head.cpp\"\n\n"

test = readFile "../out/project.xml" >>= return . createProgram . projectFromXml >>= writeFile "../out/project.cpp"
test2 = readFile "../out/project.xml" >>= return . projectFromXml
