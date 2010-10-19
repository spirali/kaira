
module Codegen (
	emitProgram,
	callPrint,
	callPrintfStr,
	typeString,
	typeSafeString,
	orderTypeByDepedancy -- for Tests.hs
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Declarations
import Utils
import ProjectTools

data Scope = Scope {
	scopeDeclarations :: Declarations
} deriving (Show)

data SourceCode = Text String | Join SourceCode SourceCode | Block SourceCode | Eol | Empty

stdFunctions = [
	("+", TInt, [TInt, TInt]),
	("*", TInt, [TInt, TInt])]

sourceCodeToStr :: SourceCode -> String
sourceCodeToStr code =
	toStr code 0 True
	where
		toStr Empty _ _ = ""
		toStr (Join a b) n i = toStr a n i ++ toStr b n (endWithEol a)
		toStr (Text s) n True = indentStr n ++ s
		toStr (Text s) _ False = s
		toStr (Block c) n i = "\n" ++ toStr c (n + 1) True ++ if endWithEol c then "" else "\n"
		toStr Eol n i = "\n"

		endWithEol Eol = True
		endWithEol (Block _) = True
		endWithEol (Join a b) = endWithEol b
		endWithEol _ = False

(<+>) :: SourceCode -> SourceCode -> SourceCode
a <+> Empty = a
Empty <+> b = b
a <+> b = Join a b

indentStr :: Int -> String
indentStr n = List.replicate n '\t'

joinMap :: (a -> SourceCode) -> [a] -> SourceCode
joinMap f = foldr ((<+>) . f) Empty

infixFunctions = [ "+", "-", "/", "*", "!=", "==", "||", "&&", ">=", "<=", ">", "<" ]

addDeclarations :: Scope -> Declarations -> Scope
addDeclarations scope decls =
	Scope $ decls `declarationsJoin` scopeDeclarations scope

emitCall scope ('.':name) (obj:params) =
	emitExpression scope obj ++ dotOrArrow ++ name ++ "(" ++ addDelimiter "," (map (emitExpression scope) params) ++ ")"
	where dotOrArrow =
		case exprType (scopeDeclarations scope) obj of
			TStruct _ _ -> "."
			TPointer _ -> "->"
			TRaw _ -> "."
			TString -> "."
			x -> error $ "Invalid type for calling method '" ++ name ++ "' at " ++ show obj ++ "/" ++ show x

emitCall scope "Base.asString" [x] = emitExpression scope $ exprAsString (scopeDeclarations scope) x
emitCall scope "List.size" [e1] = emitExpression scope e1 ++ ".size()"
emitCall scope "List.clear" [e1] = emitExpression scope e1 ++ ".clear()"
emitCall scope "List.eraseAt" [e1,e2] =
	emitExpression scope e1 ++ ".erase(" ++ emitExpression scope e1 ++ ".begin()+" ++ emitExpression scope e2 ++ ")"
emitCall scope "List.append" [e1,e2] =
	emitExpression scope e1 ++ ".push_back(" ++ emitExpression scope e2 ++ ")"
emitCall scope name [e1] | name `elem` infixFunctions = 	emitExpression scope e1
emitCall scope name (e:rest) | name `elem` infixFunctions =
	"(" ++ emitExpression scope e ++ " " ++ name ++ " " ++ emitCall scope name rest ++ ")"
emitCall scope name params =
	name ++ "(" ++ addDelimiter "," (map (emitExpression scope) params) ++ ")"

emitExpression :: Scope -> Expression -> String
emitExpression scope (ExprVar s) = s
emitExpression scope (ExprType s) = s
emitExpression scope (ExprInt i) = show i
emitExpression scope (ExprString str) = "\"" ++ str ++ "\""
emitExpression scope (ExprCall name params) = emitCall scope name params
{- Tuple at -}
emitExpression scope (ExprAt (ExprInt index) expr) =
	case exprType (scopeDeclarations scope) expr of
		TTuple _ -> emitExpression scope expr ++ "." ++ emitTupleMember index
		TPointer (TTuple _) -> emitExpression scope expr ++ "->" ++ emitTupleMember index
		t -> error $ "Unsuported type in emitExpression (ExprAt): " ++ show index ++ " " ++ show expr ++ "/" ++ show t

emitExpression scope (ExprAt (ExprString index) expr) =
	case exprType (scopeDeclarations scope) expr of
		TStruct _ _ -> emitExpression scope expr ++ "." ++ index
		TPointer _ -> emitExpression scope expr ++ "->" ++ index
		x -> error $ "Unsuported type in emitExpression (ExprAt): " ++ show index

emitExpression scope (ExprAddr expr) = "&(" ++ emitExpression scope expr ++ ")"
emitExpression scope (ExprDeref expr) = "*(" ++ emitExpression scope expr ++ ")"
emitExpression scope x = error $ "EmitExpression: " ++ show x

emitVarDeclarations :: Scope -> [VarDeclaration] -> SourceCode
emitVarDeclarations scope vdecls =
	joinMap declare vdecls
	where
		declare (name, t) = Text (typeString t ++ " " ++ name ++ ";") <+> Eol

emitInstruction :: Scope -> Instruction -> SourceCode
emitInstruction scope (IExpr expr) = Text (emitExpression scope expr ++ ";") <+> Eol
emitInstruction scope (ISet expr1 expr2) =
	Text (emitExpression scope expr1 ++ " = " ++ emitExpression scope expr2 ++ ";") <+> Eol
emitInstruction scope (IStatement decls instructions) =
	Text "{" <+> Block (declarationsCode <+> instructionsCode) <+> Text "}" <+> Eol
	where
		instructionsCode = joinMap (emitInstruction newScope) instructions
		newScope = addDeclarations scope (statementDeclarations decls instructions)
		declarationsCode = emitVarDeclarations newScope decls
emitInstruction scope (IDefine name t (Just expr)) = 
	Text (typeString t ++ " " ++ name ++ " = " ++ emitExpression scope expr ++ ";") <+> Eol
emitInstruction scope (IDefine name t Nothing) = 
	Text (typeString t ++ " " ++ name ++ ";") <+> Eol
emitInstruction scope (IReturn expr) = Text ("return " ++ emitExpression scope expr ++ ";") <+> Eol
emitInstruction scope IContinue = Text "continue;" <+> Eol
emitInstruction scope INoop = Empty
emitInstruction scope (IForeach var counterVar expr body) =
	Text ("for (" ++ varDecl ++ "; " ++ cycleTest ++ "; " ++ counterVar ++ "++) {")
	<+> Block (setVar <+> joinMap emit body) <+> Text "}" <+> Eol
	where
		elementType = case exprType (scopeDeclarations scope) expr  of
						TArray t -> t
						_ -> error "Unsuported type for IForeach"
		arrayLen = "(" ++ emitExpression scope expr ++ ").size()"
		setVar = Text (typeString elementType ++ " " ++ var ++ " = " ++ emitExpression scope expr ++ "[" ++ counterVar ++ "];") <+> Eol
		varDecl = "size_t " ++ counterVar ++ " = 0"
		cycleTest = counterVar ++ " < " ++ arrayLen
		emit = emitInstruction
			(addDeclarations scope (declarationsFromVarList [ (counterVar, TInt), (var, elementType) ]))
emitInstruction scope (IInline str) = Text str <+> Eol
emitInstruction scope (IIf expr branch1 INoop) =
	Text ("if (" ++ emitExpression scope expr ++ ") ") <+> emitInstruction scope branch1
emitInstruction scope (IIf expr branch1 branch2) =
	Text ("if (" ++ emitExpression scope expr ++ ") ") <+> emitInstruction scope branch1 <+> Text "else " <+> emitInstruction scope branch2
emitTupleMember index = "t_" ++ show index

initialScope :: Function -> Scope
initialScope function = Scope { scopeDeclarations = decls }
	where decls = declarationsFromVarList $ parameters function ++ declarations function

typeString :: Type -> String
typeString (TArray t) = "std::vector<" ++ typeString t ++ ">"
typeString (TRaw d) = d
typeString (TPointer t) = typeString t ++ "*"
typeString (TStruct name _) = name
typeString TString = "std::string"
typeString (TData _ rawType _ _) = rawType
typeString x = typeSafeString x

-- |Converts type to string in way that string can be used as part of identifier
typeSafeString :: Type -> String
typeSafeString TVoid = "void"
typeSafeString TInt = "int"
typeSafeString TString = "string"
typeSafeString (TTuple ts) = "Tuple" ++ show (length ts) ++ "_" ++ addDelimiter "_" (map typeSafeString ts)
typeSafeString TUndefined = "<undefined>"
typeSafeString (TArray t) = "Array_" ++ typeSafeString t
typeSafeString (TRaw d) = d
typeSafeString (TPointer t) = "Ptr_" ++ typeSafeString t
typeSafeString (TStruct name _) = name
typeSafeString (TData name _ _ _) = name
typeSafeString x = error $ "typeSafeString: " ++ show x

declareVar :: Scope -> String -> String
declareVar scope varName =
	case varType (scopeDeclarations scope) varName of
		TUndefined -> error "Undefined variable cannot be defined"
		t -> typeString t ++ " " ++ varName

declareVar' :: [VarDeclaration] -> [String]
declareVar' [] = []
declareVar' ((name,t):vs) = (typeString t ++ " " ++ name) : declareVar' vs

declareParam :: [VarDeclaration] -> [String]
declareParam [] = []
declareParam ((name,(TPointer t)):vs) = (typeString t ++ "* " ++ name) : declareParam vs
declareParam ((name,(TRaw d)):vs) = (d ++ " " ++ name) : declareParam vs
declareParam ((name,t):vs) = (typeString t ++ " & " ++ name) : declareParam vs

emitFunction :: Function -> String
emitFunction function =
	typeString (returnType function) ++ " " ++ functionName function ++ "(" ++ paramString ++ ")\n" ++ body
	where
		decls = makeDeclarations (statementVarList (declarations function) (instructions function)) stdFunctions
		body = sourceCodeToStr(emitInstruction scope (addConstructors decls statement))
		scope = initialScope function
		paramString = addDelimiter "," $ declareParam (parameters function)
		statement = makeStatement (declarations function) (instructions function ++ extraInstructions)
		extraInstructions =
			case extraCode function of
				"" -> [INoop]
				x -> [IInline x]

emitGlobals :: [VarDeclaration] -> String
emitGlobals [] = ""
emitGlobals ((name, t):rest) =
	typeString t ++ " " ++ name ++ ";\n" ++ emitGlobals rest

orderTypeByDepedancy :: (Set.Set Type) -> [Type]
orderTypeByDepedancy types = 
	dependacyOrder orderFn types
	where 
		orderFn ordered t = Set.empty == Set.difference (subTypes t) (Set.fromList ordered)

emitProgram :: String -> [VarDeclaration] -> [Function] -> String
emitProgram prologue globals functions =
	prologue ++ typeDeclarations ++ "\n\n" ++ emitGlobals globals ++ "\n\n" ++ concatMap emitFunction (extraFunctions ++ functions)
	where
		typeDeclarations = concatMap emitTypeDeclaration orderedTypes
		allTypes = Set.fold (\t s -> Set.union s $ subTypes' t) Set.empty (Set.unions (map gatherTypes functions))
		allDefinedTypes = Set.filter isDefined allTypes
		orderedTypes = orderTypeByDepedancy allDefinedTypes
		extraFunctions = [] {- map printFunction orderedTypes -}

emitTypeDeclaration :: Type -> String
emitTypeDeclaration (TTuple types) =
	{- FIXME: use TStruct emit -}
	"struct " ++ typeName ++ " {\n" ++ constructor1 ++ "\n" ++ constructor2 ++ innerPart types 0 ++ "};\n"
	where
		typeName = typeSafeString (TTuple types)
		decls = zip (map (\x -> "t_" ++ show x) [0..]) types
		innerPart [] _ = ""
		innerPart (t:ts) level = "\t" ++ typeString t ++ " t_" ++ show level ++ ";\n" ++ innerPart ts (level + 1)
		constructor1 = "\t" ++ typeName ++ "() {}"
		const t = case t of
			(TData _ _ _ _) -> ""
			(TPointer _) -> ""
			_ -> "const "
		constructor2 = "\t" ++ typeName ++ "(" ++ addDelimiter "," (map (\(n,t) -> const t ++ typeString t ++ " & " ++ n) decls) ++ ") {\n"
			++ concatMap (\(n,t) -> "\t\tthis->" ++ n ++ " = " ++ n ++ ";\n") decls ++ "\t}\n"

emitTypeDeclaration (TStruct name decls) =
	"struct " ++ name ++ " {\n" ++ innerPart decls ++ "};\n"
	where
		innerPart [] = ""
		innerPart ((name, t):ts) = "\t" ++ typeString t ++ " " ++ name ++ ";\n" ++ innerPart ts
emitTypeDeclaration _ = ""

varDeclarationTypes :: [VarDeclaration] -> TypeSet
varDeclarationTypes decls = Set.fromList [ t | (n,t) <- decls ]

gatherExprTypes :: Expression -> TypeSet
gatherExprTypes x =
	case exprType emptyDeclarations x of -- FIXME: not emptyDeclarations
		TUndefined -> Set.empty
		t -> Set.singleton t

gatherInstructionTypes :: Instruction -> TypeSet
gatherInstructionTypes (IExpr expr) = gatherExprTypes expr
gatherInstructionTypes (ISet _ expr) = gatherExprTypes expr
gatherInstructionTypes (IIf expr i1 i2) =
	Set.unions [ gatherExprTypes expr, gatherInstructionTypes i1, gatherInstructionTypes i2 ]
gatherInstructionTypes (IStatement decls instrs) =
	Set.union (varDeclarationTypes decls) $ Set.unions (map gatherInstructionTypes instrs)
gatherInstructionTypes (IForeach _ _ expr instrs) =
	Set.union (gatherExprTypes expr) $ Set.unions (map gatherInstructionTypes instrs)
gatherInstructionTypes (IDefine _ t (Just expr)) = Set.union (Set.singleton t) (gatherExprTypes expr)
gatherInstructionTypes (IDefine _ t Nothing) = Set.singleton t
gatherInstructionTypes INoop = Set.empty
gatherInstructionTypes _ = Set.empty

gatherTypes :: Function -> TypeSet
gatherTypes f =
	Set.unions $ [ varDeclarationTypes (declarations f), varDeclarationTypes (parameters f)] ++
		map gatherInstructionTypes (instructions f)

{-
	subTypes' x returns subtypes with x
 	subTypes x returns subtypes without x
-}

subTypes :: Type -> TypeSet
subTypes (TArray t) = subTypes' t
subTypes (TTuple types) = Set.unions $ map subTypes' types
subTypes (TPointer t) = subTypes' t
subTypes (TStruct _ decls) = Set.unions $ map (subTypes'.snd) decls
subTypes _ = Set.empty

subTypes' :: Type -> TypeSet
subTypes' tt = Set.union (Set.singleton tt) (case tt of
	(TArray t) -> subTypes' t
	(TTuple types) -> Set.unions $ map subTypes' types
	(TPointer t) -> subTypes' t
	(TStruct name decls) -> Set.unions $ map (subTypes'.snd) decls 
	_ -> Set.empty)

printFunctionName :: Type -> String
printFunctionName t = "print_" ++ typeSafeString t

printFunctionHelper :: Type -> [Instruction] -> Function
printFunctionHelper t instructions =
	Function {
			functionName = printFunctionName t,
			returnType = TVoid,
			declarations = [],
			parameters = [ ("x", t) ],
			extraCode = "",
			instructions = instructions
	}

callPrintfStr :: String -> Instruction
callPrintfStr str =
	IExpr (ExprCall "printf" [ ExprString str ])

callPrint :: Type -> Expression -> Instruction
callPrint t expr =
	IExpr (ExprCall (printFunctionName t) [ expr ])

printFunction :: Type -> Function
printFunction TInt = printFunctionHelper TInt [ IExpr (ExprCall "printf" [ ExprString "%i", ExprVar "x" ]) ]
printFunction (TTuple types) =
	printFunctionHelper (TTuple types) $ (callPrintfStr "(":instructions) ++ [ callPrintfStr ")" ]
	where
		instructions = List.intersperse delimInstr $ countedMap printInstr types
		printInstr n t = IExpr (ExprCall (printFunctionName t) [ ExprAt (ExprInt n) (ExprVar "x") ])
		delimInstr = callPrintfStr ","
printFunction (TArray t) =
	printFunctionHelper (TArray t) [ callPrintfStr "[", cycle, callPrintfStr "]" ]
	where
		cycle = IForeach "y" "i" (ExprVar "x") [
			IExpr (ExprCall (printFunctionName t) [ (ExprVar "y") ]),
			callPrintfStr "," ]

printFunction x = printFunctionHelper x []

addConstructors :: Declarations -> Instruction -> Instruction
addConstructors decls i =
	mapExprs' addConstr decls i
	where
       addConstr decls (ExprTuple xs) =
			ExprCall (typeSafeString (exprType decls (ExprTuple xs))) $ map (addConstr decls) xs
       addConstr decls (ExprCall name exprs) = ExprCall name $ map (addConstr decls) exprs
       addConstr decls (ExprAt a b) = ExprAt (addConstr decls a) (addConstr decls b)
       addConstr decls (ExprDeref e) = ExprDeref $ addConstr decls e
       addConstr decls (ExprAddr e) = ExprAddr $ addConstr decls e
       addConstr decls x = x

exprAsString :: Declarations -> Expression -> Expression
exprAsString decls (ExprString x) = ExprString x
exprAsString decls (ExprInt x) = ExprString $ show x
exprAsString decls x =
	case exprType decls x of
		TInt -> ExprCall "ca_int_to_string" [x]
		TString -> x
		TTuple [] -> ExprString "()"
		(TData name _ _ functions) | hasKey "getstring" functions -> ExprCall (name ++ "_getstring") [x]
		(TData name _ _ _) -> ExprCall "std::string" [ ExprString name ]
		TTuple types -> ExprCall "+" $ [ ExprCall "std::string"
			[ ExprString "(" ], ExprCall "Base.asString" [ ExprAt (ExprInt 0) x ]]
				++ concat [ [ExprString ",", ExprCall "Base.asString"
					[ ExprAt (ExprInt i) x ]] | i <- [1..length types-1]] ++ [ ExprString ")" ]
		x -> error $ "exprAsString: " ++ show x


