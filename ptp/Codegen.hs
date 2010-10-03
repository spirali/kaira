
module Codegen (
	emitProgram,
	callPrint,
	callPrintfStr,
	typeString,
	typeSafeString,
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
		toStr (Text s) n True = (indentStr n) ++ s
		toStr (Text s) _ False = s
		toStr (Block c) n i = "\n" ++ toStr c (n + 1) True ++ if (endWithEol c) then "" else "\n"
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
joinMap f [] = Empty 
joinMap f (a:rest) = f a <+> joinMap f rest

infixFunctions = [ "+", "-", "/", "*", "!=", "==", "||", "&&", ">=", "<=", ">", "<" ]

addDeclarations :: Scope -> Declarations -> Scope
addDeclarations scope decls = 
	Scope $ decls `declarationsJoin` (scopeDeclarations scope)

emitCall scope ('.':name) (obj:params) = 
	emitExpression scope obj ++ dotOrArrow ++ name ++ "(" ++ (addDelimiter "," $ map (emitExpression scope) params) ++ ")"
	where dotOrArrow = 
		case exprType (scopeDeclarations scope) obj of
			TStruct _ _ -> "."
			TPointer _ -> "->"
			TData _ -> "."
			_ -> error $ "Invalid type for calling method " ++ show obj

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
	name ++ "(" ++ (addDelimiter "," $ map (emitExpression scope) params) ++ ")"

emitExpression :: Scope -> Expression -> String
emitExpression scope (ExprVar s) = s
emitExpression scope (ExprInt i) = show i
emitExpression scope (ExprString str) = "\"" ++ str ++ "\""
emitExpression scope (ExprCall name params) = emitCall scope name params
{- Tuple at -}
emitExpression scope (ExprAt (ExprInt index) expr) = 
	case exprType (scopeDeclarations scope) expr of
		TTuple _ -> emitExpression scope expr ++ "." ++ emitTupleMember index
		TPointer (TTuple _) -> emitExpression scope expr ++ "->" ++ emitTupleMember index
		_ -> error $ "Unsuported type in emitExpression (ExprAt): " ++ show index

emitExpression scope (ExprAt (ExprString index) expr) = 
	case exprType (scopeDeclarations scope) expr of
		TStruct _ _ -> emitExpression scope expr ++ "." ++ index
		TPointer _ -> emitExpression scope expr ++ "->" ++ index
		x -> error $ "Unsuported type in emitExpression (ExprAt): " ++ show index

emitExpression scope (ExprAddr expr) = "&(" ++ emitExpression scope expr ++ ")"
emitExpression scope (ExprDeref expr) = "*(" ++ emitExpression scope expr ++ ")"
emitExpression scope x = error $ "EmitExpression: " ++ (show x)

emitVarDeclarations :: Scope -> [VarDeclaration] -> SourceCode
emitVarDeclarations scope vdecls = 
	joinMap declare vdecls
	where declare (name, t) = Text (typeString t ++ " " ++ name ++ ";") <+> Eol

emitInstruction :: Scope -> Instruction -> SourceCode
emitInstruction scope (IExpr expr) = Text (emitExpression scope expr ++ ";") <+> Eol
emitInstruction scope (ISet expr1 expr2) = Text (emitExpression scope expr1 ++ " = " ++ emitExpression scope expr2 ++ ";") <+> Eol
emitInstruction scope (IStatement decls instructions) = 
	(Text "{") <+> Block (declarations <+> joinMap emit instructions) <+> (Text "}") <+> Eol
	where 
		emit i = emitInstruction (addDeclarations scope (declarationsFromVarList decls)) i
		declarations = emitVarDeclarations scope decls
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
		arrayLen = "(" ++ (emitExpression scope expr) ++ ").size()"
		setVar = Text ((typeString elementType) ++ " " ++ var ++ " = " ++ emitExpression scope expr ++ "[" ++ counterVar ++ "];") <+> Eol
		varDecl = "int " ++ counterVar ++ " = 0"
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
typeString (TData d) = d 
typeString (TPointer t) = typeString t ++ "*"
typeString (TStruct name _) = name
typeString x = typeSafeString x

typeSafeString :: Type -> String
typeSafeString TVoid = "void"
typeSafeString TInt = "int"
typeSafeString (TTuple ts) = "Tuple" ++ show (length ts) ++ "_" ++ addDelimiter "_" (map typeSafeString ts)
typeSafeString TUndefined = "<undefined>"
typeSafeString (TArray t) = "Array_" ++ typeSafeString t
typeSafeString (TData d) = d
typeSafeString (TPointer t) = "Ptr_" ++ typeSafeString t
typeSafeString (TStruct name _) = name
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
declareParam ((name,(TData d)):vs) = (d ++ " " ++ name) : declareParam vs
declareParam ((name,t):vs) = (typeString t ++ " & " ++ name) : declareParam vs

emitFunction :: Function -> String
emitFunction function = 
	typeString (returnType function) ++ " " ++ (functionName function) ++ "(" ++ paramString ++ ")\n" ++ body
	where 
		decls = makeDeclarations (declarations function) stdFunctions
		body = sourceCodeToStr(emitInstruction scope (addConstructors decls statement))
		scope = initialScope function
		paramString = addDelimiter "," $ declareParam (parameters function)
		statement = IStatement (declarations function) $ (instructions function) ++ extraInstructions
		extraInstructions = 
			case extraCode function of
				"" -> [INoop]
				x -> [IInline x]

emitGlobals :: [VarDeclaration] -> String
emitGlobals [] = ""
emitGlobals ((name, t):rest) = 
	typeString t ++ " " ++ name ++ ";\n" ++ emitGlobals rest

emitProgram :: String -> [VarDeclaration] -> [Function] -> String
emitProgram prologue globals functions = 
	prologue ++ typeDeclarations ++ "\n\n" ++ emitGlobals globals ++ "\n\n" ++ concatMap emitFunction (extraFunctions ++ functions) 
	where 
		typeDeclarations = concatMap emitTypeDeclaration orderedTypes
		allTypes = Set.fold (\t s -> Set.union s $ subTypes' t) Set.empty (Set.unions (map gatherTypes functions))
		allDefinedTypes = Set.filter isDefined allTypes
		orderedTypes = List.sortBy depOrd (Set.toList allDefinedTypes)
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
		constructor2 = "\t" ++ typeName ++ "(" ++ addDelimiter "," (map (\(n,t) -> "const " ++ typeString t ++ " & " ++ n) decls) ++ ") {\n" 
			++ concatMap (\(n,t) -> "\t\tthis->" ++ n ++ " = " ++ n ++ ";\n") decls ++ "\t}\n"

emitTypeDeclaration (TStruct name decls) = 
	"struct " ++ name ++ " {\n" ++ innerPart decls ++ "};\n"
	where 
		innerPart [] = ""
		innerPart ((name, t):ts) = "\t" ++ typeString t ++ " " ++ name ++ ";\n" ++ innerPart ts
emitTypeDeclaration _ = ""

varDeclarationTypes :: [VarDeclaration] -> TypeSet
varDeclarationTypes decls = Set.fromList $ [ t | (n,t) <- decls ]

gatherExprTypes :: Expression -> TypeSet
gatherExprTypes x = 
	if t == TUndefined then 
		Set.empty 
	else 
		Set.singleton t 
	where t = exprType (emptyDeclarations) x  {- FIXME -}

gatherInstructionTypes :: Instruction -> TypeSet
gatherInstructionTypes (IExpr expr) = gatherExprTypes expr
gatherInstructionTypes (ISet _ expr) = gatherExprTypes expr
gatherInstructionTypes (IIf expr i1 i2) = 
	Set.unions [ gatherExprTypes expr, gatherInstructionTypes i1, gatherInstructionTypes i2 ]
gatherInstructionTypes (IStatement decls instrs) = 
	Set.union (varDeclarationTypes decls) $ Set.unions (map gatherInstructionTypes instrs)
gatherInstructionTypes (IForeach _ _ expr instrs) = 
	Set.union (gatherExprTypes expr) $ Set.unions (map gatherInstructionTypes instrs)
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
subTypes (TTuple types) = (Set.unions $ map subTypes' types)
subTypes (TPointer t) = subTypes' t
subTypes (TStruct _ decls) = Set.unions $ map (subTypes'.snd) decls
subTypes _ = Set.empty

subTypes' :: Type -> TypeSet
subTypes' (TArray t) = Set.union (Set.singleton (TArray t)) (subTypes' t)
subTypes' (TTuple types) = Set.union (Set.singleton (TTuple types)) (Set.unions $ map subTypes' types)
subTypes' (TPointer t) = Set.union (Set.singleton (TPointer t)) (subTypes' t)
subTypes' (TStruct name decls) = Set.union (Set.singleton (TStruct name decls)) (Set.unions $ map (subTypes'.snd) decls)
subTypes' x = Set.singleton x

dependsOn :: Type -> Type -> Bool
x `dependsOn` y = Set.member y (subTypes x)
	
depOrd :: Type -> Type -> Ordering
x `depOrd` y
	| x == y = EQ
	| x `dependsOn` y = GT
	| otherwise = EQ

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
	printFunctionHelper (TArray t) $ [ callPrintfStr "[", cycle, callPrintfStr "]" ]
	where 
		cycle = IForeach "y" "i" (ExprVar "x") [
			IExpr (ExprCall (printFunctionName t) [ (ExprVar "y") ]),
			callPrintfStr "," ]

printFunction x = printFunctionHelper x []

addConstructors :: Declarations -> Instruction -> Instruction
addConstructors decls i =
	mapExprs' addConstr decls i
	where
       addConstr decls (ExprTuple xs) = ExprCall (typeSafeString (exprType decls (ExprTuple xs))) $ map (addConstr decls) xs
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
		TTuple [] -> ExprString "()"
		TTuple types -> ExprCall "+" $ [ ExprCall "std::string" [ ExprString "(" ], ExprCall "Base.asString" [ ExprAt (ExprInt 0) x ]]
			++ concat [ [ExprString ",", ExprCall "Base.asString" [ ExprAt (ExprInt i) x ]] | i <- [1..(length types)-1]] ++ [ ExprString ")" ]
		x -> error $ "exprAsString: " ++ show x


