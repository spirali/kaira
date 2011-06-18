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

module Codegen (
	emitProgram,
	typeString,
	typeSafeString,
	orderTypeByDepedancy -- for Tests.hs
) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Declarations
import CodegenTypes
import Utils
import ProjectTools
import CodegenTools

data SourceCode = Text String
	| Join SourceCode SourceCode
	| Block SourceCode
	| Eol
	| LineDirective (Maybe (String, Int)) -- Nothing means "return to original source file"
	| Empty

(<+>) :: SourceCode -> SourceCode -> SourceCode
a <+> Empty = a
Empty <+> b = b
a <+> b = Join a b

indentStr :: Int -> String
indentStr n = List.replicate n '\t'

joinMap :: (a -> SourceCode) -> [a] -> SourceCode
joinMap f = foldr ((<+>) . f) Empty

infixFunctions = [ "+", "-", "/", "*", "!=", "==", "||", "&&", ">=", "<=", ">", "<", "%" ]

emitCall ('.':name) (obj:params) =
	emitExpression obj ++ "." ++ name ++ "(" ++ addDelimiter "," (map emitExpression params) ++ ")"
emitCall ('-':'>':name) (obj:params) =
	emitExpression obj ++ "->" ++ name ++ "(" ++ addDelimiter "," (map emitExpression params) ++ ")"
emitCall name [e1] | name `elem` infixFunctions = 	emitExpression e1
emitCall name (e:rest) | name `elem` infixFunctions =
	"(" ++ emitExpression e ++ " " ++ name ++ " " ++ emitCall name rest ++ ")"
emitCall name params =
	name ++ "(" ++ addDelimiter "," (map emitExpression params) ++ ")"

emitExpression :: Expression -> String
emitExpression (EVar s) = s
emitExpression (EType t) = typeString t
emitExpression (EInt i) = show i
emitExpression (EString str) = "\"" ++ escapeString str ++ "\""
emitExpression (ECall name params) = emitCall name params
emitExpression ENull = "NULL"
emitExpression (ECast e t) = "((" ++ typeString t ++ ") " ++ emitExpression e ++ ")"
emitExpression (EMember name expr) = emitExpression expr ++ "." ++ name
emitExpression (EMemberPtr name expr) = emitExpression expr ++ "->" ++ name

emitExpression (EAddr expr) = "&(" ++ emitExpression expr ++ ")"
emitExpression (EDeref expr) = "(*(" ++ emitExpression expr ++ "))"
emitExpression (ENew expr) = "new " ++ emitExpression expr
emitExpression x = error $ "EmitExpression: " ++ show x

emitInstruction :: Instruction -> SourceCode
emitInstruction (IExpr expr) = Text (emitExpression expr ++ ";") <+> Eol
emitInstruction (ISet expr1 expr2) =
	Text (emitExpression expr1 ++ " = " ++ emitExpression expr2 ++ ";") <+> Eol
emitInstruction (IStatement instructions) =
	Text "{" <+> Block instructionsCode <+> Text "}" <+> Eol
	where
		instructionsCode = joinMap emitInstruction instructions

emitInstruction (IDefine name t (Just expr)) =
	Text (typeString t ++ " " ++ name ++ " = " ++ emitExpression expr ++ ";") <+> Eol
emitInstruction (IDefine name t Nothing) =
	Text (typeString t ++ " " ++ name ++ ";") <+> Eol
emitInstruction (IReturn expr) = Text ("return " ++ emitExpression expr ++ ";") <+> Eol
emitInstruction IContinue = Text "continue;" <+> Eol
emitInstruction INoop = Empty
emitInstruction (IForeach elementType var source body) =
	Text ("for (" ++ varDecl ++ "; " ++ cycleTest ++ "; ++" ++ var ++ ") {")
	<+> Block (joinMap emitInstruction body) <+> Text "}" <+> Eol
	where
		varDecl = "std::vector<" ++ typeString elementType ++ ">::iterator " ++ var ++ " = " ++ emitExpression source ++ ".begin()"
		cycleTest = var ++ " != " ++ emitExpression source ++ ".end()"

emitInstruction (IWhile expr i) = Text ("while (" ++ emitExpression expr ++ ")") <+> Block (emitInstruction i)
emitInstruction (IDo expr i) = Text "do " <+> emitInstruction i  <+> Text ("while (" ++ emitExpression expr ++ ");")
emitInstruction (IInline str) = Text str <+> Eol
emitInstruction (IIf expr branch1 INoop) =
	Text ("if (" ++ emitExpression expr ++ ") ") <+> emitInstruction branch1
emitInstruction (IIf expr branch1 branch2) =
	Text ("if (" ++ emitExpression expr ++ ") ") <+> emitInstruction branch1 <+> Text "else " <+> emitInstruction branch2
emitTupleMember index = "t_" ++ show index

declareParam :: [ParamDeclaration] -> [String]
declareParam [] = []
declareParam ((name,t, ParamRef):vs) = (typeString t ++ " & " ++ name) : declareParam vs
declareParam ((name,t, ParamConst):vs) = ("const " ++ typeString t ++ " & " ++ name) : declareParam vs
declareParam ((name,t, ParamNormal):vs) = (typeString t ++ " " ++ name) : declareParam vs

emitFunction :: Function -> SourceCode
emitFunction function =
	prefix <+> Text functionDeclaration <+> Eol <+> body <+> suffix
	where
		functionDeclaration = declareFunction function ++ iCall
		body = emitInstruction $ IStatement (instructions function ++ extraInstructions)
		(prefix, suffix) = case functionSource function of
				Just x -> (LineDirective (Just x), LineDirective Nothing)
				Nothing -> (Empty, Empty)
		extraInstructions = case extraCode function of
				"" -> [INoop]
				x -> [IInline x]
		iCall = case initCalls function of
			[] -> ""
			xs -> " : " ++ (addDelimiter "," $ map emitExpression xs)

declareFunction :: Function -> String
declareFunction function = typeString (returnType function) ++ " "
			++ functionName function ++ "(" ++ paramString ++ ")"
	where paramString = addDelimiter "," $ declareParam (parameters function)

emitGlobals :: [VarDeclaration] -> String
emitGlobals [] = ""
emitGlobals ((name, t):rest) =
	typeString t ++ " " ++ name ++ ";\n" ++ emitGlobals rest

orderTypeByDepedancy :: (Set.Set Type) -> [Type]
orderTypeByDepedancy types =
	dependacyOrder orderFn types
	where
		orderFn ordered t = Set.empty == Set.difference (subTypes t) (Set.fromList ordered)

emitProgram :: String -> String -> TypeSet -> [VarDeclaration] -> [Function] -> String
emitProgram fileName prologue types globals functions =
	sourceCodeToStr fileName $
	Text prologue <+> typedefs <+> funDecls <+> typeDeclarations <+> Text (emitGlobals globals) <+> Eol <+> joinMap emitFunction functions
	where
		typeDeclarations = Text $ concatMap emitTypeDeclaration orderedTypes
		allTypes = Set.fold (\t s -> Set.union s $ subTypes' t) Set.empty types
		orderedTypes = orderTypeByDepedancy allTypes
		funDecls = joinMap (\f -> Text $ declareFunction f ++ ";\n") functions
		typedefs = joinMap Text $ Maybe.mapMaybe typedef $ Set.toList allTypes

typedef :: Type -> Maybe String
typedef (TClass name _ _ _) = Just $ "class " ++ name ++ ";\n"
typedef _ = Nothing

emitTypeDeclaration :: Type -> String
emitTypeDeclaration (TClass name ancestor methods attributes) =
	"class " ++ name ++ inheritance ++ " {\n\tpublic:\n" ++ methodsCode ++ attrCode attributes ++ "};\n"
	where
		attrCode [] = ""
		attrCode ((name, t):ts) = "\t" ++ typeString t ++ " " ++ name ++ ";\n" ++ attrCode ts
		inheritance = case ancestor of
			Nothing -> ""
			Just x -> " : public " ++ x
		methodsCode = sourceCodeToStr "" $ Block $ joinMap emitFunction methods

emitTypeDeclaration _ = ""

{-
	subTypes' x returns subtypes with x
 	subTypes x returns subtypes without x
-}

subTypes :: Type -> TypeSet
subTypes (TArray t) = subTypes' t
subTypes (TTemplate t1 t2) = Set.union (subTypes' t1) (subTypes' t2)
subTypes (TPointer t) = subTypes' t
subTypes (TClass name _ _ decls) = Set.unions $ map (subTypes'.snd) decls
subTypes _ = Set.empty

subTypes' :: Type -> TypeSet
subTypes' tt = Set.union (Set.singleton tt) (case tt of
	(TArray t) -> subTypes' t
	(TTemplate t1 t2) -> Set.union (subTypes' t1) (subTypes' t2)
	(TPointer t) -> subTypes' t
	(TClass name _ _ decls) -> Set.unions $ map (subTypes'.snd) decls
	_ -> Set.empty)

sourceCodeToStr :: String -> SourceCode -> String
sourceCodeToStr originalFilename code =
	let (str, l) = toStr code 0 True 1 in str
	where
		toStr :: SourceCode -> Int -> Bool -> Int -> (String, Int)
		toStr (Join a b) n i lineno =
			let (str1, l1) = toStr a n i lineno in
			let	(str2, l2) = toStr b n (endWithEol a) l1 in
					(str1 ++ str2, l2)
		toStr (Text s) n True lineno = (indentStr n ++ s, lineno + countOfEols s)
		toStr (Text s) _ False lineno = (s, lineno + countOfEols s)
		toStr (Block c) n i lineno = let (str, l) = toStr c (n + 1) True (lineno + 1) in
			("\n" ++ str ++ end, l + lineno')
			where (end, lineno') = if endWithEol c then ("",0) else ("\n", 1)
		toStr Eol _ _ lineno = ("\n", lineno + 1)
		toStr (LineDirective (Just (file, lnum))) _ _ lineno =
			("#line " ++ show lnum ++ " \"" ++ file ++ "\"\n", lineno + 1)
		toStr (LineDirective Nothing) _ _ lineno =
			("#line " ++ show (lineno + 1) ++ " \"" ++ originalFilename ++ "\"\n", lineno + 1)
		toStr Empty _ _ lineno = ("",lineno)

		endWithEol Eol = True
		endWithEol (Text str) = "\n" `List.isSuffixOf` str
		endWithEol (Block _) = True
		endWithEol (LineDirective _) = True
		endWithEol (Join a b) = endWithEol b
		endWithEol _ = False

		countOfEols [] = 0
		countOfEols ('\n':xs) = 1 + countOfEols xs
		countOfEols (_:xs) = countOfEols xs
