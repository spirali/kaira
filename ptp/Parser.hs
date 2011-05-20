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

module Parser (
	parseType,
	parseExpr,
	parseExpr',
	parseEdgeInscription,
	parseGuard,
	parseParameters,
	parseInitExpr
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Declarations
import ProjectTools

languageDef = emptyDef { Token.commentStart    = "/*"
	, Token.commentEnd      = "*/"
	, Token.identStart      = letter
	, Token.identLetter     = alphaNum <|> char '_'
	, Token.reservedNames   = []
	, Token.caseSensitive   = True
	, Token.reservedOpNames = [ "+", "-", "*", "//", "%", "==", "!="
	, "<", ">", "<=", ">=", "&&", "||" ]
}
lexer = Token.makeTokenParser languageDef

parens = Token.parens lexer
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
integer = do { i <- Token.integer lexer; return (fromInteger i) }
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer
semi = Token.semi lexer

intParser = do
	i <- integer
	return (ExprInt i)

stringParserHelper :: String -> Parser String
stringParserHelper str = do
 	s <- many (noneOf "\\\"\n\r")
	do { char '"'; return (str ++ s) } <|> do {
		char '\\'; c <- anyChar; stringParserHelper (str ++ "\\" ++ [c])
	} <?> "quote"

stringParser = do
	char '"'
	s <- stringParserHelper ""
	whiteSpace
	return (ExprString s)

identifierParser = do
	ds <- identifier
	do {
		exprs <- parenParamsParser;
		return (ExprCall ds exprs)
	} <|> return (ExprVar ds)

parameterParser = do
	char '#'
	ls <- identifier
	return (ExprParam ls)

parenParamsParser = do
	exprs <- parens (sepBy expressionParser comma)
	return exprs

tupleParser = do
	exprs <- parenParamsParser
	case exprs of
		[x] -> return x
		_ -> return (ExprTuple exprs)

pathItems = sepBy expressionParser (char '/')

pathParser = do { char '/'; items <- pathItems; return (AbsPath items) }
	<|> do { items <- pathItems; return (RelPath 0 items) }

expressionParser :: Parser NelExpression
expressionParser = buildExpressionParser optable baseExpr
optable = [
	[ Infix (opBinary "%") AssocLeft ],
	[ Infix (opBinary "*") AssocLeft ],
	[ Infix (opBinary "//") AssocLeft ],
	[ Infix (opBinary "+") AssocLeft ],
	[ Infix (opBinary "-") AssocLeft ],
	[ Infix (opBinary ">") AssocLeft ],
	[ Infix (opBinary "<") AssocLeft ],
	[ Infix (opBinary "==") AssocLeft ],
	[ Infix (opBinary "!=") AssocLeft ],
	[ Infix (opBinary ">=") AssocLeft ],
	[ Infix (opBinary "<=") AssocLeft ],
	[ Infix (opBinary "||") AssocLeft ],
	[ Infix (opBinary "&&") AssocLeft ]]
baseExpr    = intParser <|> stringParser <|> parameterParser <|> identifierParser <|> tupleParser
opBinary name   = reservedOp name >> return (\x y -> (ExprCall name) [x, y])

concreteTypeParser :: String -> TypeTable -> Parser NelType
concreteTypeParser source typeNames = do
	str <- identifier
	case Map.lookup str typeNames of
		Just x -> return x
		Nothing -> error $ source ++ ":1:Invalid type name"

tupleTypeParser :: String -> TypeTable -> Parser NelType
tupleTypeParser source typeNames = do
	exprs <- parens $ sepBy1 (typeParser source typeNames) comma
	return (TypeTuple exprs)

typeParser :: String -> TypeTable -> Parser NelType
typeParser source typeNames = do
	tupleTypeParser source typeNames <|> concreteTypeParser source typeNames

parametersParser :: String -> TypeTable -> Parser [NelVarDeclaration]
parametersParser source typeNames =
	do {eof; return []} <|> do {
		t <- typeParser source typeNames;
		i <- identifier;
		rest <- do {eof; return []} <|> do { char ',' >> whiteSpace >> parametersParser source typeNames };
		return $ (i, t):rest
	}

edgePackingParser :: Parser (String, Maybe NelExpression)
edgePackingParser = do
	char '~'
	s <- identifier
	do { x <- (parens expressionParser); return (s, Just x) } <|> return (s, Nothing)

edgeInscriptionParser :: Parser (EdgeInscription, Path)
edgeInscriptionParser = do
	mainExpr <- do { (name, limit) <- edgePackingParser; return (EdgePacking name limit) } <|> (expressionParser >>= (return . EdgeExpression))
	path <- do { char '@'; path <- pathParser; return path } <|> do { return $ RelPath 0 []; }
	return (mainExpr, path)

guardParser :: Parser NelExpression
guardParser = do { eof; return ExprTrue; } <|> expressionParser

initExprParser :: Parser [NelExpression]
initExprParser = sepEndBy expressionParser semi

strErrorMessage :: ParseError -> String
strErrorMessage perror =
	unlines $ map (\line -> prefix ++ line) $ filter ((/=) "") $ lines message
	where
		pos = errorPos perror
		prefix = (sourceName pos) ++ ":" ++ show (sourceLine pos) ++ ":"
		message = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" (errorMessages perror)

parseHelper :: Parser a -> String -> String -> a
parseHelper parser source str =
	case parse (do { whiteSpace; x <- parser; eof; return x }) source str of
		Left x -> error $ strErrorMessage x
		Right x -> x

parseType :: TypeTable -> String -> String -> NelType
parseType typeNames source "" = error $ source ++ ":1:Type is empty"
parseType typeNames source str =
	parseHelper (typeParser source typeNames) source str

parseExpr :: String -> String -> NelExpression
parseExpr source "" = error $ source ++ ":1:Expression is empty"
parseExpr source str = parseHelper expressionParser source str

parseExpr' :: String -> String -> Maybe NelExpression
parseExpr' source "" = Nothing
parseExpr' source x = Just $ parseExpr source x

parseEdgeInscription :: String -> String -> (EdgeInscription, Path)
parseEdgeInscription source "" = error $ source ++ ":1:Inscription is empty"
parseEdgeInscription source str = parseHelper edgeInscriptionParser source str

parseGuard :: String -> String -> NelExpression
parseGuard = parseHelper guardParser

parseInitExpr :: String -> String -> [NelExpression]
parseInitExpr = parseHelper initExprParser

-- |Parses "Int a, String b" as [("a", TInt), ("b", TString)]
parseParameters :: TypeTable -> String -> String -> [NelVarDeclaration]
parseParameters typeNames source str = parseHelper (parametersParser source typeNames) source str
