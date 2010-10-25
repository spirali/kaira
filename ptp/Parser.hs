module Parser (
	parseType,
	parseExpr,
	parseExpr',
	parseEdgeInscription,
	parseGuard,
	parseParameters
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
	, Token.commentLine     = "//"
	, Token.identStart      = letter
	, Token.identLetter     = alphaNum <|> char '_'
	, Token.reservedNames   = []
	, Token.caseSensitive   = True
	, Token.reservedOpNames = ["+", "-", "*", "/", "==", "!="
	, "<", ">", "<=", ">=", "&&", "||" ]
}
lexer = Token.makeTokenParser languageDef

parens = Token.parens lexer
identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
integer = do { i <- Token.integer lexer; return (fromInteger i) }
whiteSpace = Token.whiteSpace lexer
comma = Token.comma lexer

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

expressionParser :: Parser NelExpression
expressionParser = buildExpressionParser optable baseExpr
optable = [
	[ Infix (opBinary "*") AssocLeft ],
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

edgeInscriptionParser :: Parser EdgeInscription
edgeInscriptionParser =
	do { (name, limit) <- edgePackingParser; return (EdgePacking name limit) } <|> (expressionParser >>= (return . EdgeExpression))

guardParser :: Parser NelExpression
guardParser = do { eof; return ExprTrue; } <|> expressionParser

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

parseEdgeInscription :: String -> String -> EdgeInscription
parseEdgeInscription source "" = error $ source ++ ":1:Inscription is empty"
parseEdgeInscription source str = parseHelper edgeInscriptionParser source str

parseGuard :: String -> String -> NelExpression
parseGuard = parseHelper guardParser

-- |Parses "Int a, String b" as [("a", TInt), ("b", TString)]
parseParameters :: TypeTable -> String -> String -> [NelVarDeclaration]
parseParameters typeNames source str = parseHelper (parametersParser source typeNames) source str
