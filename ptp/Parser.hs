module Parser (
	parseType,
	parseExpr,
	parseExpr',
	parseEdgeInscription,
	parseGuard,
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
	, Token.identLetter     = alphaNum
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

expressionParser :: Parser Expression
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

concreteTypeParser :: TypeNames -> Parser Type
concreteTypeParser typeNames = do
	str <- identifier
	case Map.lookup str typeNames of
		Just x -> return x
		Nothing -> return TUndefined

tupleTypeParser :: TypeNames -> Parser Type
tupleTypeParser typeNames = do
	exprs <- parens $ sepBy1 (typeParser typeNames) comma
	return (TTuple exprs)

typeParser :: TypeNames -> Parser Type
typeParser typeNames = do
	tupleTypeParser typeNames <|> concreteTypeParser typeNames

edgePackingParser :: Parser (String, Maybe Expression)
edgePackingParser = do
	char '~'
	s <- identifier 
	do { x <- (parens expressionParser); return (s, Just x) } <|> return (s, Nothing)

edgeInscriptionParser :: Parser EdgeInscription
edgeInscriptionParser = 
	do { (name, limit) <- edgePackingParser; return (EdgePacking name limit) } <|> (expressionParser >>= (return . EdgeExpression))

guardParser :: Parser Expression 
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
	case parse parser source str of
		Left x -> error $ strErrorMessage x
		Right x -> x

parseType :: TypeNames -> String -> String -> Type
parseType typeNames source "" = error $ source ++ ":1:Type is empty"
parseType typeNames source str = 
	let t = parseHelper (whiteSpace >> typeParser typeNames) source str in 
		if isUndefined t then error $ source ++ ":1:Invalid type" else t

parseExpr :: String -> String -> Expression
parseExpr source "" = error $ source ++ ":1:Expression is empty"
parseExpr source str = parseHelper (whiteSpace >> expressionParser) source str

parseExpr' :: String -> String -> Maybe Expression
parseExpr' source "" = Nothing
parseExpr' source x = Just $ parseExpr source x

parseEdgeInscription :: String -> String -> EdgeInscription 
parseEdgeInscription source "" = error $ source ++ ":1:Inscription is empty"
parseEdgeInscription source str = parseHelper (whiteSpace >> edgeInscriptionParser) source str

parseGuard :: String -> String -> Expression
parseGuard = parseHelper (whiteSpace >> guardParser)
