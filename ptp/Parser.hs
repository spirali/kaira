module Parser (
	parseType,
	parseExpr,
	parseExpr',
	parseEdgeInscription,
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Declarations

skipSpaces = skipMany space

rawIntParser = do
	ds <- many1 digit
	skipSpaces
	return (read ds)

intParser = do
	i <- rawIntParser
	return (ExprInt i)

identifierParser = do
	ds <- many1 letter
	skipSpaces
	do {
		exprs <- parenParamsParser;
 		skipSpaces;
		return (ExprCall ds exprs)
	} <|> return (ExprVar ds)

parameterParser = do
	char '#'
	ls <- many1 letter
	skipSpaces
	return (ExprParam ls)

separatorParser = do 
	skipMany (char ',')
	skipSpaces

parenParamsParser = do
	exprs <- parens (sepBy expressionParser separatorParser)
	skipSpaces
	return exprs

tupleParser = do
	exprs <- parenParamsParser
	case exprs of
		[x] -> return x
		_ -> return (ExprTuple exprs)

parens p = do
	char '('
	skipSpaces
	x <- p
	char ')'
	skipSpaces
	return x

expressionParser :: Parser Expression
expressionParser = buildExpressionParser optable baseExpr
optable = [ [ Infix opTimes AssocLeft ], [ Infix opAdd AssocLeft ] ]
baseExpr    = intParser <|> parameterParser <|> identifierParser <|> tupleParser 
opTimes   = string "*" >> skipSpaces >> return (\x y -> (ExprCall "*") [x, y])
opAdd     = string "+" >> skipSpaces >> return (\x y -> (ExprCall "+") [x, y])

intTypeParser :: Parser Type
intTypeParser = do
	string "Int"
	skipMany space
	do {
		char '('
		; i1 <- rawIntParser
		; separatorParser
		; i2 <- rawIntParser
		; char ')'
		; return TInt
	} <|> return TInt

dataTypeParser = do
	string "Data"
	char '('
	s <- many1 letter
	char ')'
	return (TData s)

tupleTypeParser = do
	char '('
	exprs <- sepBy1 typeParser separatorParser
	char ')'
	return (TTuple exprs)

typeParser :: Parser Type
typeParser = do
	intTypeParser <|> dataTypeParser <|> tupleTypeParser

edgePackingParser :: Parser (String, Maybe Expression)
edgePackingParser = do
	char '~'
	s <- many1 letter
	skipSpaces
	do { x <- (parens expressionParser); return (s, Just x) } <|> return (s, Nothing)

edgeInscriptionParser :: Parser EdgeInscription
edgeInscriptionParser = 
	do { (name, limit) <- edgePackingParser; return (EdgePacking name limit) } <|> (expressionParser >>= (return . EdgeExpression))

parseSimple :: Parser a -> String -> a
parseSimple parser str = 
	case parse parser "" str of
		Left x -> error $ "Parsing error of " ++ str ++ ": " ++ show x
		Right x -> x

parseType :: String -> Type
parseType = parseSimple typeParser

parseExpr :: String -> Expression
parseExpr = parseSimple (skipSpaces >> expressionParser)

parseExpr' :: String -> Maybe Expression
parseExpr' "" = Nothing
parseExpr' x = Just $ parseExpr x

parseEdgeInscription :: String -> EdgeInscription 
parseEdgeInscription = parseSimple (skipSpaces >> edgeInscriptionParser)
