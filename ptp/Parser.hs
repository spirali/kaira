module Parser (
	parseType,
	parseExpr,
	parseExpr',
) where

import Text.ParserCombinators.Parsec
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Declarations

rawIntParser = do
	ds <- many1 digit
	return (read ds)

intParser = do
	i <- rawIntParser
	return (ExprInt i)

identifierParser = do
	ds <- many1 letter
	skipMany space
	do {
		exprs <- parenParamsParser;
		return (ExprCall ds exprs)
	} <|> return (ExprVar ds)

separatorParser = do 
	skipMany space
	skipMany (char ',')
	skipMany space

parenParamsParser = do
	char '('
	exprs <- sepBy expressionParser separatorParser
	char ')'
	return exprs

tupleParser = do
	exprs <- parenParamsParser
	return (ExprTuple exprs)

expressionParser :: Parser Expression
expressionParser = do
	intParser <|> identifierParser <|> tupleParser

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

parseSimple :: Parser a -> String -> a
parseSimple parser str = 
	let Right e = parse parser "" str in e

parseType :: String -> Type
parseType = parseSimple typeParser

parseExpr :: String -> Expression
parseExpr = parseSimple expressionParser

parseExpr' :: String -> Maybe Expression
parseExpr' "" = Nothing
parseExpr' x = Just $ parseExpr x
