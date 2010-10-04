module Parser (
	parseType,
	parseExpr,
	parseExpr',
	parseEdgeInscription,
	parseGuard,
) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List

import Declarations

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
baseExpr    = intParser <|> parameterParser <|> identifierParser <|> tupleParser 
opBinary name   = reservedOp name >> return (\x y -> (ExprCall name) [x, y])

intTypeParser :: Parser Type
intTypeParser = do
	string "Int"
	return TInt

dataTypeParser = do
	string "Data"
	char '('
	s <- many1 letter
	char ')'
	return (TData s)

tupleTypeParser = do
	exprs <- parens $ sepBy1 typeParser comma
	return (TTuple exprs)

typeParser :: Parser Type
typeParser = do
	intTypeParser <|> dataTypeParser <|> tupleTypeParser

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

parseSimple :: Parser a -> String -> a
parseSimple parser str = 
	case parse parser "" str of
		Left x -> error $ "Parsing error of " ++ str ++ ": " ++ show x
		Right x -> x

parseType :: String -> Type
parseType = parseSimple typeParser

parseExpr :: String -> Expression
parseExpr = parseSimple (whiteSpace >> expressionParser)

parseExpr' :: String -> Maybe Expression
parseExpr' "" = Nothing
parseExpr' x = Just $ parseExpr x

parseEdgeInscription :: String -> EdgeInscription 
parseEdgeInscription = parseSimple (whiteSpace >> edgeInscriptionParser)

parseGuard :: String -> Expression
parseGuard = parseSimple (whiteSpace >> guardParser)
