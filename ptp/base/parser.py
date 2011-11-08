
from pyparsing import *

import expressions as e
import neltypes as t
import utils

digits = "0123456789"
lpar = Suppress("(")
rpar = Suppress(")")
delim = Suppress(",")
operator_chars = [ "+", "-", "*", "/", "%", "=", "!", "<", ">", "&", "|" ]
def OpKeyword(x):
    return Keyword(x, operator_chars)

def ident_action(tokens):
    if tokens[1] is None:
        return e.ExprVar(tokens[0])
    else:
        return e.ExprCall(tokens[0], list(tokens[1]))

def tuple_action(tokens):
    if len(tokens[0]) == 1:
        return tokens[0][0]
    else:
        return e.ExprTuple(list(tokens[0]))

def operator_action(tokens):
    if len(tokens) == 1:
        return tokens[0]
    prev = tokens[:-2]
    return e.ExprCall(tokens[-2], [ operator_action(prev), tokens[-1] ])

ident = Word(alphanums+"_")
expression = Forward()
integer = (Optional("-", "+") + Word(digits)).setParseAction(lambda tokens: e.ExprInt(int(tokens[0] + tokens[1])))
string = dblQuotedString.setParseAction(lambda tokens: e.ExprString(tokens[0][1:-1]))
array_normal = Optional(Group(expression + ZeroOrMore( Suppress( delim ) + expression )), [])
array_normal.setParseAction(lambda tokens: e.ExprArray(tuple(tokens[0])))
array_range = expression + Suppress(Literal("..")) + expression
array_range.setParseAction(lambda tokens: e.ExprCall('range', list(tokens)))
array = Suppress("[") + (array_range | array_normal) + Suppress("]")
parameter = (Suppress("#") + ident).setParseAction(lambda tokens: e.ExprParam(tokens[0]))
parens = lpar + Optional(Group(expression + ZeroOrMore( Suppress( delim ) + expression )), []) + rpar
var_or_call = (ident + Optional(parens, None)).setParseAction(ident_action)
tupleparser = parens.copy().setParseAction(tuple_action)
atom = (integer | string | var_or_call | tupleparser | parameter | array)

op1 = OpKeyword("*") | OpKeyword("/")
op2 = OpKeyword("+") | OpKeyword("-")
op3 = OpKeyword("==") | OpKeyword("!=") | OpKeyword("<=") | OpKeyword(">=") | OpKeyword("<") | OpKeyword(">")
op4 = OpKeyword("&&") | OpKeyword("||")

expr1 = (atom + ZeroOrMore(op1 + atom)).setParseAction(operator_action)
expr2 = (expr1 + ZeroOrMore(op2 + expr1)).setParseAction(operator_action)
expr3 = (expr2 + ZeroOrMore(op3 + expr2)).setParseAction(operator_action)
expression << (expr3 + ZeroOrMore(op4 + expr3)).setParseAction(operator_action)

typeparser = Forward()
typeargs = Optional(lpar + Group(typeparser + ZeroOrMore( Suppress( delim ) + typeparser )) + rpar, [])
typeparser << (Optional(ident,"") + typeargs).setParseAction(lambda tokens: t.Type(tokens[0], list(tokens[1])))

packing = Suppress("~").setParseAction(lambda tokens: "packing")
output = Optional(packing, "normal") + expression + Optional(Suppress("@") + expression, None) \
    + Optional(Suppress("?") + expression, None)
input = Optional(packing, "normal") + expression

parameter = (typeparser + ident).setParseAction(lambda tokens: (tokens[1], tokens[0]))
parameters = parameter + ZeroOrMore(delim + parameter)

def parse_expression(string, source = None):
    if string.strip() == "":
        raise utils.PtpException("Expression missing", source)
    try:
        return expression.parseString(string)[0]
    except ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_type(string, source = None):
    if string.strip() == "":
        raise utils.PtpException("Type missing", source)
    try:
        return typeparser.parseString(string)[0]
    except ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_expression_or_empty(string, source):
    if string.strip() == "":
        return None
    else:
        return parse_expression(string, source)

def parse_output_inscription(string, source = None):
    if string.strip() == "":
        raise utils.PtpException("Expression missing", source)
    try:
        return tuple(output.parseString(string, source))
    except ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_input_inscription(string, source = None):
    if string.strip() == "":
        raise utils.PtpException("Expression missing", source)
    try:
        return tuple(input.parseString(string, source))
    except ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_parameters_declaration(string, source = None):
    if string.strip() == "":
        return ()
    try:
        return tuple(parameters.parseString(string))
    except ParseException, e:
        raise utils.PtpException(e.msg, source)
