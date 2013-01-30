#
#    Copyright (C) 2013 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.


import pyparsing as pp
import base.utils as utils
import base.net

digits = "0123456789"
operator_chars = "+-*/%=!<>&|~"

lpar, rpar, dot, delim, sem, lbracket, rbracket, lt, bg, aps \
    = map(pp.Suppress, "().,;[]<>'")

ident = pp.Word(pp.alphas+"_:", pp.alphanums+"_:")
expression = pp.Forward()
number = pp.Word(digits) + pp.Optional(dot + pp.Word(digits))
string = pp.dblQuotedString
char = aps + pp.Word(pp.alphanums, exact=1) + aps
operator = pp.Word(operator_chars)
parens = lpar + pp.Optional(expression + pp.ZeroOrMore(delim + expression)) + rpar
basic_expression = pp.Forward()
basic_expression << ((number |
                      char |
                      string |
                      ident + pp.Optional(parens) |
                      lpar + expression + rpar)
                          + pp.Optional(dot + basic_expression)
                          + pp.Optional(pp.OneOrMore(operator) + basic_expression))
expression << pp.Optional(operator) + basic_expression

mark = pp.Empty().setParseAction(lambda loc, t: loc)
full_expression = (mark + expression.suppress() + mark) \
    .setParseAction(lambda s, loc, t: s[t[0]:t[1]])

expressions = pp.delimitedList(full_expression, ";")

typename = pp.Forward()
typename << (pp.ZeroOrMore(ident +
             pp.Optional(lt + typename + bg) +
             pp.Optional(pp.Literal("*"))))

edge_config_param = lpar + full_expression + rpar
edge_config_item = pp.Group(ident + pp.Optional(edge_config_param, None))
edge_config = lbracket + pp.Group(pp.delimitedList(edge_config_item, ",")) + rbracket

edge_expr = (pp.Optional(edge_config, ()) +
             pp.Optional(pp.Group(expressions), ()) +
             pp.Optional(pp.Suppress("@") + full_expression, None))
init_by_expressions = (lbracket + expressions + rbracket) \
                        .setParseAction(lambda t: ("exprs", tuple(t)))
init_by_vector = pp.ParseElementEnhance(full_expression).setParseAction(lambda t: ("vector", t[0]))
init_expression = init_by_expressions | init_by_vector

def parse_expression(expr, source, allow_empty):
    if len(expr.strip()) == 0:
        if allow_empty:
            return None
        else:
            return "Expression is empty"
    try:
        return full_expression.parseString(expr, parseAll=True)[0]
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def check_typename(tname, source):
    if len(tname) == 0:
        raise utils.PtpException("Type is empty", source)
    try:
        typename.parseString(tname, parseAll=True)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def is_variable(expr):
    try:
        ident.parseString(expr, parseAll=True)
        return True
    except pp.ParseException:
        return False

def take_substrings(string, pairs):
    return [ string[start:end] for start, end in pairs ]

def split_expressions(string, source):
    if string.strip() == "":
        return []
    try:
        return expressions.parseString(string, parseAll=True)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_init_expression(string, source):
    if string.strip() == "":
        return (None, None)
    try:
        return init_expression.parseString(string, parseAll=True)[0]
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def parse_edge_expression(string, source):
    try:
        configs, expressions, target = edge_expr.parseString(string, parseAll=True)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

    config = {}
    for name, param in configs:
        if name in config:
            raise utils.PtpException(
                "Configuration option '{0}' used twice".format(name), source)
        else:
            config[name] = param

    return (config,
            [ base.net.EdgeInscription(expr)
                for expr in expressions ],
            target)
