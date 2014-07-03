#
#    Copyright (C) 2013-2014 Stanislav Bohm
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

# Reserved words of C++ (including C++11)
reserved_words = set([
    "alignas", "alignof", "and", "and_eq", "asm", "auto",
    "bitand", "bitor", "bool", "break", "case", "catch",
    "char", "char16_t", "char32_t", "class", "compl", "const",
    "constexpr", "const_cast", "continue", "decltype", "default", "delete",
    "do", "double", "dynamic_cast", "else", "enum", "explicit",
    "export", "extern", "false", "float", "for", "friend",
    "goto", "if", "inline", "int", "long", "mutable",
    "namespace", "new", "noexcept", "not", "not_eq", "nullptr",
    "operator", "or", "or_eq", "private", "protected", "public",
    "register", "reinterpret_cast", "return", "short", "signed", "sizeof",
    "static", "static_assert", "static_cast", "struct", "switch", "template",
    "this", "thread_local", "throw", "true", "try", "typedef",
    "typeid", "typename", "union", "unsigned", "using", "virtual",
    "void", "volatile", "wchar_t", "while", "xor", "xor_eq",
])

digits = "0123456789"
operator_chars = "+-*/%=!<>&|~"

lpar, rpar, dot, delim, sem, lbracket, rbracket, lt, bg, aps \
    = map(pp.Suppress, "().,;[]<>'")

def list_of(parser, delim, begin, end):
    return pp.Group(begin + pp.Optional(parser + pp.ZeroOrMore(delim + parser)) + end)

expression = pp.Forward()
typename = pp.Forward()

ident = pp.Word(pp.alphas+"_:", pp.alphanums+"_:")
number = pp.Suppress(pp.Word(digits) + pp.Optional(dot + pp.Word(digits)))
string = pp.Suppress(pp.dblQuotedString)
char = pp.Suppress(aps + pp.Word(pp.alphanums, exact=1) + aps)
operator = pp.Suppress(pp.Word(operator_chars))
parens = list_of(expression, delim, lpar, rpar)
template = list_of(typename, delim, lt, bg)
var_or_call = ident + pp.Suppress(pp.Optional(template)) + pp.Optional(parens, None)
var_or_call.setParseAction(lambda t: t[0] if t[1] is None else t[1])
basic_expression = pp.Forward()
basic_expression << ((number |
                      char |
                      string |
                      var_or_call |
                      lpar + expression + rpar)
                          + pp.Optional(dot + basic_expression)
                          + pp.Optional(pp.OneOrMore(operator) + basic_expression))
expression << pp.Optional(operator) + basic_expression

mark = pp.Empty().setParseAction(lambda loc, t: loc)
full_expression = (mark + expression.suppress() + mark) \
    .setParseAction(lambda s, loc, t: s[t[0]:t[1]].strip())

expressions = pp.delimitedList(full_expression, ";")

typename << (pp.ZeroOrMore(ident +
             pp.Optional(template) +
             pp.Optional(pp.Literal("*"))))

edge_config_param = lpar + full_expression + rpar
edge_config_item = pp.Group(ident + pp.Optional(edge_config_param, None))
edge_config = lbracket + pp.Group(pp.delimitedList(edge_config_item, ",")) + rbracket

edge_inscription = pp.Group(
    pp.Optional(edge_config, ()) +
    pp.Optional(full_expression, None) +
    pp.Optional(pp.Suppress("@") + full_expression, None))

edge_expr = pp.delimitedList(edge_inscription, ";")

init_by_expressions = (lbracket + expressions + rbracket) \
                        .setParseAction(lambda t: ("exprs", tuple(t)))
init_by_vector = pp.ParseElementEnhance(full_expression).setParseAction(
    lambda t: ("vector", t[0]))
init_expression = init_by_expressions | init_by_vector

def parse_expression(expr, source, allow_empty):
    if len(expr.strip()) == 0:
        if allow_empty:
            return None
        else:
            return "Missing expression"
    try:
        return full_expression.parseString(expr, parseAll=True)[0]
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def get_expr_variables(expr):
    if not expr:
        return set()
    s = set(expression.parseString(expr, parseAll=True))
    s.difference_update(reserved_words)
    return s

def parse_typename(tname, source):
    if len(tname) == 0:
        raise utils.PtpException("Missing type", source)
    try:
        return typename.parseString(tname, parseAll=True)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def is_variable(expr):
    if expr is None or expr.strip() in reserved_words:
        return False
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
    if len(string.strip()) == 0:
        raise utils.PtpException("Missing expression", source)

    try:
        inscriptions = edge_expr.parseString(string, parseAll=True)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

    results = []
    for configs, expr, target in inscriptions:
        config = {}
        for name, param in configs:
            if name in config:
                raise utils.PtpException(
                    "Configuration option '{0}' used more then once".format(name), source)
            else:
                config[name] = param
        results.append((config, expr, target))
    return results
