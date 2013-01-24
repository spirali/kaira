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

digits = "0123456789"
operator_chars = "+-*/%=!<>&|"

lpar, rpar, dot, delim, sem = map(pp.Suppress, "().,;")

ident = pp.Word(pp.alphas+"_:", pp.alphanums+"_:")
expression = pp.Forward()
number = (pp.Optional("-", "+") + pp.Word(digits) +
          pp.Optional(dot + pp.Word(digits)))
string = pp.dblQuotedString
operator = pp.Word(operator_chars)
parens = lpar + pp.Optional(expression + pp.ZeroOrMore(delim + expression)) + rpar
var_or_call = ident + pp.Optional(parens)
term = number | string | var_or_call
expression << term

mark = pp.Empty().setParseAction(lambda loc, t: loc)
expression_marks = pp.Group(mark + expression.suppress() + mark)

expressions_marks = expression_marks + pp.ZeroOrMore(sem + expression_marks)

typename = ident


def check_expression(expr):
    if len(expr) == 0:
        return "Expression is empty"
    try:
        expression.parseString(expr, parseAll=True)
        return None
    except pp.ParseException, e:
        return e.lineno, e.col, e.msg

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

def split_expressions(string, source):
    if string.strip() == "":
        return []
    try:
        return [ string[start:end] for start, end in
            expressions_marks.parseString(string, parseAll=True) ]
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)
