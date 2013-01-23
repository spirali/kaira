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

lpar, rpar, dot, delim = map(pp.Suppress, "().,")

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

typename = ident

expression_endl = expression + pp.StringEnd()
typename_endl = typename + pp.StringEnd()
variable_endl = ident + pp.StringEnd()

def check_expression(expr):
    if len(expr) == 0:
        return "Expression is empty"
    try:
        expression_endl.parseString(expr)
        return None
    except pp.ParseException, e:
        return e.lineno, e.col, e.msg

def check_typename(tname, source):
    if len(tname) == 0:
        raise utils.PtpException("Type is empty", source)
    try:
        typename_endl.parseString(tname)
    except pp.ParseException, e:
        raise utils.PtpException(e.msg, source)

def is_variable(expr):
    try:
        variable_endl.parseString(expr)
        return True
    except pp.ParseException:
        return False
