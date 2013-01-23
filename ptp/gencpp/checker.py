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
#


import base.tester
import base.utils as utils

class CheckType(base.tester.Check):

    def __init__(self, type_name):
        self.type_name = type_name

    def write_content(self, writer):
        writer.line("{0} {1};", self.type_name, self.new_id())


class CheckStatement(base.tester.Check):

    def __init__(self, expression, decls=(), return_type="void"):
        self.expression = expression
        self.decls = decls
        self.return_type = return_type

    def write_prologue(self, writer):
        writer.line("{0} {1} ({2}) {{",
                    self.return_type,
                    self.new_id(),
                    ",".join("{0} {1}".format(t, name) for name, t in self.decls))

    def write_epilogue(self, writer):
        writer.line("}}")

    def write_content(self, writer):
        writer.raw_line(self.expression)


class Checker:

    def __init__(self):
        self.types = {}
        self.expressions = []

    def check_type(self, typename, source):
        sources = self.types.get(typename)
        if sources:
            self.types[typename].append(source)
        else:
            self.types[typename] = [ source ]

    def check_expression(self, expr, decls, return_type, source):
        self.expressions.append((expr, decls, return_type, source))

    def run(self):
        tester = base.tester.Tester()

        for t, sources in self.types.items():
            check = CheckType(t)
            check.key = sources[0]
            tester.add(CheckType(t))

        for expr, decls, return_type, source in self.expressions:
            check = CheckStatement(expr + ";", decls)
            check.key = source
            tester.add(check)
            check = CheckStatement("return (" + expr + ");", decls, return_type)
            check.key = source
            check.own_message = "Invalid type of expression"
            tester.add(check)

        check = tester.run()
        if check is not None:
            self.resolve_error(check)

    def resolve_error(self, check):
        source = check.key
        raise utils.PtpException(check.message, source)
