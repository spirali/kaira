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
import base.paths as paths
import os.path
import build

class CheckStatement(base.tester.Check):

    def __init__(self, expression, decls=None, return_type="void", source=None):
        self.expression = expression
        self.decls = decls
        self.return_type = return_type
        self.source = source

    def write_prologue(self, writer):
        if self.decls is not None:
            decls = utils.decls_to_list(self.decls)
        else:
            decls = ()
        writer.line("{0} {1} ({2}) {{",
                    self.return_type,
                    self.new_id(),
                    ",".join("{0} {1}".format(t, name) for name, t in decls))

    def write_epilogue(self, writer):
        writer.line("}}")

    def write_content(self, writer):
        writer.raw_line(self.expression)

    def throw_exception(self):
        raise utils.PtpException(self.message, self.source)

class TypeChecker:

    def __init__(self, name, source, functions):
        self.name = name
        self.sources = set([ source ])
        self.functions = set(functions)

    def update(self, type_checker):
        assert type_checker.name == self.name
        self.sources.update(type_checker.sources)
        self.functions.update(type_checker.functions)

    def add_checks(self, tester):
        var = tester.new_id()

        source = min(self.sources)
        check = CheckStatement("{0} *{1};".format(self.name, tester.new_id()), source=source)
        check.own_message = "Invalid type '{0}'".format(self.name)
        tester.add(check)

        message = "Function '{0}' not defined for type '{1}'"
        if "token_name" in self.functions:
            decls = { var: self.name + " &" }
            check = CheckStatement("token_name({0});".format(var),
                                   decls, source=source)
            check.own_message = message.format("token_name", self.name)
            tester.add(check)

        if "pack" in self.functions:
            decls = { var: self.name + " &", "packer": "CaPacker &" }
            check = CheckStatement("pack(packer, {0});".format(var),
                                   decls,
                                   source=source)
            check.own_message = message.format("pack", self.name)
            tester.add(check)

        if "pack" in self.functions:
            decls = { var: self.name + " &", "unpacker": "CaUnpacker &" }
            check = CheckStatement("return unpack<{0} >(unpacker);".format(self.name),
                                   decls,
                                   self.name,
                                   source=source)
            check.own_message = message.format("unpack", self.name)
            tester.add(check)



class Checker:

    def __init__(self, project):
        self.project = project
        self.types = {}
        self.expressions = []

    def check_type(self, typename, source, functions=()):
        t = self.types.get(typename)
        if t is None:
            self.types[typename] = TypeChecker(typename, source, functions)
        else:
            self.types[typename].update(TypeChecker(typename, source, functions))

    def check_expression(self, expr, decls, return_type, source):
        self.expressions.append((expr, decls, return_type, source))

    def prepare_writer(self, filename):
        builder = build.Builder(self.project, filename)
        build.write_header(builder)
        return builder

    def run(self):
        tester = base.tester.Tester()
        tester.prepare_writer = self.prepare_writer
        tester.args = [ "-I", os.path.join(paths.KAIRA_ROOT, paths.CAILIE_INCLUDE_DIR) ]

        tester.run()

        if tester.stderr:
            raise utils.PtpException(tester.stderr)

        for t in self.types.values():
            t.add_checks(tester)

        for expr, decls, return_type, source in self.expressions:
            check = CheckStatement(expr + ";", decls, source=source)
            tester.add(check)
            check = CheckStatement("return (" + expr + ");", decls, return_type, source)
            check.own_message = "Invalid type of expression"
            tester.add(check)

        check = tester.run()
        if check is not None:
            check.throw_exception()
