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

import parser
import generator
import checker

class CppTargetEnv:

    def parse_typename(self, string, source):
        return parser.parse_typename(string, source)

    def parse_expressions(self, string, source):
        return parser.split_expressions(string, source)

    def parse_expression(self, string, source, allow_empty):
        return parser.parse_expression(string, source, allow_empty)

    def parse_init_expression(self, string, source):
        return parser.parse_init_expression(string, source)

    def parse_edge_expression(self, string, source):
        return parser.parse_edge_expression(string, source)

    def is_expr_variable(self, string):
        return parser.is_variable(string)

    def get_expr_variables(self, string):
        return parser.get_expr_variables(string)

    def get_checker(self, project):
        return checker.Checker(project)

    def get_generator(self, project):
        return generator.CppGenerator(project)
