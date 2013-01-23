#
#    Copyright (C) 2012, 2013 Stanislav Bohm
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

from base.writer import Writer
import os

def emit_declarations(decls, reference=False):
    if reference:
        r = "&"
    else:
        r = ""
    return ",".join(("{0} {2}{1}".format(t, name, r) for name, t in decls))

def const_string(value):
    def escape(char):
        if char == "\n":
            return "\\n"
        if char == "\r":
            return "\\r"
        if char == "\t":
            return "\\t"
        if char == "\\":
            return "\\\\"
        if char == '"':
            return '\\"'
        return char
    return '"{0}"'.format("".join((escape(char) for char in value)))

def const_boolean(value):
    if value:
        return "true"
    else:
        return "false"


class CppWriter(Writer):

    def block_begin(self):
        self.line("{{")
        self.indent_push()

    def block_end(self):
        self.indent_pop()
        self.line("}}")

    def if_begin(self, expr):
        self.line("if ({0}) {{", expr)
        self.indent_push()

    def while_begin(self, expr):
        self.line("while ({0}) {{", expr)
        self.indent_push()

    def do_begin(self):
        self.line("do {{")
        self.indent_push()

    def do_end(self, expr):
        self.indent_pop()
        self.line("}} while ({0});", expr)

    def for_begin(self, start, condition, it):
        self.line("for ({0}; {1}; {2})", start, condition, it)
        self.line("{{")
        self.indent_push()

    def write_class_head(self, name, parent = None):
        if parent:
            inheritance = " : public {0} ".format(parent)
        else:
            inheritance = ""
        self.line("class {0} {1}{{", name, inheritance)
        self.indent_push()
        self.line("public:")

    def write_class_end(self):
        self.indent_pop()
        self.line("}};")

    def write_var_decl(self, name, t, reference = False):
        self.line("{0} {2}{1};", t, name, "&" if reference else "")

    def write_method_start(self, decl):
        self.line(decl + " {{")
        self.indent_push()

    def write_method_end(self):
        self.indent_pop()
        self.line("}}")

    def write_constructor(self, name, decls, inits):
        decl = "{0}({1})".format(name, decls)
        if inits:
            decl += " : " + ",".join(inits)
        self.write_method_start(decl)

    def line_directive(self, filename, lineno):
        self.line('#line {0} "{1}"', lineno, filename)

    def write_function(self, declaration, code, file_lineno = None):
        if file_lineno:
            filename, lineno = file_lineno
            self.line_directive(filename, lineno)
        self.raw_line(declaration)
        self.line("{{")
        self.raw_text(code)
        self.line("}}")
        if file_lineno:
            self.line_directive(os.path.basename(self.filename),
                                                 self.get_next_line_number())
