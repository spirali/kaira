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

def replace_dolar(string, text):
    result = []
    l = len(string)
    i = 0
    while i < l:
        c = string[i]
        if c == "$":
            if i + 1 < l and string[i + 1] == c:
                i += 1
                result.append(c)
            else:
                result.append(text)
        else:
            result.append(c)
        i += 1
    return "".join(result)

def get_safe_name(string):
    """
        Creates C++ identifier from string
    """
    result = []
    for c in string:
        if c.isalnum():
            result.append(c)
        else:
            result.append("_")
    return "".join(result)

class CppWriter(Writer):

    def block_begin(self):
        self.line("{{")
        self.indent_push()

    def block_end(self):
        self.indent_pop()
        self.line("}}")

    def switch_begin(self, expr):
        self.line("switch ({0}) {{", expr)
        self.indent_push()

    def if_begin(self, expr, *args, **kw):
        self.line("if ({0}) {{", self.expand(expr, *args, **kw))
        self.indent_push()

    def if_not_begin(self, expr, *args, **kw):
        self.line("if (!({0})) {{", self.expand(expr, *args, **kw))
        self.indent_push()

    def else_if(self, expr, *args, **kw):
        self.indent_pop()
        self.line("}} else if ({0}) {{", self.expand(expr, *args, **kw))
        self.indent_push()

    def write_else(self):
        self.indent_pop()
        self.line("}} else {{")
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

    def for_begin(self, expr, *args, **kw):
        self.line("for ({0}) {{", self.expand(expr, *args, **kw))
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

    def write_function(self, declaration, code, file_lineno=None):
        if file_lineno:
            filename, lineno = file_lineno
            self.line_directive(filename, lineno)
        self.raw_line(declaration)
        self.line("{{")
        self.raw_text(code)
        self.line("}}")

    def expand(self, string, *args):
        if "$" in string:
            string = replace_dolar(string, "__kaira__")
        return string.format(*args)

    def line(self, string, *args, **kw):
        if "$" in string:
            string = replace_dolar(string, "__kaira__")
        Writer.line(self, string, *args, **kw)
