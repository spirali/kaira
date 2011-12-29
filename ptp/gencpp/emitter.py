#
#    Copyright (C) 2011 Stanislav Bohm
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

infix_functions = [ "+", "-", "*", "/", "==", "!=", "&&", "||", "<", ">", "<=", ">=" ]

from base.neltypes import Type

class Emitter(object):

    def __init__(self, project):
        self.project = project
        self.extern_table = {}
        self.variable_emitter = lambda name: name

    def call(self, name, args):
        a = [ e.emit(self) for e in args ]
        if name in infix_functions and len(args) == 2:
            return "(({1}) {0} ({2}))".format(name, a[0], a[1])
        else:

            ufunction = self.project.get_user_function(name)
            if ufunction:
                name = "ufunction_" + name
                if ufunction.with_context:
                    a = [ "ctx" ] + a
            else:
                if name == "process_id":
                    name = "thread->get_process_id"
                elif name == "process_count":
                    name = "thread->get_process_count"
                else:
                    name = "ca_" + name
            return "{0}({1})".format(name, ", ".join(a))

    def const_int(self, value):
        return str(value)

    def const_double(self, value):
        return str(value)

    def const_string(self, value):
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

    def const_array(self, value, t):
        if t.name != "Array" or len(t.args) != 1:
            raise Exception("Invalid type")
        args = "".join( [ "(" + e.emit(self) + ")" for e in value ] )
        return '(ca_array<{0} >{1}.end())'.format(self.emit_type(t.args[0]), args)

    def const_boolean(self, value):
        if value:
            return "true"
        else:
            return "false"

    def parameter(self, name):
        return "__param_" + name

    def variable(self, name):
        return self.variable_emitter(name)

    def tuple(self, t, args):
        return "{0}({1})".format(t.get_safe_name(), ",".join((e.emit(self) for e in args)))

    def set_extern(self, name, value):
        self.extern_table[name] = value

    def extern(self, name):
        return self.extern_table[name]

    def tuple_get(self, expr, index):
        return "(({0}).t{1})".format(expr.emit(self), index)

    def i_set(self, writer, varname, expr):
        vname = self.variable_emitter(varname)
        writer.line("{0} = {1};", vname, expr.emit(self))

    def i_if(self, writer, expr, true, false):
        writer.line("if ({0}) {{", expr.emit(self))
        writer.indent_push()
        true.emit(self, writer)
        writer.indent_pop()
        writer.line("}} else {{")
        writer.indent_push()
        false.emit(self, writer)
        writer.indent_pop()
        writer.line("}}")

    def i_extern(self, writer, name):
        writer.line(self.extern_table[name])

    def emit_type(self, t):
        if isinstance(t, str):
            return t
        if not isinstance(t, Type):
            raise Exception("'{0}' cannot be emitted as type".format(t))
        if t.name == "":
            return t.get_safe_name()
        a = t.get_arity()
        if a == 0:
            if t.name == "Int":
                return "int"
            elif t.name == "String":
                return "std::string"
            elif t.name == "Bool":
                return "bool"
            elif t.name == "Double":
                return "double"
            elif t.name == "Float":
                return "float"

            etype = self.project.get_extern_type(t.name)
            if etype:
                return etype.get_rawtype()
        elif a == 1:
            if t.name == "Array":
                return "std::vector<" + self.emit_type(t.args[0]) + " >"
        raise Exception("Type '{0}' cannot be emitted".format(t))
