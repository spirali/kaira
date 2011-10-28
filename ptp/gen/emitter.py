
infix_functions = [ "+", "-", "*", "/", "==", "!=", "&&", "||" ]

from base.neltypes import Type

class Emitter(object):

    def __init__(self):
        self.extern_table = {}
        self.variable_emitter = lambda name: name

    def call(self, name, args):
        if name in infix_functions and len(args) == 2:
            return "(({1}) {0} ({2}))".format(name, args[0].emit(self), args[1].emit(self))
        else:
            return "{0}({1})".format(name, ", ".join( [ e.emit(self) for e in args ]))

    def const_int(self, value):
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
            if t.name == "Bool":
                return "bool"
            elif t.name == "String":
                return "std::string"
        elif a == 1:
            if t.name == "Array":
                return "std::vector<" + self.emit_type(t.args[0]) + ">"
            if t.name == "__Place":
                return "CaPlace<" + self.emit_type(t.args[0]) + ">"
            if t.name == "__Token":
                return "CaToken<" + self.emit_type(t.args[0]) + ">*"
        raise Exception("Type '{0}' cannot be emitted".format(t))

    def emit_declarations(self, decls):
        return ",".join(("{0} {1}".format(self.emit_type(t), name) for name, t in decls))
    
    def as_string(self, expr, t):
        if t.name == "":
            return "({0}).as_string()".format(expr)
        return "ca_int_to_string({0})".format(expr)
