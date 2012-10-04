#
#    Copyright (C) 2011, 2012 Stanislav Bohm
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


import base.utils as utils
from base.neltypes import t_int, t_string, t_float, t_double, t_bool
import emitter
import os.path
from base.gentools import get_ordered_types
from writer import CppWriter

class Builder(CppWriter):

    def __init__(self, project, filename=None):
        CppWriter.__init__(self)
        self.filename = filename
        self.project = project
        self.emitter = emitter.Emitter(project)

    def emit_type(self, t):
        return self.emitter.emit_type(t)


def get_to_string_function_name(project, t):
    if t.name == "":
        return "{0}_as_string".format(t.get_safe_name())
    if len(t.args) == 0:
        etype = project.get_extern_type(t.name)
        if etype:
            return "{0}_getstring".format(etype.name)
    if t == t_string:
        return "ca_string_to_string"
    if t.name == "Array" and len(t.args) == 1:
        return "array_{0}_as_string".format(t.args[0].get_safe_name())
    if t == t_double:
        return "ca_double_to_string"
    if t == t_float:
        return "ca_float_to_string"
    if t == t_bool:
        return "ca_bool_to_string"
    return "ca_int_to_string"

def get_unpack_code(project, t, unpacker):
    if t == t_int:
        return "{0}.unpack_int()".format(unpacker)
    if t == t_string:
        return "{0}.unpack_string()".format(unpacker)
    if t == t_float:
        return "{0}.unpack_float()".format(unpacker)
    if t == t_double:
        return "{0}.unpack_double()".format(unpacker)
    if t.name == "Array":
        return "array_{1}_unpack({0})".format(unpacker, t.args[0].get_safe_name())
    if t.name == "":
        return "{0}({1})".format(t.get_safe_name(), unpacker)
    etype = project.get_extern_type(t.name)
    if etype:
        if etype.get_transport_mode() == "Disabled":
            raise utils.PtpException("Transport of type '{0.name}' is disabled".format(etype))
        if etype.get_transport_mode() == "Direct":
            return "* (({1} *) {0}.unpack(sizeof({1})))".format(unpacker, etype.get_rawtype())
        return "{0.name}_unpack({1})".format(etype, unpacker)
    raise Exception("Unknown type: " + str(t))

def get_pack_code(project, t, packer, code):
    if t == t_int:
        return "{0}.pack_int({1})".format(packer, code)
    if t == t_string:
        return "{0}.pack_string({1})".format(packer, code)
    if t == t_float:
        return "{0}.pack_float({1})".format(packer, code)
    if t == t_double:
        return "{0}.pack_double({1})".format(packer, code)
    if t.name == "Array":
        return "array_{2}_pack({0}, {1})".format(packer, code, t.args[0].get_safe_name())
    if t.name == "":
        return "({1}).pack({0})".format(packer, code)
    etype = project.get_extern_type(t.name)
    if etype:
        if etype.get_transport_mode() == "Disabled":
            raise utils.PtpException("Transport of type '{0.name}' is disabled".format(etype))
        if etype.get_transport_mode() == "Direct":
            return "{0}.pack(&{1}, sizeof({1}))".format(packer, code)
        return "{0.name}_pack({1}, {2})".format(etype, packer, code)
    raise Exception("Unknown type: " + str(t))

def get_code_as_string(project, expr, t):
    if t == t_string:
        return expr
    return "{0}({1})".format(get_to_string_function_name(project, t), expr)

def write_header(builder):
    builder.line("/* This file is automatically generated")
    builder.line("   do not edit this file directly! */")
    builder.emptyline()
    builder.line('#include <cailie.h>')
    builder.line('#include <algorithm>')
    builder.line('#include <stdlib.h>')
    builder.line('#include <stdio.h>')
    builder.line('#include <sstream>')
    builder.emptyline()

    if builder.project.get_head_code():
        builder.line_directive("*head", 1)
        builder.raw_text(builder.project.get_head_code())
        builder.line_directive(os.path.basename(builder.filename),
                                             builder.get_next_line_number())
        builder.emptyline()

def write_parameters(builder):
    for p in builder.project.get_parameters():
        tstr = builder.emitter.emit_type(p.get_type())
        builder.line("{0} __param_{1};", tstr, p.get_name())
        decl = "{0} parameter_{1}()".format(tstr, p.get_name())
        code = "\treturn __param_{0};".format(p.get_name())
        builder.write_function(decl, code)

def write_tuple_class(builder, tp):
    class_name = tp.get_safe_name()
    builder.write_class_head(class_name)
    decls = [ ("t{0}".format(i), ta) for i, ta in enumerate(tp.args) ]
    for name, ta in decls:
        builder.write_var_decl(name, builder.emit_type(ta))

    builder.write_constructor(class_name,
                           builder.emit_declarations(decls),
                           [ "{0}({0})".format(name) for name, _ in decls ])
    builder.write_method_end()

    args = [ "{0}({1})".format(e, get_unpack_code(builder.project, t, "unpacker"))
             for e, t in decls ]
    builder.write_constructor(class_name, "CaUnpacker &unpacker", args)
    builder.write_method_end()

    builder.write_method_start("std::string as_string() const")
    builder.line('return std::string("(") + {0} + ")";',
              ' + "," +'.join((get_code_as_string(builder.project, e, t)
                               for e, t in decls)))
    builder.write_method_end()

    builder.write_method_start("void pack(CaPacker &packer)")
    for e, ta in decls:
        builder.line(get_pack_code(builder.project, ta, "packer", e) + ";")
    builder.write_method_end()

    builder.write_class_end()

def write_array_as_string(builder, t):
    builder.line("std::string array_{0}_as_string(const std::vector <{1} > &vector)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("std::stringstream osstream;")
    builder.line('osstream << "[";')
    builder.line("if (vector.size() > 0)")
    builder.block_begin()
    builder.line("std::vector<{0} >::const_iterator i = vector.begin();",
              builder.emit_type(t))
    builder.line("osstream << {0};", get_code_as_string(builder.project, "*i", t))
    builder.line("i++;")
    builder.line("for (; i != vector.end(); i++)")
    builder.block_begin()
    builder.line('osstream << "," << {0};', get_code_as_string(builder.project, "*i", t))
    builder.block_end()
    builder.block_end()
    builder.line('osstream << "]";')
    builder.line("return osstream.str();")
    builder.block_end()


def write_array_pack(builder, t):
    builder.line("void array_{0}_pack(CaPacker &packer, std::vector <{1} > &vector)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("packer.pack_size(vector.size());")
    builder.line("for (std::vector<{0} >::iterator i = vector.begin(); i != vector.end(); i++)",
              builder.emit_type(t))
    builder.block_begin()
    builder.line("{0};", get_pack_code(builder.project, t, "packer", "(*i)"))
    builder.block_end()
    builder.block_end()

def write_array_unpack(builder, t):
    builder.line("std::vector <{1} > array_{0}_unpack(CaUnpacker &unpacker)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("size_t s = unpacker.unpack_size();")
    builder.line("std::vector <{0} > vector;", builder.emit_type(t))
    builder.line("for (size_t t = 0; t < s; t++)")
    builder.block_begin()
    builder.line("vector.push_back({0});", get_unpack_code(builder.project, t, "unpacker"))
    builder.block_end()
    builder.line("return vector;")
    builder.block_end()

def write_array_declaration(builder, t):
    builder.line("std::string array_{0}_as_string(const std::vector <{1} > &vector);",
              t.get_safe_name(), builder.emit_type(t))
    builder.line("size_t array_{0}_size(std::vector <{1} > &vector);",
              t.get_safe_name(), builder.emit_type(t))
    builder.line("void array_{0}_pack(CaPacker &packer, std::vector <{1} > &vector);",
              t.get_safe_name(), builder.emit_type(t))
    builder.line("std::vector <{1} > array_{0}_unpack(CaUnpacker &unpacker);",
              t.get_safe_name(), builder.emit_type(t))

def write_types_declaration(builder):
    write_extern_types_functions(builder, False)
    for t in get_ordered_types(builder.project):
        if t.name == "":
            write_tuple_class(builder, t)
            builder.line("std::string {0}_as_string(const {1} &s);",
                      t.get_safe_name(), builder.emit_type(t))
        if len(t.args) == 1 and t.name == "Array":
            write_array_declaration(builder, t.args[0])

def write_tuple_as_string(builder, t):
    builder.line("std::string {0}_as_string(const {1} &s)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("return s.as_string();")
    builder.block_end()

def write_types(builder):
    write_extern_types_functions(builder, True)
    for t in get_ordered_types(builder.project):
        if len(t.args) == 1 and t.name == "Array":
            write_array_as_string(builder, t.args[0])
            write_array_pack(builder, t.args[0])
            write_array_unpack(builder, t.args[0])
        if t.name == "":
            write_tuple_as_string(builder, t)

def write_user_function(builder, ufunction):
    params =  ufunction.get_parameters()
    if ufunction.with_context:
        params = [ ("ctx", "CaContext &") ] + list(params)
    returntype = builder.emit_type(ufunction.get_returntype())
    declaration = "{1} ufunction_{0}({2})".format(ufunction.get_name(),
                                                  returntype,
                                                  builder.emit_declarations(params))
    source = ("*{0.id}/user_function".format(ufunction), 1)
    builder.write_function(declaration, ufunction.get_code(), source)

def write_user_functions(builder):
    for ufunction in builder.project.get_user_functions():
        write_user_function(builder, ufunction)

def write_extern_types_functions(builder, definitions):
    decls = {
             "getstring" : "std::string {0.name}_getstring(const {0.rawtype} &obj)",
             "pack" : "void {0.name}_pack(CaPacker &packer, {0.rawtype} &obj)",
             "unpack" : "{0.rawtype} {0.name}_unpack(CaUnpacker &unpacker)"
    }

    def write_fn(etype, name):
        source = ("*{0}/{1}".format(etype.get_name(), name), 1)
        builder.write_function(decls[name].format(etype), etype.get_code(name), source)

    def declare_fn(etype, name):
        builder.line(decls[name].format(etype) + ";")

    if definitions:
        f = write_fn
    else:
        f = declare_fn

    for etype in builder.project.get_extern_types():
        if definitions and not etype.has_code("getstring"):
            builder.write_function(decls["getstring"].format(etype),
                "return {0};".format(builder.emitter.const_string(etype.name)))
        else:
            f(etype, "getstring")

        if etype.get_transport_mode() == "Custom":
            if not etype.has_code("pack") or not etype.has_code("unpack"):
                raise utils.PtpException("Extern type has custom transport mode "
                                         "but pack/unpack missing.")
            f(etype, "pack")
            f(etype, "unpack")

def write_parameters_setters(builder):
    for p in builder.project.get_parameters():
        builder.line("void set_pararameter_{0}({1} {0})",
                     p.get_name(),
                     builder.emit_type(p.get_type()))
        builder.block_begin()
        builder.line("__param_{0} = {0};", p.get_name())
        builder.block_end()