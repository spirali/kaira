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

        # Real class used for thread representation,
        # CaThreadBase is cast to this type
        self.thread_class = "CaThread"

        # Generate operator== and operator!= for generated types
        # If true then all ExternTypes have to implement operator== and operator!=
        self.generate_operator_eq = False

        # Generate hash functions for generated types
        # If true then all ExternTypes have to implement get_hash
        self.generate_hash = False


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

def get_hash_function_name(project, t):
    if t == t_string:
        return "ca_hash_string"
    if t == t_double:
        return "ca_hash_double"
    if t == t_float:
        return "ca_hash_float"
    if t == t_int:
        return "ca_hash_int"
    if t == t_bool:
        return "ca_hash_bool"
    if t.name == "":
        return "{0}_hash".format(t.get_safe_name())
    if t.name == "Array":
        return "array_{0}_hash".format(t.args[0].get_safe_name())
    etype = project.get_extern_type(t.name)
    if etype:
        if etype.has_hash_function():
            return "{0}_hash".format(etype.name)
        else:
            raise utils.PtpException("Extern type '{0}' has not defined hash function"
                                    .format(etype.name))
    raise Exception("Unknown type: " + str(t))

def get_hash_code(project, t, code):
    return "{0}({1})".format(get_hash_function_name(project, t), code)

def get_hash_combination(codes):
    if not codes:
        return "113"
    numbers = [1, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71,
              73, 79, 83, 89, 97, 101, 103, 107, 109, 113]
    result = []
    for i, code in enumerate(codes):
        result.append("({0} * {1})".format(numbers[i % len(numbers)], code))

    return "^".join(result)

def get_hash_codes(project, types_codes):
    codes = [ get_hash_code(project, t, code) for t, code in types_codes ]
    return get_hash_combination(codes)

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
    write_parameters_forward(builder)
    builder.emptyline()
    if builder.project.get_head_code():
        builder.line_directive("*head", 1)
        builder.raw_text(builder.project.get_head_code())
        builder.line_directive(os.path.basename(builder.filename),
                                             builder.get_next_line_number())
        builder.emptyline()

def write_parameters_forward(builder):
    builder.line("struct param")
    builder.block_begin()
    for p in builder.project.get_parameters():
        builder.line("static CaParameterInt {0};", p.get_name())
    builder.write_class_end()

def write_parameters(builder):
    for p in builder.project.get_parameters():
        builder.line("CaParameterInt param::{0}({1}, {2}, CA_PARAMETER_MANDATORY);",
                     p.name,
                     builder.emitter.const_string(p.name),
                     builder.emitter.const_string(p.description))

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

    if builder.generate_operator_eq:
        builder.line("bool operator==(const {0} &rhs) const", class_name)
        builder.block_begin()
        for e, ta in decls:
            builder.line("if ({0} != rhs.{0}) return false;", e)
        builder.line("return true;")
        builder.block_end()

        builder.line("bool operator!=(const {0} &rhs) const", class_name)
        builder.block_begin()
        builder.line("return !(*this == rhs);")
        builder.block_end()

    if builder.generate_hash:
        types_codes = [ (ta, name) for name, ta in decls ]
        builder.line("size_t hash() const", class_name)
        builder.block_begin()
        builder.line("return {0};", get_hash_codes(builder.project, types_codes))
        builder.block_end()



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

def write_array_hash(builder, t):
    builder.line("size_t array_{0}_hash(const std::vector <{1} > &vector)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("size_t h = vector.size();")
    builder.line("for (std::vector<{0} >::const_iterator i = vector.begin(); i != vector.end(); i++)",
              builder.emit_type(t))
    builder.block_begin()
    builder.line("h += {0};", get_hash_code(builder.project, t, "(*i)"))
    builder.line("h += h << 10;")
    builder.line("h ^= h >> 6;")
    builder.block_end()
    builder.line("h += h << 3;")
    builder.line("h ^= h >> 11;")
    builder.line("h += h << 15;")
    builder.line("return h;")
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
    if builder.generate_hash:
        builder.line("size_t array_{0}_hash(const std::vector <{1} > &vector);",
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

def write_tuple_hash(builder, t):
    builder.line("size_t {0}_hash(const {1} &s)",
              t.get_safe_name(), builder.emit_type(t))
    builder.block_begin()
    builder.line("return s.hash();")
    builder.block_end()

def write_types(builder):
    write_extern_types_functions(builder, True)
    for t in get_ordered_types(builder.project):
        if len(t.args) == 1 and t.name == "Array":
            write_array_as_string(builder, t.args[0])
            write_array_pack(builder, t.args[0])
            write_array_unpack(builder, t.args[0])
            if builder.generate_hash:
                write_array_hash(builder, t.args[0])
        if t.name == "":
            write_tuple_as_string(builder, t)
            if builder.generate_hash:
                write_tuple_hash(builder, t)

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

def write_trace_user_function(builder, ufunction, type):
    declaration = "void trace_{0}(CaTraceLog *tracelog, const {1} &value)".format(
                                                    ufunction.get_name(), type)
    returntype = builder.emit_type(ufunction.get_returntype())
    code = "\t" + returntype + " result = ufunction_" + ufunction.get_name()
    n = len(ufunction.get_parameters())
    if n == 1:
        code += "(value);\n"
    else:
        code += "(" + ", ".join(["value.t{0}".format(i) for i in xrange(n)]) + ");\n"
    code += "\ttracelog->trace_{0}(result);\n".format(ufunction.get_returntype().name.lower())
    builder.write_function(declaration, code)

def write_trace_value(builder, type):
    declaration = "void trace_value(CaTraceLog *tracelog, const {0} &value)".format(
                                                                        builder.emit_type(type))
    code = "\tstd::string result = {0}(value);\n".format(
            get_to_string_function_name(builder.project, type)) +\
            "\ttracelog->trace_string(result);\n"
    builder.write_function(declaration, code)

def write_trace_user_functions(builder):
    traces = []
    value_traces = []
    for net in builder.project.nets:
        for place in net.places:
            for fn_name in place.tracing:
                if fn_name == "value":
                    if not place.type in value_traces:
                        value_traces.append(place.type)
                    continue
                if not (fn_name, place.type) in traces:
                    traces.append((fn_name, place.type))

    for type in value_traces:
        write_trace_value(builder, type)
    for fn_name, type in traces:
        fn = builder.project.get_user_function(fn_name.replace("fn: ", ""))
        write_trace_user_function(builder, fn, builder.emit_type(type))

def write_extern_types_functions(builder, definitions):
    decls = {
             "getstring" : "std::string {0.name}_getstring(const {0.rawtype} &obj)",
             "pack" : "void {0.name}_pack(CaPacker &packer, {0.rawtype} &obj)",
             "unpack" : "{0.rawtype} {0.name}_unpack(CaUnpacker &unpacker)",
             "hash" : "size_t {0.name}_hash(const {0.rawtype} &obj)",
    }

    def write_fn(etype, name):
        source = ("*{0}/{1}".format(etype.get_name(), name), 1)
        if etype.get_code(name) is None:
            raise utils.PtpException(
                    "Function '{0}' for extern type '{1.name}' has no body defined" \
                        .format(name, etype))
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

        if etype.has_hash_function():
            f(etype, "hash")

def write_basic_definitions(builder):
    write_parameters(builder)
    write_types(builder)
    write_user_functions(builder)
    write_trace_user_functions(builder)
