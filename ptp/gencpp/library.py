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
import build
import buildnet
import writer

def write_library_functions(builder):
    for net in builder.project.nets:
            write_library_function(builder, net)
    write_parameters_setters(builder)
    write_library_init_function(builder, )

def write_parameters_setters(builder):
    for p in builder.project.get_parameters():
        builder.line("void set_pararameter_{0}({1} value)",
                     p.get_name(),
                     builder.emit_type(p.get_type()))
        builder.block_begin()
        builder.line("param::{0}.__set_value(value);", p.get_name())
        builder.block_end()

def write_library(builder, header_filename):
    builder.line("#include \"{0}\"", header_filename)
    builder.emptyline()
    buildnet.write_core(builder)
    write_library_functions(builder)

def write_library_header_file(builder):
    builder.line("#ifndef CA_LIBRARY_{0}", builder.project.get_name())
    builder.line("#define CA_LIBRARY_{0}", builder.project.get_name())
    builder.emptyline()
    build.write_header(builder)
    build.write_types_declaration(builder)
    builder.emptyline()

    builder.line("void calib_init(int argc, char **argv);")
    for p in builder.project.get_parameters():
        builder.line("void set_pararameter_{0}({1} {0});",
                  p.get_name(),
                  builder.emit_type(p.get_type()))

    for net in builder.project.get_modules():
        builder.line("void {0}({1});",
                  net.name,
                  emit_library_function_declaration(builder.emitter, net))
    builder.emptyline()
    builder.line("#endif // CA_LIBRARY_{0}", builder.project.get_name())

def write_library_init_function(builder):
    builder.line("void calib_init(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder)
    builder.block_end()

def write_library_function(builder, net):
    builder.line("void {0}({1})",
                 net.name,
                 emit_library_function_declaration(builder.emitter, net, "___"))

    builder.block_begin()

    builder.line("ca_spawn_net({0});", net.get_index())
    builder.line("Net_{0} *n = (Net_{0}*)ca_get_main_net();", net.id)
    builder.line("CaThread *thread = ca_get_first_process()->get_thread(0);")

    em = emitter.Emitter(builder.project)
    em.variable_emitter = lambda name: "___" + name
    for e in net.interface_edges_out:
        buildnet.write_send_token(builder, em, e,
                                  trace_send=False,
                                  locking=False,
                                  interface_edge=True)

    builder.line("n->set_manual_delete();")
    builder.line("ca_main();")

    conditions = []
    for edge in net.interface_edges_in:
        if edge.is_normal():
            if not isinstance(edge.expr, ExprVar):
                raise utils.PtpException("Invalid expression", edge.expr.source)
            conditions.append("n->place_{0.id}.is_empty()".format(edge.get_place()))
    if conditions:
        builder.if_begin(" ||".join(conditions))
        builder.line('fprintf(stderr, "Token in output places of module {0} not found\\n");',
            net.get_name())
        builder.line("exit(-1);")
        builder.block_end()

    for e in net.get_interface_edges_in():
        if e.is_normal():
            for var in e.expr.get_free_vars():
                ret = "n->place_{0.id}.first_value()".format(e.get_place())
                builder.line("___{0} = {1};", var, ret)
        else:
            ret = "n->place_{0.id}.to_vector()".format(e.get_place())
            builder.line("___{0} = {1};", e.varname, ret)

    builder.line("delete n;")
    builder.block_end()

def get_library_input_arguments(net):
    variables = list(net.get_module_input_vars())
    variables.sort()
    return variables

def get_library_output_arguments(net):
    variables = list(net.get_module_output_vars())
    variables.sort()
    return variables

def get_library_output_only_arguments(net):
    variables = list(net.get_module_output_vars() - net.get_module_input_vars())
    variables.sort()
    return variables

def get_library_function_arguments(net, prefix=""):
    context = net.get_interface_context()
    input_vars = get_library_input_arguments(net)
    output_vars = get_library_output_only_arguments(net)
    return [ (prefix + name, context[name]) for name in input_vars + output_vars ]

def emit_library_function_declaration(emitter, net, prefix=""):
    return writer.emit_declarations(emitter, get_library_function_arguments(net, prefix),
                                    reference=True)
