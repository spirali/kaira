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
    buildnet.write_user_functions(builder)

def write_library_header_file(builder):
    build.write_header_file(builder, close_guard=False)
    builder.emptyline()
    builder.line("void calib_init(int argc, char **argv);")
    for p in builder.project.get_parameters():
        builder.line("void set_pararameter_{0}({1} {0});",
                  p.get_name(),
                  builder.emit_type(p.get_type()))

    for net in builder.project.nets:
        builder.line("void {0}({1});",
                 net.name,
                 writer.emit_declarations(get_library_function_arguments(net),
                                          reference=True))
    build.write_header_file_close_guard(builder)

def write_library_init_function(builder):
    builder.line("void calib_init(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder)
    builder.block_end()

def write_library_function(builder, net):
    builder.line("void {0}({1})",
                 net.name,
                 writer.emit_declarations(get_library_function_arguments(net),
                                          reference=True))
    builder.block_begin()

    builder.line("ca::spawn_net({0});", net.get_index())
    builder.line("Net_{0} *$n = (Net_{0}*)ca::get_main_net();", net.id)

    for place in net.get_input_places():
        builder.line("$n->place_{0.id}.add({0.interface_input});", place)

    builder.line("$n->set_manual_delete();")
    builder.line("ca::main();")

    for place in net.get_output_places():
        builder.if_begin("$n->place_{0.id}.is_empty()", place)
        builder.line('fprintf(stderr, "Token in output place of net {0} not found. Aborting.\\n");',
            net.get_name())
        builder.line("exit(-1);")
        builder.block_end()
        builder.line("{0.interface_output} = $n->place_{0.id}.begin()->value;", place)

    builder.line("delete $n;")
    builder.block_end()

def get_library_function_arguments(net):
    inputs = net.get_input_places()
    outputs = [ place for place in net.get_output_places() if place not in inputs ]
    return [ (place.interface_input, place.type) for place in inputs ] + \
           [ (place.interface_output, place.type) for place in outputs ]
