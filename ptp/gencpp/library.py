#
#    Copyright (C) 2011-2013 Stanislav Bohm
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
        builder.line("void set_parameter_{0.name}({0.type} value)", p)
        builder.block_begin()
        builder.line("param::{0.name}.__set_value(value);", p)
        builder.block_end()

def write_library(builder, header_filename):
    builder.line("#include \"{0}\"", header_filename)
    builder.emptyline()
    buildnet.write_core(builder)
    write_library_functions(builder)
    buildnet.write_user_functions(builder)

def get_library_function_declaration(net):
    inputs = net.get_input_places()
    input_names = set(place.interface_input for place in inputs)
    outputs = [ place for place in net.get_output_places()
                if place.interface_output not in input_names ]
    return  ",".join([ "{0} &{1}".format(place.type, place.interface_input) for place in inputs ] +
           [ "{0} &{1}".format(place.type, place.interface_output) for place in outputs ])

def write_library_header_file(builder):
    build.write_header_file(builder, close_guard=False)
    builder.emptyline()
    builder.line("void calib_init(int argc, char **argv);")
    for p in builder.project.get_parameters():
        builder.line("void set_parameter_{0.name}({0.type} {0.name});", p)

    for net in builder.project.nets:
        builder.line("void {0}({1});",
                 net.name,
                 get_library_function_declaration(net))
    build.write_header_file_close_guard(builder)

def write_library_init_function(builder):
    builder.line("void calib_init(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder)
    builder.block_end()

def write_library_function(builder, net, rpc=False):
    if rpc:
        args = builder.expand("void *$data, ca::Packer &$packer")
    else:
        args = get_library_function_declaration(net)

    builder.line("void {0}({1})", net.name, args)
    builder.block_begin()

    if rpc:
        builder.line("ca::Unpacker $unpacker($data);")

    builder.line("ca::spawn_net({0});", net.get_index())
    builder.line("Net_{0} *$n = (Net_{0}*)ca::get_main_net();", net.id)

    for place in net.get_input_places():
        if rpc:
            builder.block_begin()
            builder.line("ca::Token<{0.type} > *$token = new ca::Token<{0.type}>();", place)
            builder.line("ca::unpack($unpacker, $token->value);")
            builder.line("$n->place_{0.id}.add_token($token);", place)
            builder.block_end()
        else:
            builder.line("$n->place_{0.id}.add({0.interface_input});", place)

    builder.line("$n->set_manual_delete();")
    builder.line("ca::main();")

    for place in net.get_output_places():
        builder.if_begin("$n->place_{0.id}.is_empty()", place)
        builder.line('fprintf(stderr, "Token in output place of net {0} not found. Aborting.\\n");',
            net.get_name())
        builder.line("exit(-1);")
        builder.block_end()

        if rpc:
            builder.line("ca::pack($packer, $n->place_{0.id}.begin()->value);", place)
        else:
            builder.line("{0.interface_output} = $n->place_{0.id}.begin()->value;", place)

    builder.line("delete $n;")
    builder.block_end()


