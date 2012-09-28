#
#    Copyright (C) 2012 Stanislav Bohm
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

import buildnet
import build
import library

def write_client(builder, header_filename):
    builder.line("#include \"{0}\"", header_filename)
    builder.line("#include <caclient.h>")
    builder.line("static CaClient client;")
    builder.emptyline()
    build.write_types(builder)
    for net in builder.project.get_modules():
        builder.line("static int __{0.name}_id;", net)
        write_client_library_function(builder, net)
        builder.emptyline()

    write_client_init_function(builder)

def write_client_init_function(builder):

    builder.line("void calib_init(int argc, char **argv)")
    builder.block_begin()
    builder.line("client.connect();")

    for net in builder.project.get_modules():
        name = builder.emitter.const_string(net.name)
        defs = builder.emitter.const_string(
            library.emit_library_function_declaration(builder.emitter, net))
        builder.line("client.register_function({0}, {1}, &__{2}_id);", name, defs, net.name)

    builder.block_end()

def write_client_library_function(builder, net):
    builder.line("void {0}({1})",
                 net.name,
                 library.emit_library_function_declaration(builder.emitter, net, "___"))
    builder.block_begin()

    context = net.get_interface_context()

    builder.line("CaPacker packer(CA_PACKER_DEFAULT_SIZE, CA_RESERVED_CALL_PREFIX);")

    for name in library.get_library_input_arguments(net):
        builder.line("{0};",
                     build.get_pack_code(builder.project,
                                         context[name],
                                         "packer", "___" + name))

    builder.line("CaUnpacker unpacker = client.call(__{0}_id, packer);", net.name)

    for name in library.get_library_output_arguments(net):
        builder.line("___{0} = {1};",
                     name,
                     build.get_unpack_code(builder.project, context[name], "unpacker"))

    builder.block_end()


def write_server(builder):
    build.write_header(builder)
    builder.line("#include <caserver.h>")
    build.write_types_declaration(builder)
    buildnet.write_core(builder)
    library.write_library_functions(builder)

    for net in builder.project.get_modules():
        write_library_function_wrapper(builder, net)

    write_server_main(builder)

def write_server_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    builder.line("calib_init(argc, argv);")
    builder.line("CaServer server;")

    for net in builder.project.get_modules():
        declaration = library.emit_library_function_declaration(builder.emitter, net)
        builder.line("server.register_function({0},{1},{2}_wrapper);",
                  builder.emitter.const_string(net.name),
                  builder.emitter.const_string(declaration),
                  net.name)
    builder.line("server.run();")
    builder.line("return 0;")
    builder.block_end()

def write_library_function_wrapper(builder, net):
    builder.line("CaPacker {0}_wrapper(void *buffer)", net.name)
    builder.block_begin()

    builder.line("CaUnpacker unpacker(buffer);")

    context = net.get_interface_context()

    for name in library.get_library_input_arguments(net):
        builder.line("{2} ___{0} = {1};",
                  name,
                  build.get_unpack_code(builder.project, context[name], "unpacker"),
                  builder.emit_type(context[name]))

    for name in library.get_library_output_only_arguments(net):
        builder.line("{1} ___{0};", name, builder.emit_type(context[name]))

    args = ",".join("___" + name for name, _ in library.get_library_function_arguments(net))
    builder.line("{0}({1});", net.name, args)

    builder.line("CaPacker packer(CA_PACKER_DEFAULT_SIZE, sizeof(size_t));")

    for name in library.get_library_output_arguments(net):
        builder.line("{0};", build.get_pack_code(builder.project,
                                                 context[name],
                                                 "packer", "___" + name))
    builder.line("return packer;")
    builder.block_end()
