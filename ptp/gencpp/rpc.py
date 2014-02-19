#
#    Copyright (C) 2012,2013 Stanislav Bohm
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
import library
import writer

def write_client(builder, header_filename):
    builder.line("#include \"{0}\"", header_filename)
    builder.line("#include <caclient.h>")
    builder.line("using namespace caclient;")
    builder.line("static CaClient client;")
    builder.emptyline()
    for net in builder.project.nets:
        builder.line("static int ${0.name}_id;", net)
        write_client_library_function(builder, net)
        builder.emptyline()

    write_client_init_function(builder)

def write_client_init_function(builder):
    builder.line("void calib_init(int argc, char **argv)")
    builder.block_begin()
    builder.line("client.connect();")

    for net in builder.project.nets:
        declaration = library.get_library_function_declaration(net)
        builder.line("client.register_function({0}, {1}, &${2}_id);",
                writer.const_string(net.name),
                writer.const_string(declaration),
                net.name)
    builder.block_end()

def write_client_library_function(builder, net):
    builder.line("void {0}({1})",
                 net.name,
                 library.get_library_function_declaration(net))
    builder.block_begin()

    builder.line("ca::Packer $packer(ca::PACKER_DEFAULT_SIZE, CACLIENT_RESERVED_CALL_PREFIX);")

    for place in net.get_input_places():
        builder.line("ca::pack($packer, {0.interface_input});", place)

    builder.line("ca::Unpacker $unpacker = client.call(${0}_id, $packer);", net.name)

    for place in net.get_output_places():
        builder.line("ca::unpack($unpacker, {0.interface_output});", place)

    builder.block_end()

def write_server(builder):
    builder.line("#include <caserver.h>")
    builder.line("#include \"{0}.h\"", builder.project.get_name())
    builder.emptyline()
    builder.line("using namespace caserver;")
    buildnet.write_core(builder)
    for net in builder.project.nets:
        library.write_library_function(builder, net, rpc=True)
    library.write_library_init_function(builder)
    write_server_main(builder)
    buildnet.write_user_functions(builder)

def write_server_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    builder.line("calib_init(argc, argv);")
    builder.line("CaServer server;")

    for net in builder.project.nets:
        declaration = library.get_library_function_declaration(net)
        builder.line("server.register_function({0},{1},{2});",
                  writer.const_string(net.name),
                  writer.const_string(declaration),
                  net.name)
    builder.line("server.run();")
    builder.line("return 0;")
    builder.block_end()
