#
#    Copyright (C) 2012-2013 Stanislav Bohm
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

def write_core(builder):
    build.write_basic_definitions(builder)
    for net in builder.project.nets:
        buildnet.write_net_functions_forward(builder, net)
    for net in builder.project.nets:
        write_net_class(builder, net)
        write_net_class_extension(builder, net)
        builder.write_class_end()
    for net in builder.project.nets:
        write_net_functions(builder, net)

def write_net_class(builder, net):
    buildnet.write_net_class(builder,
                             net,
                             namespace="cass",
                             write_constructor=False,
                             write_class_end=False)
    return

def write_pack_method(builder, net):
    builder.write_method_start("void pack(ca::Packer &packer)")
    for place in net.places:
         builder.line("ca::pack(packer, place_{0.id});", place)
    builder.write_method_end()

def write_net_class_extension(builder, net):
    write_pack_method(builder, net)

def write_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    builder.line("cass::Core core;")
    buildnet.write_main_setup(builder, "cass::init", start_process=False)
    builder.line("core.generate();")
    builder.line("core.postprocess();")
    builder.line("return 0;")
    builder.block_end()

def write_statespace_program(builder):
    builder.thread_class = "cass::Thread"
    builder.generate_all_pack = True

    build.write_header(builder)
    builder.line("#include <caverif.h>")
    write_core(builder)
    write_main(builder)
    buildnet.write_user_functions(builder)

def write_spawn(builder, net):
    builder.line("ca::NetBase * spawn_{0.id}(ca::ThreadBase *$thread, ca::NetDef *$def) {{", net)
    builder.indent_push()
    builder.line("{0} *$net = new {0}();", buildnet.get_net_class_name(net))
    buildnet.write_init_net(builder, net)
    builder.line("return $net;")
    builder.block_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        buildnet.write_transition_functions(builder,
                                            tr,
                                            locking=False)
