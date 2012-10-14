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
    class_name = buildnet.get_net_class_name(net)
    builder.write_class_head(class_name, "cass::Net")

    # Defualt constructor
    builder.write_constructor(class_name,"",[ "cass::Net()" ])
    builder.write_method_end()

    place_decls = [ ("place_" + str(place.id),
                     "cass::Place<{0} >".format(builder.emit_type(place.type)))
                    for place in net.places ]

    # Copy constructor
    builder.write_constructor(class_name,
                              "{0} &net".format(class_name),
                              [ "cass::Net()" ] +
                                  [ "{0}(net.{0})".format(name) for name, t in place_decls ])
    builder.write_method_end()

    for name, t in place_decls:
        builder.write_var_decl(name, t)

def write_net_class_extension(builder, net):
    class_name = buildnet.get_net_class_name(net)
    builder.write_method_start("bool is_equal(const cass::Net &rhs) const")
    if net.places:
        builder.line("{0} *net = ({0}*) &rhs;", class_name);
        conditions = []
        for place in net.places:
            conditions.append("(place_{0.id} == net->place_{0.id})".format(place))
        builder.line("return {0};", " && ".join(conditions))
    else:
        builder.line("return true;")
    builder.write_method_end()

    builder.write_method_start("cass::Net * copy()")
    builder.line("{0} *net = new {0}(*this);", class_name)
    builder.line("return net;")
    builder.write_method_end()

def write_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder)
    builder.line("cass::Core core;")
    builder.line("core.generate();")
    builder.line("core.verify();")
    builder.line("return 0;")
    builder.block_end()

def write_statespace_program(builder):
    builder.thread_class = "cass::Thread"
    builder.generate_operator_eq = True

    build.write_header(builder)
    builder.line("#include <caverif.h>")
    build.write_types_declaration(builder)
    write_core(builder)
    write_main(builder)

def write_spawn(builder, net):
    builder.line("CaNetBase * spawn_{0.id}(CaThreadBase *thread, CaNetDef *def) {{", net)
    builder.indent_push()
    builder.line("{0} *net = new {0}();", buildnet.get_net_class_name(net))
    buildnet.write_init_net(builder, net)
    builder.line("return net;")
    builder.block_end()

def write_net_functions(builder, net):
    write_spawn(builder, net)

    for tr in net.transitions:
        buildnet.write_transition_functions(builder,
                                            tr,
                                            locking=False,
                                            use_get_net=True)
