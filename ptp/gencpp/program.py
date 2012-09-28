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

def write_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    builder.line("ca_project_description({0});",
        builder.emitter.const_string(builder.project.description))

    params = builder.project.get_parameters()
    names = ",".join((builder.emitter.const_string(p.name) for p in params))
    builder.line("const char *pnames[] = {{{0}}};", names)
    descriptions = ",".join((builder.emitter.const_string(p.description)
                             for p in params))
    builder.line("const char *pdesc[] = {{{0}}};", descriptions)
    pvalues = ",".join(("&__param_" + p.name for p in params))
    builder.line("int *pvalues[] = {{{0}}};", pvalues)
    builder.line("ca_init(argc, argv, {0}, pnames, pvalues, pdesc);", len(params))
    for net in builder.project.nets:
        buildnet.write_register_net(builder, net)
    defs = [ "def_" + str(net.id) for net in builder.project.nets ]
    builder.line("CaNetDef *defs[] = {{{0}}};", ",".join(defs))
    builder.line("ca_setup({0}, defs);", len(defs));
    builder.line("ca_spawn_net(0);");
    builder.line("ca_main();");
    builder.line("return 0;")
    builder.block_end()

def write_standalone_program(builder):
    build.write_header(builder)
    build.write_types_declaration(builder)
    buildnet.write_core(builder)
    write_main(builder)
