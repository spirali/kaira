#
#    Copyright (C) 2013 Stanislav Bohm
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
    buildnet.write_main_setup(builder, start_process=False)
    builder.line("casr::main();");
    builder.line("return 0;")
    builder.block_end()

def write_simrun_program(builder):
    build.write_header(builder)
    builder.line("#include <simrun.h>")
    buildnet.write_core(builder)
    write_main(builder)
