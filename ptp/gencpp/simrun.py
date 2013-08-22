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

def write_run_configuration(builder):
    # Here we do not use block_begin because we want to supress indentation
    # User function has to start at the first column to obtain
    # correct positions of error messages
    builder.line("class RunConfiguration : public casr::RunConfiguration {{")
    builder.line("public:")
    declaration = \
            "ca::IntTime packet_time(casr::Context &ctx, int origin_id, int target_id, size_t size)"
    builder.write_function(declaration,
                           builder.project.communication_model_code,
                           ("*communication-model", 1))
    builder.line("}};")

def write_clock(builder, tr):
    class_name = "Clock_{0.id}".format(tr)
    builder.write_class_head(class_name, "ca::Clock")
    builder.write_constructor(class_name,
                                "ca::ThreadBase *thread",
                              [ "ctx(thread)" ])
    builder.write_method_end()
    builder.line("ca::IntTime tock();")
    builder.line("protected:")
    builder.line("casr::Context ctx;")
    builder.write_class_end()

def write_clock_tock(builder, tr):
    builder.write_function(
            "ca::IntTime Clock_{0.id}::tock()".format(tr),
            "ca::IntTime clockTime = Clock::tock();\n"
                "return {0};".format(tr.clock_substitution),
            ("*{0.id}/clock-substitution".format(tr), 1))

def write_clocks_tock(builder):
    for net in builder.project.nets:
        for tr in net.transitions:
            if tr.clock_substitution:
                write_clock_tock(builder, tr)

def write_clocks(builder):
    for net in builder.project.nets:
        for tr in net.transitions:
            if tr.clock_substitution:
                write_clock(builder, tr)

def write_main(builder):
    builder.line("int main(int argc, char **argv)")
    builder.block_begin()
    buildnet.write_main_setup(builder, start_process=False)
    builder.line("RunConfiguration run_configuration;")
    builder.line("casr::main(run_configuration);");
    builder.line("return 0;")
    builder.block_end()

def write_simrun_program(builder):
    build.write_header(builder)
    write_clocks(builder)
    buildnet.write_core(builder)
    write_run_configuration(builder)
    write_main(builder)
    buildnet.write_user_functions(builder)
    write_clocks_tock(builder)
