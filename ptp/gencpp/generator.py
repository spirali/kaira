#
#    Copyright (C) 2011 Stanislav Bohm
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

from builder import Builder, CppWriter, emit_declarations
import emitter
import os

class CppGenerator:

    def __init__(self, project):
        self.project = project

    def build(self, output_filename):
        builder = Builder(self.project, output_filename)
        builder.build()
        builder.write_to_file(output_filename)
        
    def build_library(self, output_filename):
        head_file, ext = os.path.splitext(output_filename)
        head_file = head_file + ".h"
        
        builder = Builder(self.project, output_filename)
        builder.build_library(os.path.basename(head_file))
        builder.write_to_file(output_filename)
        
        module_head = Builder(self.project, head_file)
        module_head.build_head_file()
        module_head.write_to_file(head_file)

    def get_place_user_fn_header(self, place_id):
        place = self.project.get_place(place_id)
        t = emitter.Emitter(self.project).emit_type(place.type)
        if t[-1] == ">":
            t += " "
        return "void place_fn(CaContext &ctx, std::vector<{1}> &tokens)\n".format(place, t)

    def get_transition_user_fn_header(self, transition_id):
        transition = self.project.get_transition(transition_id)
        context = transition.get_context()
        w = CppWriter()
        em = emitter.Emitter(self.project)
        w.line("struct Vars {{")
        for key, value in context.items():
            w.line("\t{1} {0};", key, em.emit_type(value))
        w.line("}};")
        w.emptyline()
        w.line("void transition_fn(CaContext &ctx, Vars &var)")
        return w.get_string()

    def get_user_function_header(self, ufunction_name):
        ufunction = self.project.get_user_function(ufunction_name)
        em = emitter.Emitter(self.project)
        t = em.emit_type(ufunction.get_returntype())
        if ufunction.with_context:
            ctx = "CaContext &ctx, "
        else:
            ctx = ""
        decls = emit_declarations(em, ufunction.get_parameters())
        return "{0} {1}({2}{3})\n{{\n".format(t, ufunction_name, ctx, decls)
