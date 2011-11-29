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

from builder import Builder, CppWriter
import emitter

class CppGenerator:

    def __init__(self, project):
        self.project = project

    def build(self, output_filename):
        builder = Builder(self.project, output_filename)
        builder.build()
        builder.write_to_file(output_filename)

    def get_place_user_fn_header(self, place_id):
        place = self.project.get_place(place_id)
        t = emitter.Emitter(self.project).emit_type(place.type)
        if t[-1] == ">":
            t += " "
        return "void place_fn(CaContext &ctx, std::vector<{1}> &tokens)\n{{\n".format(place, t)

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
        w.line("void transition_fn(CaContext &ctx, Vars &vars)")
        return w.get_string()
