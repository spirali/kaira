#
#    Copyright (C) 2011, 2012 Stanislav Bohm
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
from octave import OctaveBuilder
import emitter
import os
import base.utils
import makefiles

class CppGenerator:

    def __init__(self, project):
        self.project = project

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


class CppProgramGenerator(CppGenerator):

    def build(self, directory):
        source_filename = os.path.join(directory, self.project.get_name() + ".cpp")

        builder = Builder(self.project, source_filename)
        builder.build()
        builder.write_to_file()

        makefiles.write_program_makefile(self.project, directory)



class CppLibGenerator(CppGenerator):

    def build(self, directory):

        if self.project.get_target_mode() == "lib":
            self.build_library(directory)
            makefiles.write_library_makefile(self.project, directory)

        if self.project.get_target_mode() == "rpc-lib":
            self.build_server(directory)
            self.build_client_library(directory)
            makefiles.write_library_makefile(self.project, directory, rpc = True)

        if self.project.get_target_mode() == "octave":
            self.build_library(directory)
            self.build_oct_files(directory)
            makefiles.write_library_makefile(self.project, directory, octave = True)

        if self.project.get_target_mode() == "rpc-octave":
            self.build_server(directory)
            self.build_client_library(directory)
            self.build_oct_files(directory)
            makefiles.write_library_makefile(self.project, directory, rpc = True, octave = True)


    def build_client_library(self, directory):
        source_filename = os.path.join(directory, self.project.get_name() + ".cpp")
        header_filename = os.path.join(directory, self.project.get_name() + ".h")

        # Build .cpp
        builder = Builder(self.project, source_filename)
        builder.build_client_library(self.project.get_name() + ".h")
        builder.write_to_file()

        # Build .h
        builder = Builder(self.project, header_filename)
        builder.build_library_header_file()
        builder.write_to_file()


    def build_server(self, directory):
        server_directory = os.path.join(directory, "server")

        # Check for server directory
        if os.path.exists(server_directory):
            if not os.path.isdir(server_directory):
                raise base.utils.PtpException("'server' exists but it is not directory")
        else:
            os.makedirs(server_directory)

        source_filename = os.path.join(server_directory, self.project.get_name() + "_server.cpp")

        builder = Builder(self.project, source_filename)
        builder.build_server()
        builder.write_to_file()

        makefiles.write_server_makefile(self.project, server_directory)

    def build_library(self, directory):
        source_filename = os.path.join(directory, self.project.get_name() + ".cpp")
        header_filename = os.path.join(directory, self.project.get_name() + ".h")

        # Build .cpp
        builder = Builder(self.project, source_filename)
        builder.build_library(self.project.get_name() + ".h")
        builder.write_to_file()

        # Build .h
        builder = Builder(self.project, header_filename)
        builder.build_library_header_file()
        builder.write_to_file()

    def build_oct_files(self, directory):
        source_filename = os.path.join(directory, self.project.get_name() + "_oct.cpp")
        m_filename = os.path.join(directory, self.project.get_name() + ".m")

        builder = Builder(self.project, source_filename)
        builder.build_oct(self.project.get_name() + ".h")
        builder.write_to_file()

        builder = OctaveBuilder(self.project)
        builder.build_loader(self.project.get_name() + ".oct")
        builder.write_to_file(m_filename)


