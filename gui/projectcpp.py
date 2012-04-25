#    Copyright (C) 2011 Stanislav Bohm
#    Copyright (C) 2011 Ondrej Meca
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
import os
from project import Project, NativeExternType

class ProjectCppBase(Project):

    def __init__(self, file_name):
        Project.__init__(self, file_name)
        self.build_options = {
            "CC" : "g++",
            "CFLAGS" : "-O2",
            "LIBS" : ""
        }

    def create_native_extern_type(self):
        return ExternTypeCpp()

    def get_syntax_highlight_key(self):
        """return language for GtkSourceView"""
        return "cpp"

    def get_head_filename(self):
        return os.path.join(self.get_directory(), "head.cpp")

    def get_initial_head_file_content(self):
        return "/* This file is included at the beginning of the main source file,\n" \
               "   so definitions from this file can be used in functions in\n" \
               "   transitions and places. */\n\n"

    def get_source_file_patterns(self):
        return ["*.cpp", "*.cc", "*.c"]

    @classmethod
    def get_extenv_for_simulator_name(self):
        """ When we run simulator we have to build regular application even
            we are building library """
        return "C++"


class ProjectCpp(ProjectCppBase):

    def __init__(self, file_name):
        ProjectCppBase.__init__(self, file_name)
        self.build_options = {
            "CC" : "g++",
            "CFLAGS" : "-O2",
            "LIBS" : ""
        }

    @classmethod
    def get_extenv_name(self):
        return "C++"

    def is_library(self):
        return False

class ProjectCppLibrary(ProjectCppBase):

    def __init__(self, file_name):
        ProjectCppBase.__init__(self, file_name)
        self.target_mode = "lib"

    @classmethod
    def get_extenv_name(self):
        return "C++ library"

    def is_library(self):
        return True

class ExternTypeCpp(NativeExternType):

    def get_default_function_code(self, name):
        if name == "getstring":
            return "\treturn \"" + self.name + "\";\n"
        else:
            return "\t// Need implementation\n"

    def get_function_declaration(self, name):
        if name == "getstring":
            return "std::string getstring(const " + self.raw_type + " &obj)"
        elif name == "getsize":
            return "size_t getsize(const " + self.raw_type + " &obj)"
        elif name == "pack":
            return "void pack(CaPacker &packer, const " + self.raw_type + " &obj)"
        elif name == "unpack":
            return self.raw_type + " unpack(CaUnpacker &unpacker)"
        elif name == "to_octave_value":
            return "octave_value to_octave_value(const " + self.raw_type + " &obj)"
        elif name == "from_octave_value":
            return ""+self.raw_type+" from_octave_value(const octave_value &obj)"
