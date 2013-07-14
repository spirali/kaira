#    Copyright (C) 2011-2013 Stanislav Bohm
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

from project import Project

class ProjectCpp(Project):

    def __init__(self, file_name):
        Project.__init__(self, file_name)
        self.build_options = {
            "CFLAGS" : "-O2",
            "LIBS" : ""
        }

    def get_syntax_highlight_key(self):
        """return language for GtkSourceView"""
        return "cpp"

    def get_head_comment(self):
        return "/* The code from 'head' is included at the beginning of generated project.\n" \
               " * The main purpose is to put here definitions (#includes, classes, ...)\n" \
               " * that can be used everywhere. (functions in transitions and places).\n" \
               " */\n\n"

    def get_source_file_patterns(self):
        return ["*.cpp", "*.cc", "*.c"]

    @classmethod
    def get_target_env_name(self):
        return "C++"
