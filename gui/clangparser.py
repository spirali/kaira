#
#    Copyright (C) 2014 Jan Homola
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


import clang.cindex as clang
from ptp import base
import os

library_path = "/usr/lib/x86_64-linux-gnu"
temp_file_path = "/tmp/"
temp_file_name = "kairaclangtemp.cpp"

if os.path.exists(temp_file_path):
    if not os.path.isfile(temp_file_path + temp_file_name):
        file(temp_file_path + temp_file_name, "w+", 1)

if not clang.Config.loaded:
    clang.Config.set_library_file(library_path + "/libclang.so.1")

class ClangParser():

    def __init__(self, completion):
        self.view = completion.view
        self.completion = completion
        self.index = clang.Index.create()
        self.args = ["-I" + self.completion.project.get_directory()]
        self.tu = None
        self.options = (clang.TranslationUnit.PARSE_PRECOMPILED_PREAMBLE |
                       clang.TranslationUnit.PARSE_INCLUDE_BRIEF_COMMENTS_IN_CODE_COMPLETION |
                       clang.TranslationUnit.PARSE_CACHE_COMPLETION_RESULTS)

        def default_parsing_func():
            raise Exception("No parsing function found")

        self.parse_function = default_parsing_func
        self.element = None
        self.element_header = None
        self.invisible_code = ["#include \"" +
                               os.path.join(base.paths.KAIRA_ROOT,base.paths.CAILIE_INCLUDE_DIR, "cailie.h")
                               + "\"\n"]
        self.line_offset = 1
        self.data = None
        self.file = None
        self.type = None
        self.invisible_code_line_count = 1
        self._load_header()

    def get_invisible_string(self):
        return ''.join(self.invisible_code)

    def get_header_line_offset(self):
        return self.line_offset

    def get_invisible_code_line_count(self):
        lines = ''.join(self.invisible_code)
        return lines.count("\n")

    def _load_header(self):
        #TODO:specific exception PtpException
        header = None
        try:
            generator = self.completion.project.get_generator()
            header = generator.get_param_struct()
        except Exception, e:
            self.completion.app.window.console.write(e.message, "error")

        if header:
            self.invisible_code.append(header + "\n")
            self.line_offset += header.count("\n") + 1

    def _data_from_buffer(self):
        #code for head
        invisible_code = ''.join(self.invisible_code)
        return ''.join([invisible_code, self.element_header, self.completion.code_editor.get_text("")])

    def _data_from_node(self):
        #code for place or transition
        header_comment = self.completion.project.get_head_comment()
        head_code = self.completion.project.get_head_code()
        code = self.completion.code_editor.get_text("")
        invisible_code = ''.join(self.invisible_code)
        return ''.join([invisible_code, header_comment, head_code,
                        "\n", self.element_header, "{\n", code, "}\n"])

    def _data_from_label(self):
        #code for Type attribute in place node
        header_comment = self.completion.project.get_head_comment()
        head_code = self.completion.project.get_head_code()
        code = self.completion.code_editor.get_text("")
        invisible_code = ''.join(self.invisible_code)
        return ''.join([invisible_code, header_comment, head_code, "\n", code, "\n"])

    def get_line_offset(self):
        head_comment = self.completion.project.get_head_comment()
        head_code = self.completion.project.get_head_code()
        all_head = head_comment + head_code
        line_offset = all_head.count("\n")
        return line_offset

    def set_line_offset(self, offset):
        self.line_offset += offset

    def set_type(self, header, element = None):
        if header and not element:
            self.element_header = header
            self.parse_function = self._data_from_buffer
            self.type = "header"
        elif header and element:
            self.element = element
            self.element_header = header
            self.parse_function = self._data_from_node
            self.set_line_offset(self.get_line_offset() + 1)
            self.type = "node"
        elif not header and not element:
            self.parse_function = self._data_from_label
            self.set_line_offset(self.get_line_offset() + 1)
            self.type = "type"

    def get_file_and_data(self):
        file = ""
        data = ""

        try:
            file = temp_file_path + temp_file_name
            data = self.parse_function()
        except Exception, e:
            data = ""
            self.completion.app.window.console.write(e.message + "\n", "error")
        return file, data

    def parse(self):
        self.file,self.data = self.get_file_and_data()
        unsaved_files = [(self.file, self.data)]
        self.tu = self.index.parse(self.file, self.args, unsaved_files, self.options)
        return self.tu

    def reparse(self):
        if self.tu:
            self.file, self.data = self.get_file_and_data()
            unsavedFiles = [(self.file, self.data)]
            self.tu.reparse(unsavedFiles, options = self.options)

    def reparse_tu(self, data):
        if self.tu:
            unsavedFiles = [(self.file, data)]
            self.tu.reparse(unsavedFiles, self.options)

    def get_translation_unit(self):
        return self.tu

    def code_complete(self, line, col):
        if self.tu and self.data and self.file:
            unsaved_files = [(self.file, self.data)]

            if line > 0 and col > 0:
                #macros,snippets,brief comments
                return self.tu.codeComplete(self.file, line + self.line_offset,
                                             col, unsaved_files, False, True, True)
            else:
                return None
        else:
            self.completion.parse_source_code()
            unsaved_files = [(self.file, self.data)]

            if line > 0 and col > 0:
                return self.tu.codeComplete(self.file, line + self.line_offset,
                                             col, unsaved_files, False, True, True)
