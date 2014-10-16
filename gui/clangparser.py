#!/usr/bin/env python
# -*- coding: utf-8 -*-

import clang.cindex as clang
from ptp import base
import os

libraryPath = "/usr/lib/x86_64-linux-gnu"
tempfilepath = "/tmp/"
tempfilename = "kairaclangtemp.cpp"

if os.path.exists(tempfilepath):
    if not os.path.isfile(tempfilepath + tempfilename):
        file(tempfilepath+tempfilename,"w+",1)

if not clang.Config.loaded:
    clang.Config.set_library_file(libraryPath + "/libclang.so.1")

class ClangParser():

    def __init__(self, completion):
        self.view = completion.view
        self.completion = completion
        self.index = clang.Index.create()
        self.args = ["-I" + libraryPath + "/clang/3.4/include/"]
        self.tu = None
        self.options = (clang.TranslationUnit.PARSE_PRECOMPILED_PREAMBLE |
		       clang.TranslationUnit.PARSE_DETAILED_PROCESSING_RECORD |
                       clang.TranslationUnit.PARSE_INCLUDE_BRIEF_COMMENTS_IN_CODE_COMPLETION |
                       clang.TranslationUnit.PARSE_CACHE_COMPLETION_RESULTS)
        self.parseFunction = None
        self.element = None
        self.elementheader = None
        self.cailielib = "#include \""+os.path.join(base.paths.KAIRA_ROOT,base.paths.CAILIE_INCLUDE_DIR,"cailie.h") + "\"\n"
        self.lineoffset = 1
        self.data = None
        self.file = None
        self.type = None
        self.libCount = 1

    def _data_from_buffer(self):
        return ''.join([self.cailielib,self.elementheader,self.completion.codeeditor.get_text("")])

    def _data_from_node(self):
        headheader = self.completion.project.get_head_comment()
        headcode = self.completion.project.get_head_code()
        code = self.completion.codeeditor.get_text("")
        return ''.join([headheader,self.cailielib,headcode,self.elementheader,"{\n",code,"}\n"])

    def _data_from_label(self):
        headheader = self.completion.project.get_head_comment()
        headcode = self.completion.project.get_head_code()
        code = self.completion.codeeditor.get_text("")
        return ''.join([headheader,self.cailielib,headcode,"\n",code,"\n"])

    def get_line_offset(self):
        headheader = self.completion.project.get_head_comment()
        headcode = self.completion.project.get_head_code()
        allhead = headheader + headcode
        lineoffset = allhead.count("\n")
        return lineoffset

    def set_line_offset(self, offset):
        self.lineoffset = 1
        self.lineoffset += offset

    def set_type(self, header, element = None):
        if header and not element:
            self.elementheader = header
            self.parseFunction = self._data_from_buffer
            self.type = "header"
        elif header and element:
            self.element = element
            self.elementheader = header
            self.parseFunction = self._data_from_node
            self.set_line_offset(self.get_line_offset())
            self.type = "node"
        elif not header and not element:
            self.parseFunction = self._data_from_label
            self.set_line_offset(self.get_line_offset()+1)
            self.type = "type"

    def get_file_and_data(self):
        file = tempfilepath+tempfilename
        data = self.parseFunction()
        return file, data

    def parse(self):
        self.file,self.data = self.get_file_and_data()
        unsavedFiles = [(self.file,self.data)]
        self.tu = self.index.parse(self.file,self.args,unsavedFiles,self.options)
        return self.tu

    def reparse(self):
        if self.tu:
            self.file,self.data = self.get_file_and_data()
            unsavedFiles = [(self.file,self.data)]
            self.tu.reparse(unsavedFiles,options = self.options)

    def reparse_tu(self, data):
        if self.tu:
            unsavedFiles = [(self.file,data)]
            self.tu.reparse(unsavedFiles,self.options)

    def get_translation_unit(self):
        return self.tu

    def code_complete(self, line, col):
        if self.tu and self.data and self.file:
            unsavedFiles = [(self.file,self.data)]

            if line > 0 and col > 0:
                return self.tu.codeComplete(self.file,line + self.lineoffset,col,unsavedFiles,False,True,False) #macros,snippets,brief comments
            else:
                return None
        else:
            self.completion.parse_source_code()
            unsavedFiles = [(self.file,self.data)]

            if line > 0 and col > 0:
                return self.tu.codeComplete(self.file,line + self.lineoffset,col,unsavedFiles,False,True,False)
