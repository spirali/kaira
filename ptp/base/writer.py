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

class Writer(object):

    filename = None

    def __init__(self):
        self.lines = []
        self.indent = ""

    def raw_line(self, string):
        self.lines.append(self.indent + string)

    def emptyline(self):
        self.lines.append("")

    def line(self, string, *args, **kw):
        self.raw_line(string.format(*args, **kw))

    def indent_push(self):
        self.indent += "\t"

    def indent_pop(self):
        self.indent = self.indent[:-1]

    def add_writer(self, writer):
        writer.write_to_writer(self)

    def raw_text(self, text):
        if len(text) == 0:
            return
        lines = text.split("\n")
        if text[-1] == "\n":
            lines = lines[:-1]
        for line in lines:
            self.raw_line(line)

    def get_string(self):
        return "\n".join(self.lines) + "\n"

    def write_to_file(self, filename=None):
        if filename is None:
            assert self.filename is not None
            filename = self.filename

        with open(filename, "w") as f:
            for line in self.lines:
                f.write(line + "\n")

    def write_to_writer(self, writer):
        for line in self.lines:
            writer.raw_line(line)

    def get_next_line_number(self):
        return len(self.lines) + 1

    def get_current_line_number(self):
        return len(self.lines)
