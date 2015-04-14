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


import gtksourceview2 as gtksource
import gtk
import pango

class Buffer(gtksource.Buffer):

    def __init__(self):
        gtksource.Buffer.__init__(self)
        self.init_tags()

    def init_tags(self):
        tag_table = self.get_property("tag_table")
        error_tag = gtk.TextTag("error")
        error_tag.set_property("underline-set", True)
        error_tag.set_property("underline", pango.UNDERLINE_ERROR)
        warning_tag = gtk.TextTag("warning")
        warning_tag.set_property("underline-set", True)
        warning_tag.set_property("underline",pango.UNDERLINE_SINGLE)
        tag_table.add(error_tag)
        tag_table.add(warning_tag)

    def get_all_text(self):
        return self.get_text(self.get_start_iter(), self.get_end_iter())

    def set_tag_at(self, tag_name, line, start_col, end_col):
        iter = self.get_iter_at_line(line)
        iter.set_line_offset(start_col)

        enditer = iter.copy()
        if end_col is not None:
            if end_col >= start_col:
                enditer.set_line_offset(end_col)
        else:
            enditer.forward_visible_word_ends(1)
        self.apply_tag_by_name(tag_name, iter, enditer)

    def get_cursor_position(self):
        return self.get_property("cursor-position")

    def get_cursor_position_tuple(self):
        position = self.get_cursor_position()
        iter = self.get_iter_at_offset(position)
        line = iter.get_line() + 1
        pos = iter.get_line_offset() + 1
        return (line, pos)
