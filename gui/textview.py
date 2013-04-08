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

import gtk


class TextViewWithLinks(gtk.TextView):
    """
        GtkTextView with support of clickable links
    """

    def __init__(self):
        gtk.TextView.__init__(self)
        self.id_counter = 0
        self.link_callbacks = {}
        self.set_editable(False)
        self.connect("button-press-event", self._button_down)
        self.connect("motion_notify_event", self._mouse_move)
        self.buffer = self.get_buffer()
        self.buffer.create_tag("normal")
        self.link_tag = self.buffer.create_tag("link", underline=True)
        self.link_hidden_tag = self.buffer.create_tag("link_hidden", invisible = True)

    def write(self, text, tag_name="normal"):
        self.buffer.insert_with_tags_by_name(self.buffer.get_end_iter(), text, tag_name)

    def create_tag(self, *args, **kw):
        self.buffer.create_tag(*args, **kw)

    def reset(self):
        self.buffer.set_text("")
        self.id_counter = 0
        self.link_callbacks = {}

    def write_link(self, text, callback):
        new_id = str(self.id_counter)
        self.link_callbacks[new_id] = callback
        self.id_counter += 1
        self.write(text, "link")
        self.write(new_id, "link_hidden")

    def _iter_at_position(self, px, py):
        px, py = self.window_to_buffer_coords(
            gtk.TEXT_WINDOW_WIDGET, int(px), int(py))
        return self.get_iter_at_location(px, py)

    def _button_down(self, w, event):
        i = self._iter_at_position(event.x, event.y)
        if i.has_tag(self.link_tag):
            i.forward_to_tag_toggle(self.link_tag)
            j = i.copy()
            j.forward_to_tag_toggle(self.link_hidden_tag)
            self.link_callbacks[self.buffer.get_text(i, j, True)]()
            return True
        else:
            return False

    def _mouse_move(self, w, event):
        i = self._iter_at_position(event.x, event.y)
        if i.has_tag(self.link_tag):
            cursor = gtk.gdk.Cursor(gtk.gdk.FLEUR)
        else:
            cursor = None
        w = self.get_window(gtk.TEXT_WINDOW_TEXT)
        w.set_cursor(cursor)
