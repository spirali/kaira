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
import codeedit

class SimRunConfig(gtk.VBox):

    def __init__(self, app, project):
        gtk.VBox.__init__(self)
        self.project = project

        hbox = gtk.HBox()
        self.pack_start(hbox, False, False)
        hbox.pack_start(gtk.Label("Predefined models:"), False, False, 5)
        button = gtk.Button("Linear")
        button.connect("clicked", lambda w: self.set_hockney())
        hbox.pack_start(button, False, False)

        header = "ca::IntTime packet_time(" \
                 "casr::Context &ctx, int source_id, int target_id, size_t size)\n"

        content = project.communication_model_code
        if content.strip() == "":
            content = "\t\n"

        self.editor = codeedit.CodeEditor(
            project.get_syntax_highlight_key(),
             [ ("", header + "{\n", content, "}\n") ])
        self.pack_start(self.editor, True, True)
        self.editor.grab_focus()
        self.editor.buffer_changed = self.buffer_changed
        self.show_all()

    def buffer_changed(self):
        self.project.communication_model_code = self.editor.get_text()

    def set_hockney(self):
        self.editor.set_text("\tconst ca::IntTime latency = 2e6;  // [ns]\n"
                             "\tconst ca::IntTime bandwidth = 20; // [byte/ns]\n"
                             "\treturn latency + size / bandwidth;\n");
