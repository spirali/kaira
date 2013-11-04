#
#    Copyright (C) 2010 Stanislav Bohm
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

from parameters import ParametersWidget
from build import BuildOptionsWidget

import gtk


class LibraryConfig(gtk.VBox):

    def __init__(self, project):
        def set_rpc(w):
            project.library_rpc = w.get_active()
        def set_octave(w):
            project.library_octave = w.get_active()

        gtk.VBox.__init__(self)
        self.project = project

        button = gtk.CheckButton("Build library in RPC mode")
        button.set_active(project.library_rpc)
        button.connect("toggled", set_rpc)
        self.pack_start(button, False, False)

        button = gtk.CheckButton("Build Octave module")
        button.set_active(project.library_octave)
        button.connect("toggled", set_octave)
        self.pack_start(button, False, False)
        self.show()


class ProjectConfig(gtk.Notebook):

    def __init__(self, app):
        gtk.Notebook.__init__(self)
        self.set_tab_pos(gtk.POS_LEFT)

        w = LibraryConfig(app.project)
        self.append_page(w, gtk.Label("Library"))

        w = ParametersWidget(app.project, app.window)
        self.append_page(w, gtk.Label("Parameters"))

        w = BuildOptionsWidget(app.project, app)
        self.append_page(w, gtk.Label("Build"))

        self.show_all()
