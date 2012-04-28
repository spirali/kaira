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
from externtypes import ExternTypesWidget
from functions import FunctionsWidget
from build import BuildOptionsWidget
from packages import PackagesWidget

import gtk
import gtkutils

class GeneralConfig(gtk.VBox):

    def __init__(self, project):
        gtk.VBox.__init__(self)
        self.project = project

        if project.is_library():
            frame = gtk.Frame("Target")
            frame.set_border_width(10)
            self.pack_start(frame, False, False)
            vbox = gtk.VBox()
            frame.add(vbox)
            gtkutils.radio_buttons([
                ("lib", "C++ library"),
                ("rpc-lib", "C++ library with RPC"),
                ("octave", "Octave plugin"),
                ("rpc-octave", "Octave plugin with RPC")
            ], project.get_target_mode(), vbox,
                lambda key: project.set_target_mode(key))

        self.show()

class ProjectConfig(gtk.Notebook):

    def __init__(self, app):
        gtk.Notebook.__init__(self)
        self.set_tab_pos(gtk.POS_LEFT)

        w = GeneralConfig(app.project)
        self.append_page(w, gtk.Label("General"))

        w = PackagesWidget(app.project)
        self.append_page(w, gtk.Label("Packages"))

        w = ParametersWidget(app.project, app.window)
        self.append_page(w, gtk.Label("Parameters"))

        w = ExternTypesWidget(app.project, app)
        self.append_page(w, gtk.Label("Extern types"))

        w = FunctionsWidget(app.project, app)
        self.append_page(w, gtk.Label("Functions"))

        w = BuildOptionsWidget(app.project, app)
        self.append_page(w, gtk.Label("Build"))

        self.show_all()
