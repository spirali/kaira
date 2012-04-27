#
#    Copyright (C) 2012 Stanislav Bohm
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

import gtkutils
import gtk
import os
import paths

class PackagesWidget(gtk.Table):

    def __init__(self, project):
        gtk.Table.__init__(self, 2, 2)
        self.project = project

        self.packages_list = gtkutils.SimpleList([("Available packages", str)])
        self.selected_list = gtkutils.SimpleList([("Selected packages", str)])
        self.attach(self.packages_list, 0, 1, 1, 2)
        self.attach(self.selected_list, 1, 2, 1, 2)

        self.read_available_packages()

        self.packages_list.connect_view("row-activated",
            lambda w, i, p: self.add_package())

        self.selected_list.connect_view("row-activated",
            lambda w, i, p: self.remove_package())


    def read_available_packages(self):
        packages = [ filename[:-5] for filename in os.listdir(paths.PACKAGES_DIR)
            if filename.endswith(".proj")]
        self.packages_list.fill([(package,) for package in packages])

    def add_package(self):
        name = self.packages_list.get_and_remove_selection(0)
        self.selected_list.append((name,))

    def remove_package(self):
        name = self.selected_list.get_and_remove_selection(0)
        self.packages_list.append((name,))
