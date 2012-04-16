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


from base.writer import Writer

class OctaveBuilder(Writer):

    def __init__(self, project):
        Writer.__init__(self)
        self.project = project

    def build_loader(self, oct_file):
        for net in self.project.get_modules():
            self.line("autoload(\"{0}\", file_in_loadpath (\"{1}\"))", net.name, oct_file)
