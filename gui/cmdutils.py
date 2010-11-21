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

import project
import sys

def export(filename):
	p = project.load_project(filename)
	p.write_project_files()
	p.export(p.get_exported_filename())

def main(args):
	if len(args) == 2 and args[0] == "--export":
		export(args[1])
		return
	print "Invalid arguments"

if __name__ == "__main__":
	args = sys.argv[1:] # Remove "cmdutils.py"
	main(args)
