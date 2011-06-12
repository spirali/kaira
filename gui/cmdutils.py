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
import argparse

def export(filename, force_packers):
	p = project.load_project(filename)
	if force_packers:
		p.set_force_packers(True)
	p.write_project_files()
	p.export(p.get_exported_filename())

def main():
	parser = argparse.ArgumentParser(description='Kaira gui command line controller')
	parser.add_argument('--export', metavar='filename', type=str)
	parser.add_argument('--force-packers', action='store_true')
	args = parser.parse_args()
	if args.export:
		export(args.export, args.force_packers)
		return

if __name__ == "__main__":
	main()
