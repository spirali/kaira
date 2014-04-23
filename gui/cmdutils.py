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

import argparse
import sys
import paths
sys.path.append(paths.PTP_DIR)
import loader
import os
import tracelog

def export(filename, directory, trace, lib):
    p = loader.load_project(filename)
    if trace and lib:
        target = "libtraced"
    elif trace:
        target = "traced"
    elif lib:
        target = "lib"
    else:
        target = "release"
    build_config = p.get_build_config(target)
    if directory is not None:
        build_config.directory = directory
    else:
        build_config.directory = os.path.dirname(filename)
    p.export(build_config)

def check_tracelog(filename):
    t = tracelog.TraceLog(filename)
    print t.get_runinstances_count()

def main():
    parser = argparse.ArgumentParser(description='Kaira gui command line controller')
    parser.add_argument('--export', metavar='filename', type=str)
    parser.add_argument('--output', metavar='directory', type=str)
    parser.add_argument("--trace", action='store_true')
    parser.add_argument('--tracelog', metavar='filename', type=str)
    parser.add_argument("--lib", action='store_true')
    args = parser.parse_args()
    if args.export:
        export(os.path.abspath(args.export), args.output, args.trace, args.lib)
        return
    if args.tracelog:
        check_tracelog(args.tracelog)

if __name__ == "__main__":
    main()
