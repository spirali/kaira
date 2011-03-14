#!/usr/bin/env python
#
#    Copyright (C) 2011 Stanislav Bohm
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

import sys
import xml.etree.ElementTree as xml
import os

def logfile_names(filename, count):
	return [ filename + "." + str(i) for i in xrange(count) ]

def string_to_time(string):
	if not string:
		return float("inf")
	a, b = string.strip().split(".")
	return int(a) * 1000000000 + int(b)

def initial_read(filename):
	header = []
	with open(filename, "r") as f:
		header.append(f.readline())
		configuration_str = f.readline()
		header.append(configuration_str)
		configuration = xml.fromstring(configuration_str)
		lines = int(configuration.get("description-lines"))
		header += [ f.readline() for i in xrange(lines) ]
	return ("".join(header), int(configuration.get("process-count")))

def open_logile(filename):
	f = open(filename, "r")
	f.readline() # skip firstline
	configuration = xml.fromstring(f.readline())
	lines = int(configuration.get("description-lines"))
	for i in xrange(lines):
		f.readline()
	report = f.readline()	
	initial_time = f.readline()
	return (f, report, string_to_time(initial_time))

def copy_until_time(output, f, process_id):
	line = f.readline()
	while line and not line[0].isdigit():
		if line == "C\n":
			line = "C{0}\n".format(process_id)
		output.write(line)
		line = f.readline()
	return string_to_time(line)

def process_logs(output, files, times, base_time):
	inf = float("inf")
	time = base_time
	while time != inf:
		for i in xrange(len(files)):
			if times[i] == time:
				output.write("{0}\n".format(time - base_time))
				times[i] = copy_until_time(output, files[i], str(i))
		time = min(times)

def main():
	if len(sys.argv) != 2:
		print "Usage: <logname_prefix>"
		return
	basename = sys.argv[1]

	if not os.path.isfile(basename + ".0"):
		print "Log {0}.0 not found".format(basename)
		return

	header, pcount = initial_read(basename + ".0")
	files = []
	times = []
	output = open(basename + ".klog", "w")
	output.write(header)
	base_time = 0
	log_names = logfile_names(basename, pcount)
	for filename in log_names:
		f, report, initial_time = open_logile(filename)
		output.write(report)
		files.append(f)
		times.append(initial_time)

	base_time = min(times)

	times = [ string_to_time(f.readline()) for f in files ]
	process_logs(output, files, times, base_time)

	output.close()
	for f in files:
		f.close()

	for name in log_names:
		os.remove(name)

if __name__ == "__main__":
	main()
