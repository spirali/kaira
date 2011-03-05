import sys
import xml.etree.ElementTree as xml

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

def copy_until_time(output, f):
	line = f.readline()
	while line and not line[0].isdigit():
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
				times[i] = copy_until_time(output, files[i])
		time = min(times)

def main():
	if len(sys.argv) != 2:
		print "Usage: <logname_prefix>"
		return
	basename = sys.argv[1]
	header, pcount = initial_read(basename + ".0")
	files = []
	times = []
	output = open(basename + ".klog", "w")
	output.write(header)
	base_time = 0
	for filename in logfile_names(basename, pcount):
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

if __name__ == "__main__":
	main()
