import sys
import xml.etree.ElementTree as xml

def logfile_names(filename, count):
	return [ filename + "." + str(i) for i in xrange(count) ]


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
	return (f, report)

def main():
	if len(sys.argv) != 2:
		print "Usage: <logname_prefix>"
		return
	basename = sys.argv[1]
	header, pcount = initial_read(basename + ".0")
	files = []
	output = open(basename + ".klog", "w")
	output.write(header)
	for filename in logfile_names(basename, pcount):
		f, report = open_logile(filename)
		output.write(report)
		files.append(f)

	output.close()
	for f in files:
		f.close()

if __name__ == "__main__":
	main()
