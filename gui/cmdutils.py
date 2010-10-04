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
