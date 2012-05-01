
import subprocess
import os

KAIRA_TESTS = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_TESTS)
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
KAIRA_TOOLS = os.path.join(KAIRA_ROOT,"tools")

PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CAILIE_DIR = os.path.join(KAIRA_ROOT, "lib")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

TEST_PROJECTS = os.path.join(KAIRA_TESTS, "projects")



def run_program(filename, parameters = []):
	return subprocess.check_output( [filename] + parameters, stderr=subprocess.STDOUT)

class RunProgram:

	def __init__(self, filename, parameters = [], cwd = None):
		self.filename = filename
		self.parameters = parameters
		self.cwd = cwd

	def result(self, pr, expected_output):
		output,errs = pr.communicate()
		if errs != "":
			self.error("Stderr is not empty but: %s" % errs)
		if pr.returncode != 0:
			self.error("Return code is not zero")
		self.check_output(expected_output, output)
		return output

	def run(self, expected_output = None):
		pr = subprocess.Popen([self.filename] + self.parameters, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd = self.cwd)
		return self.result(pr, expected_output)

	def mpirun(self, process_count, expected_output = None):
		pr = subprocess.Popen( ["mpirun -np {0} {1} {2}".format(process_count, self.filename, " ".join(self.parameters))], stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd = self.cwd, shell=True)
		return self.result(pr, expected_output)

	def fail(self, expected_stdout = None, expected_stderr = None, expected_stderr_prefix = None):
		pr = subprocess.Popen([self.filename] + self.parameters, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd = self.cwd)
		output,errs = pr.communicate()
		if pr.returncode == 0:
			self.error("Expected fail, but return code is zero")
		self.check_output(expected_stdout, output)
		self.check_output(expected_stderr, errs)
		self.check_output(expected_stderr_prefix, errs, lambda e, o: o.startswith(e))

	def check_output(self, expected, output, f = lambda a, b: a == b):
		if expected is not None and not f(expected, output):
			self.error("Excepted >>{0}<<, got >>{1}<<".format(expected, output))

	def error(self, text):
		raise Exception("Program '%s/%s': %s" % (self.filename, self.parameters, text))


class Project:

    def __init__(self, name, directory_name = None):
        self.name = name
        if directory_name is None:
            self.directory_name = name
        else:
            self.directory_name = directory_name
        self.clean()

    def get_filename(self):
        return os.path.join(self.get_directory(), self.name + ".proj")

    def get_xml_filename(self):
        return os.path.join(self.get_directory(), self.name + ".xml")

    def get_directory(self):
        return os.path.join(TEST_PROJECTS, self.directory_name)

    def get_executable(self):
        return os.path.join(self.get_directory(), self.name)

    def get_main(self):
        return os.path.join(self.get_directory(), "main")

    def clean(self):
        RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = self.get_directory()).run()

    def export(self):
        RunProgram("python", [ CMDUTILS, "--export", self.get_filename() ]).run()

    def run_ptp(self):
        RunProgram(PTP_BIN, [ self.get_xml_filename(), "--build", self.get_directory() ]).run()

    def fail_ptp(self, output):
        self.export()
        RunProgram(PTP_BIN, [ self.get_xml_filename(), "--build", self.get_directory() ]).fail(output)

    def failed_make(self, output, args = []):
        self.export()
        self.run_ptp()
        RunProgram("make", args, cwd = self.get_directory()).fail(expected_stderr_prefix = output)

    def make(self, args = []):
        RunProgram("make", args, cwd = self.get_directory()).run()

    def build(self):
        self.export()
        self.run_ptp()
        self.make()

    def run(self, *args, **kw):
        self._run(self.get_executable(), *args, **kw)

    def _run(self, executable, result = None, result_fn = None, repeat = 1, processes = 1, threads = 1, params = {}):
        args = [ "-r{0}".format(processes), "--threads={0}".format(threads) ]

        for name in params:
            args.append("-p{0}={1}".format(name, params[name]))

        for x in xrange(repeat):
            r = RunProgram(executable, args, cwd = self.get_directory()).run(result)
            if result_fn is not None:
                result_fn(r)

    def quick_test(self, *args, **kw):
        self.build()
        self.run(*args, **kw)

    def build_main(self):
        self.build()
        self.make(["-f", "makefile.main"])

    def run_main(self, *args, **kw):
        self._run(self.get_main(), *args, **kw)

    def quick_test_main(self, *args, **kw):
        self.build_main()
        self.run_main(*args, **kw)
