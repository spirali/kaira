
import subprocess

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

	def mpirun(self, process_count, thread_count, expected_output = None):
		pr = subprocess.Popen( ["mpirun -np {0} {1} -t {2}".format(process_count, self.filename, thread_count)] + self.parameters, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd = self.cwd, shell=True)
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
		if expected is not None:
			if not f(expected, output):
				self.error("Excepted >>{0}<<, got >>{1}<<".format(expected, output))

	def error(self, text):
		raise Exception("Program '%s/%s': %s" % (self.filename, self.parameters, text))


	
