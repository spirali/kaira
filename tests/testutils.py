
import subprocess

def run_program(filename, parameters = []):
	return subprocess.check_output( [filename] + parameters, stderr=subprocess.STDOUT)

class RunProgram:

	def __init__(self, filename, parameters = [], cwd = None):
		self.filename = filename
		self.parameters = parameters
		self.cwd = cwd
	
	def run(self, expected_output = None):
		pr = subprocess.Popen([self.filename] + self.parameters, stdout=subprocess.PIPE, stderr=subprocess.PIPE, cwd = self.cwd)
		output,errs = pr.communicate()
		if errs != "":
			self.error("Stderr is not empty but: %s" % errs)
		if pr.returncode != 0:
			self.error("Error code is not zero")
		if expected_output is not None:
			if output != expected_output:
				self.error("Excepted %s, got %s" % (expected_output, output))

	def error(self, text):
		raise Exception("Invalid output '%s/%s': %s" % (self.filename, self.parameters, text))


	
