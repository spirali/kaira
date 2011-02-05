
from testutils import RunProgram
from unittest import TestCase
import unittest
import os

KAIRA_TESTS = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_TESTS)
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")

PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp")
CAILIE_DIR = os.path.join(KAIRA_ROOT, "lib")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

TEST_PROJECTS = os.path.join(KAIRA_TESTS, "projects")

class BuildingTest(TestCase):

	def build(self, filename, final_output, params = []):
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
		RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
		RunProgram(PTP_BIN, [ name + ".xml", name + ".cpp" ]).run()
		RunProgram("make", [], cwd = directory).run()
		return RunProgram(name, params).run(final_output)

	def failed_ptp(self, filename, final_output):
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
		RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
		RunProgram(PTP_BIN, [ name + ".xml", name + ".cpp" ]).fail(final_output)

	def test_helloworld(self):
		self.build(os.path.join(TEST_PROJECTS, "helloworlds", "helloworld.proj"), "Hello world 12\n")
	def test_helloworld2(self):
		self.build(os.path.join(TEST_PROJECTS, "helloworlds", "helloworld2.proj"), "Hello world 5\n")
	def test_strings(self):
		self.build(os.path.join(TEST_PROJECTS, "strings", "strings.proj"), "String\nOk\nOk\nOk\nOk\n")
	def test_externtypes(self):
		output = "10 20\n107 207\n10 20\n257 77750 A looong string!!!!!\n"
		self.build(os.path.join(TEST_PROJECTS, "externtypes", "externtypes.proj"), output)
	def test_packing(self):
		output = "0\n1\n2\n3\n4\n0\n1\n2\n3\n4\n5\n5\n6\n7\n8\n9\n100\n100\n"
		self.build(os.path.join(TEST_PROJECTS, "packing", "packing.proj"), output)
	def test_broken1(self):
		self.failed_ptp(os.path.join(TEST_PROJECTS, "broken", "broken1.proj"), "*104/inscription:1:Inscription is empty\n")
	def test_broken2(self):
		self.failed_ptp(os.path.join(TEST_PROJECTS, "broken", "broken2.proj"), "*102/type:1:Type is empty\n")
	def test_parameters(self):
		self.build(os.path.join(TEST_PROJECTS, "parameters", "parameters.proj"), "9 7\n", ["-pfirst=10", "-psecond=7"])
	def test_bidirection(self):
		self.build(os.path.join(TEST_PROJECTS, "bidirection", "bidirection.proj"), "11\n12\n13\n")
	def test_scheduler(self):
		output = self.build(os.path.join(TEST_PROJECTS, "scheduler", "scheduler.proj"), None)
		d = { "First" : 0, "Second" : 0 }
		for line in output.split("\n"):
			if line:
				d[line] += 1
		self.assertEquals(d, { "First": 5, "Second": 5 })
	def test_workers(self):
		def check_output(output):
			self.assertEquals(76127, sum([ int(x) for x in output.split("\n") if x.strip() != "" ]))
		params = [ "-pLIMIT=1000", "-pSIZE=20" ]
		output = self.build(os.path.join(TEST_PROJECTS, "workers", "workers.proj"), None, params)
		check_output(output)
		output = self.build(os.path.join(TEST_PROJECTS, "workers", "workers.proj"), None, params + [ "--processes=3" ])
		check_output(output)
	def test_functions(self):
		self.build(os.path.join(TEST_PROJECTS, "functions", "functions.proj"), "9 9\n")
	def test_tuples(self):
		self.build(os.path.join(TEST_PROJECTS, "tuples", "tuples.proj"), "Ok\n")

if __name__ == '__main__':
    unittest.main()
