# -*- coding: utf-8 -*-

from testutils import RunProgram
from unittest import TestCase
import unittest
import os

KAIRA_TESTS = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_TESTS)
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
KAIRA_TOOLS = os.path.join(KAIRA_ROOT,"tools")

PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CAILIE_DIR = os.path.join(KAIRA_ROOT, "lib")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

TEST_PROJECTS = os.path.join(KAIRA_TESTS, "projects")

class BuildTest(TestCase):

	def build(self, filename, final_output, params = [], make_args = [], program_name = None):
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
		args = [ CMDUTILS, "--export", filename ]
		RunProgram("python", args).run()
		RunProgram(PTP_BIN, [ name + ".xml", "--build", name + ".cpp" ]).run()
		RunProgram("make", make_args, cwd = directory).run()
		if program_name is None:
			program_name = name
		else:
			program_name = os.path.join(directory, program_name)
		return RunProgram(program_name, params, cwd = directory).run(final_output)

	def failed_ptp(self, filename, final_output):
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
		RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
		RunProgram(PTP_BIN, [ name + ".xml", name + ".cpp" ]).fail(final_output)

	def failed_make(self, filename, stderr, make_args = []):
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
		RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
		RunProgram(PTP_BIN, [ name + ".xml", name + ".cpp" ]).run()
		RunProgram("make", make_args, cwd = directory).fail(expected_stderr_prefix = stderr)

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

	def test_eguards(self):
		self.build(os.path.join(TEST_PROJECTS, "eguards", "eguards.proj"), "3\n")

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
		output = self.build(os.path.join(TEST_PROJECTS, "workers", "workers.proj"), None, params + [ "--threads=3" ])
		check_output(output)
		p = params + [ "--threads=10" ]
		path = os.path.join(TEST_PROJECTS, "workers")
		program_name = os.path.join(path, "workers")
		for x in xrange(100):
			output = RunProgram(program_name, p, cwd = path).run(None)
			check_output(output)

	def test_functions(self):
		self.build(os.path.join(TEST_PROJECTS, "functions", "functions.proj"), "9 9\n")

	def test_tuples(self):
		self.build(os.path.join(TEST_PROJECTS, "tuples", "tuples.proj"), "Ok\n")

	def test_log(self):
		self.build(os.path.join(TEST_PROJECTS, "log", "log.proj"), "", make_args = [ "debug" ], program_name="log_debug")
		directory = os.path.join(TEST_PROJECTS, "log")
		RunProgram(os.path.join(KAIRA_TOOLS, "logmerge.py"), [ "log1" ], cwd = directory).run()
		RunProgram(os.path.join(KAIRA_TOOLS, "logmerge.py"), [ "log2" ], cwd = directory).run()
		self.assertTrue(os.path.isfile(os.path.join(TEST_PROJECTS, "log", "log1.klog")))
		self.assertTrue(os.path.isfile(os.path.join(TEST_PROJECTS, "log", "log2.klog")))

	def test_build(self):
		self.build(os.path.join(TEST_PROJECTS, "build", "build.proj"), "1: 10\n2: 20\n")

	def test_getmore(self):
		self.build(os.path.join(TEST_PROJECTS, "getmore", "getmore.proj"), "Ok 7 13\n")

	def test_broken_userfunction(self):
		self.failed_make(os.path.join(TEST_PROJECTS, "broken", "broken_userfunction.proj"), "*106/user_function:")

	def test_broken_externtype_function(self):
		self.failed_make(os.path.join(TEST_PROJECTS, "broken", "broken_externtype_function.proj"), "*MyType/getsize"), 

	def test_multicast(self):
		self.build(os.path.join(TEST_PROJECTS, "multicast", "multicast.proj"), "18000\n")

if __name__ == '__main__':
    unittest.main()
