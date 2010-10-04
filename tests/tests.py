
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

	def build(self, filename, final_output):
		RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")]).run()
		name, ext = os.path.splitext(filename)
		directory = os.path.dirname(name)
		RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
		RunProgram(PTP_BIN, [ name + ".xml", name + ".cpp" ]).run()
		RunProgram("make", [], cwd = directory).run()
		RunProgram(name, []).run(final_output)

	def test_build(self):
		self.build(os.path.join(TEST_PROJECTS, "helloworld2.proj"), "Hello world 5\n")


if __name__ == '__main__':
    unittest.main()
