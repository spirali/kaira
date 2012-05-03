# -*- coding: utf-8 -*-

from testutils import Project
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

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        Project("libhelloworld", mpi=True).quick_test_main(result)


    def test_overtake(self):
        result = "".join([ "{0}\n".format(x) for x in range(1, 101) ])
        Project("overtake", mpi=True).quick_test_main(result, processes=4, threads=4)

    def test_library_processes(self):
        result = "".join([ "{0}\n".format(x) for x in range(1, 101) ])
        p = Project("overtake", mpi=True)
        p.build_main()
        p.failed_run_main("Net 2 sends 1 token(s) to invalid process id 1 (valid ids: [0 .. 0])\n")
        for x in range(2, 5):
            p.run_main(result, threads=x*x, processes=x*x)

    def test_modules2(self):
        Project("modules2", mpi=True).quick_test("0\n", processes=5, threads=4)

if __name__ == '__main__':
    unittest.main()
