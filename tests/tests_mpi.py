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

    def test_rpc(self):
        p = Project("rpc", mpi=True,rpc=True)
        p.build_main()
        p.start_server()
        try:
            p.run_main("2000 31 31 9000 29700\n", repeat=3)
        finally:
            p.stop_server()

if __name__ == '__main__':
    unittest.main()
