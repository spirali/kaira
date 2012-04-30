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

    def run_ptp(self, name):
        return RunProgram(PTP_BIN, [ name + ".xml", "--build", os.path.dirname(name) ])

    def build(self, filename, final_output = None, make_args = [], **kw):
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        make_args = [ "mpi" ] + make_args
        RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
        args = [ CMDUTILS, "--export", filename ]
        RunProgram("python", args).run()
        self.run_ptp(name).run()
        RunProgram("make", make_args, cwd = directory).run()
        if final_output is not None:
            self.run_program(filename, final_output, **kw)

    def build_library(self, filename, final_output, processes = 10, params = [], make_args = []):
        self.build(filename, make_args = make_args)
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        RunProgram("make", [ "-f", "makefile.main", "main_mpi"], cwd = directory).run()
        RunProgram(os.path.join(directory, "main_mpi"), params,  cwd = directory).mpirun(processes, final_output)

    def run_program(self, filename, final_output, params = [], make_args = [],
        program_name = None, processes = 4, repeat = 1, check_fn = None):

        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        if program_name is None:
            program_name = name + "_mpi"
        else:
            program_name = os.path.join(directory, program_name)
        for x in xrange(repeat):
            result = RunProgram(program_name, params, cwd = directory).mpirun(processes, final_output)
            if check_fn is not None:
                check_fn(result)

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        self.build_library(os.path.join(TEST_PROJECTS, "libhelloworld", "libhelloworld.proj"), result)

    def test_overtake(self):
        result = "".join([ "{0}\n".format(x) for x in range(1, 101) ])
        self.build_library(os.path.join(TEST_PROJECTS, "overtake", "overtake.proj"), result, 4, ["--threads=4"])

    def test_library_processes(self):
        result = "".join([ "{0}\n".format(x) for x in range(1, 101) ])
        self.build_library(os.path.join(TEST_PROJECTS, "overtake", "overtake.proj"), result, 1)
        for x in range(2, 5):
            self.run_program(os.path.join(TEST_PROJECTS, "overtake", "overtake.proj"), result, params=["--threads={0}".format(x*x)], processes=x*x)

    def test_modules2(self):
        self.build(os.path.join(TEST_PROJECTS, "modules2", "modules2.proj"))
        self.run_program(os.path.join(TEST_PROJECTS, "modules2", "modules2.proj"), "0\n", params=["--threads=4"])

if __name__ == '__main__':
    unittest.main()