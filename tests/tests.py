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
        return RunProgram(PTP_BIN, [ name + ".xml", "--build", name + ".cpp" ])

    def build(self, filename, final_output = None, make_args = [], **kw):
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
        args = [ CMDUTILS, "--export", filename ]
        RunProgram("python", args).run()
        self.run_ptp(name).run()
        RunProgram("make", make_args, cwd = directory).run()
        if final_output is not None:
            self.run_program(filename, final_output, **kw)

    def build_library(self, filename, final_output, make_args = []):
        self.build(filename)
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        RunProgram("make", [ "-f", "makefile.main" ], cwd = directory).run()
        RunProgram(os.path.join(directory, "main"), cwd = directory).run(final_output)

    def run_program(self, filename, final_output, params = [], make_args = [],
        program_name = None, processes=None, repeat=1, check_fn=None):

        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        if program_name is None:
            program_name = name
        else:
            program_name = os.path.join(directory, program_name)
        if processes is not None:
            params = [ "-r {0}".format(processes) ] + params
        for x in xrange(repeat):
            result = RunProgram(program_name, params, cwd = directory).run(final_output)
            if check_fn is not None:
                check_fn(result)

    def failed_ptp(self, filename, final_output):
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
        RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
        self.run_ptp(name).fail(final_output)

    def failed_make(self, filename, stderr, make_args = []):
        name, ext = os.path.splitext(filename)
        directory = os.path.dirname(name)
        RunProgram("/bin/sh", [os.path.join(TEST_PROJECTS, "fullclean.sh")], cwd = directory).run()
        RunProgram("python", [ CMDUTILS, "--export", filename ]).run()
        self.run_ptp(name).run()
        RunProgram("make", make_args, cwd = directory).fail(expected_stderr_prefix = stderr)

    def test_helloworld(self):
        self.build(os.path.join(TEST_PROJECTS, "helloworlds", "helloworld.proj"), "Hello world 12\n")

    def test_helloworld2(self):
        self.build(os.path.join(TEST_PROJECTS, "helloworlds", "helloworld2.proj"), "Hello world 5\n")

    def test_strings(self):
        self.build(os.path.join(TEST_PROJECTS, "strings", "strings.proj"), "String\nOk\nOk\nOk\nOk\n", processes=5)

    def test_externtypes(self):
        output = "10 20\n107 207\n10 20\n257 77750 A looong string!!!!!\n10 30\n3 20003 String!!!\n"
        self.build(os.path.join(TEST_PROJECTS, "externtypes", "externtypes.proj"), output, processes=2)

    def test_packing(self):
        output = "0\n1\n2\n3\n4\n0\n1\n2\n3\n4\n5\n5\n6\n7\n8\n9\n100\n100\n"
        self.build(os.path.join(TEST_PROJECTS, "packing", "packing.proj"), output, processes=3)

    def test_broken1(self):
        self.failed_ptp(os.path.join(TEST_PROJECTS, "broken", "broken1.proj"), "*104/inscription: Expression missing\n")

    def test_broken2(self):
        self.failed_ptp(os.path.join(TEST_PROJECTS, "broken", "broken2.proj"), "*102/type: Type missing\n")

    def test_broken_module(self):
        self.failed_ptp(os.path.join(TEST_PROJECTS, "broken", "broken_module.proj"),
            "*103: Conflict of types with the assigned module in variable 'x', type in module is String\n")

    def test_parameters(self):
        self.build(os.path.join(TEST_PROJECTS, "parameters", "parameters.proj"), "9 7\n",
            params=["-pfirst=10", "-psecond=7"], processes = 10)

    def test_eguards(self):
        self.build(os.path.join(TEST_PROJECTS, "eguards", "eguards.proj"), "3\n")

    def test_bidirection(self):
        self.build(os.path.join(TEST_PROJECTS, "bidirection", "bidirection.proj"), "11\n12\n13\n")

    def test_scheduler(self):
        def check(output):
            d = { "First" : 0, "Second" : 0 }
            for line in output.split("\n"):
                if line:
                    d[line] += 1
            self.assertTrue(d["First"] > 220)
            self.assertTrue(d["Second"] > 220)
        self.build(os.path.join(TEST_PROJECTS, "scheduler", "scheduler.proj"),
            None, processes=10, check_fn = check)

    def test_workers(self):
        def check_output(output):
            self.assertEquals(76127, sum([ int(x) for x in output.split("\n") if x.strip() != "" ]))
        params = [ "-pLIMIT=1000", "-pSIZE=20" ]
        self.build(os.path.join(TEST_PROJECTS, "workers", "workers.proj"),
            None, params=params, processes=2, check_fn = check_output)
        self.run_program(os.path.join(TEST_PROJECTS, "workers", "workers.proj"), None,
             params=params + [ "--threads=3" ], processes=6, check_fn = check_output, repeat=30)
        self.run_program(os.path.join(TEST_PROJECTS, "workers", "workers.proj"), None,
             params=params + [ "--threads=1" ], processes=6, check_fn = check_output, repeat=70)
        self.build(os.path.join(TEST_PROJECTS, "workers", "workers_fixed.proj"), None,
             params=params + [ "--threads=5" ], processes=1, check_fn = check_output, repeat=110)

    def test_functions(self):
        self.build(os.path.join(TEST_PROJECTS, "functions", "functions.proj"), "9 9\n")

    def test_tuples(self):
        self.build(os.path.join(TEST_PROJECTS, "tuples", "tuples.proj"), "Ok\n")

    def test_doubles(self):
        self.build(os.path.join(TEST_PROJECTS, "doubles", "doubles.proj"), "Ok\n")

    """ TEMPORARILY DISABLED
    def test_log(self):
        self.build(os.path.join(TEST_PROJECTS, "log", "log.proj"), "", make_args = [ "debug" ], program_name="log_debug")
        directory = os.path.join(TEST_PROJECTS, "log")
        RunProgram(os.path.join(KAIRA_TOOLS, "logmerge.py"), [ "log1" ], cwd = directory).run()
        RunProgram(os.path.join(KAIRA_TOOLS, "logmerge.py"), [ "log2" ], cwd = directory).run()
        self.assertTrue(os.path.isfile(os.path.join(TEST_PROJECTS, "log", "log1.klog")))
        self.assertTrue(os.path.isfile(os.path.join(TEST_PROJECTS, "log", "log2.klog")))
    """

    def test_build(self):
        self.build(os.path.join(TEST_PROJECTS, "build", "build.proj"), "1: 10\n2: 20\n")

    def test_getmore(self):
        self.build(os.path.join(TEST_PROJECTS, "getmore", "getmore.proj"), "Ok 7 13\n")

    def test_broken_userfunction(self):
        self.failed_make(os.path.join(TEST_PROJECTS, "broken", "broken_userfunction.proj"), "*106/user_function:")

    def test_broken_externtype_function(self):
        self.failed_make(os.path.join(TEST_PROJECTS, "broken", "broken_externtype_function.proj"), "*MyType/getsize"),

    def test_multicast(self):
        self.build(os.path.join(TEST_PROJECTS, "multicast", "multicast.proj"), "1800\n", processes=4)

    def test_array(self):
        self.build(os.path.join(TEST_PROJECTS, "array", "array.proj"), "Ok\n")

    def test_bool(self):
        self.build(os.path.join(TEST_PROJECTS, "bool", "bool.proj"), "Ok\n")

    def test_modules1(self):
        self.build(os.path.join(TEST_PROJECTS, "modules1", "modules1.proj"), "148\n")
        self.run_program(os.path.join(TEST_PROJECTS, "modules1", "modules1.proj"), "148\n",
            params=["--threads=2"], repeat=100)
        self.run_program(os.path.join(TEST_PROJECTS, "modules1", "modules1.proj"), "148\n",
            params=["--threads=5"], processes=3, repeat=100)

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        self.build_library(os.path.join(TEST_PROJECTS, "libhelloworld", "libhelloworld.proj"), result)

if __name__ == '__main__':
    unittest.main()
