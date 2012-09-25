#
#    Copyright (C) 2012 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
#


import subprocess
import os.path
import os
import time
import sys

KAIRA_BENCHMARKS = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_BENCHMARKS)
KAIRA_BENCHMARKS_PROJECTS = os.path.join(KAIRA_BENCHMARKS, "projects")

KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
KAIRA_TOOLS = os.path.join(KAIRA_ROOT,"tools")

PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CAILIE_DIR = os.path.join(KAIRA_ROOT, "lib")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

benchmarks = {}
projects = set()
tests = set()


class BenchmarkBase(object):

    def __init__(self, benchmark_name, executable, processes, threads):
        self.name = benchmark_name
        self.executable = executable
        self.processes = processes
        self.threads = threads
        self.parameters = {}

    def run(self):
        args = [
            "-r {0}".format(self.processes),
            "--threads={0}".format(self.threads)
        ]
        args += [ "-p{0}={1}".format(k, v) for k, v in self.parameters.items() ]
        return run_program(self.executable, args)

    def params(self, **kw):
        self.parameters = kw


class NetBenchmark(BenchmarkBase):

    def __init__(self, benchmark_name, project_name, *args, **kw):
        directory = os.path.join(KAIRA_BENCHMARKS_PROJECTS, project_name)
        executable = os.path.join(directory, "release", project_name)
        BenchmarkBase.__init__(self, benchmark_name, executable, *args, **kw)


class TestBenchmark(BenchmarkBase):

    def __init__(self, benchmark_name, project_name, test_name, *args, **kw):
        directory = os.path.join(KAIRA_BENCHMARKS_PROJECTS,
                                 project_name,
                                 "tests",
                                 test_name)
        executable = os.path.join(directory, test_name)
        BenchmarkBase.__init__(self, benchmark_name, executable, *args, **kw)


def add(project_name, name=None, processes=1, threads=1):
    if name is None:
        name = project_name
    if name in benchmarks:
        raise Exception("Benchmark with name '{0}' already exists".format(name))
    benchmark = NetBenchmark(name, project_name, processes, threads)
    benchmarks[name] = benchmark
    projects.add(project_name)
    return benchmark

def add_test(project_name, test_name, name=None, processes=1, threads=1):
    if name is None:
        name = project_name
    if name in benchmarks:
        raise Exception("Benchmark with name '{0}' already exists".format(name))
    benchmark = TestBenchmark(name, project_name, test_name, processes, threads)
    benchmarks[name] = benchmark
    projects.add(project_name)
    tests.add((project_name, test_name))
    return benchmark

def run_program(filename, args = [], cwd = None):
    p = subprocess.Popen([filename] + args, cwd = cwd)
    tm = time.time()
    p.wait()
    tm = time.time() - tm
    if p.returncode != 0:
        raise Exception("Run {0} {1} failed".format(filename, args))
    return tm

def build_project(project):
    directory = os.path.join(KAIRA_BENCHMARKS_PROJECTS, project)
    filename = os.path.join(directory, project + ".proj")
    output = os.path.join(directory, "release")
    args = [ CMDUTILS, "--export", filename, "--output", output ]
    run_program("python", args)
    xmlfile = os.path.join(output, project + ".xml")
    run_program(PTP_BIN, [ xmlfile, "--build", output ])
    run_program("make", cwd = output)

def build_test(project, test_name):
    directory = os.path.join(KAIRA_BENCHMARKS_PROJECTS, project, "tests", test_name)
    executable = os.path.join(directory, test_name)
    if os.path.isfile(executable):
        os.remove(executable)
    run_program("make", cwd = directory)

def build_projects(projects):
    for p in projects:
        print "Building '{0}' ...".format(p)
        build_project(p)

def build_tests(tests):
    for p,t in tests:
        print "Building '{0}/{1}' ...".format(p, t)
        build_test(p, t)

def parse_args():
    if len(sys.argv) == 1 or (len(sys.argv) == 2 and sys.argv[1] == "-q"):
        print "python {0} [-q] run_name".format(sys.argv[0])
        sys.exit(0)

    if sys.argv[1] == "-q":
        build = False
        run_name = sys.argv[2]
    else:
        build = True
        run_name = sys.argv[1]
    return run_name, build

def run(runs_count = 10):
    run_name, build = parse_args()

    if build:
        build_projects(projects)
        build_tests(tests)
        print 60 * "-"
    else:
        print "Build skipped"

    lst = benchmarks.values()
    lst.sort(key=lambda b: b.name)
    results = []
    for index, benchmark in enumerate(lst):
        print "[{0}/{1}] Running {2} ".format(index + 1,
                                              len(benchmarks),
                                              benchmark.name),
        for i in xrange(runs_count):
            sys.stdout.write(".")
            sys.stdout.flush()
            tm = benchmark.run()
            results.append("{0}: {1}".format(benchmark.name, tm))
        print ""
    with open(run_name, "w") as f:
        f.write("\n".join(results))
