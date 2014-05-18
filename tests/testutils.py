
import subprocess
import os
import time

KAIRA_TESTS = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_TESTS)
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
KAIRA_TOOLS = os.path.join(KAIRA_ROOT,"tools")

PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CAILIE_DIR = os.path.join(KAIRA_ROOT, "lib")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

TEST_PROJECTS = os.path.join(KAIRA_TESTS, "projects")

class RunProgram:

    def __init__(self, filename, parameters=None, cwd=None, env=None):
        self.filename = filename
        if parameters is None:
            self.parameters = []
        else:
            self.parameters = parameters
        self.cwd = cwd
        self.env = env

    def result(self, pr, expected_output):
        output,errs = pr.communicate()
        if errs != "":
            self.error("Stderr is not empty but: %s" % errs)
        if pr.returncode != 0:
            self.error("Return code is not zero")
        self.check_output(expected_output, output)
        return output

    def run(self, expected_output = None):
        pr = subprocess.Popen([self.filename] + self.parameters, stdout=subprocess.PIPE,
            stderr=subprocess.PIPE, cwd = self.cwd, env = self.env)
        return self.result(pr, expected_output)

    def fail(self, expected_stdout=None, expected_stderr=None, prefix=False):
        pr = subprocess.Popen([self.filename] + self.parameters,
                              stdout=subprocess.PIPE,
                              stderr=subprocess.PIPE,
                              cwd=self.cwd)
        output,errs = pr.communicate()
        if pr.returncode == 0:
            self.error("Expected fail, but return code is zero")
        if prefix:
            fn = lambda e, o: o.startswith(e)
        else:
            fn = None
        self.check_output(expected_stdout, output, fn)
        self.check_output(expected_stderr, errs, fn)

    def check_output(self, expected, output, f=None):
        if f is None:
            f = lambda a, b: a == b
        if expected is not None and not f(expected, output):
            self.error("Excepted >>{0}<<, got >>{1}<<".format(expected, output))

    def error(self, text):
        raise Exception("Program '{0}/{1}': {2}".format(self.filename, self.parameters, text))


class Project:

    server = None

    def __init__(self, name, directory_name=None, mpi=False, rpc=False, trace=False, lib=False):
        self.name = name
        if directory_name is None:
            self.directory_name = name
        else:
            self.directory_name = directory_name

        self.mpi = mpi
        self.rpc = rpc
        self.trace = trace
        self.lib = lib

        self.clean()

    def get_filename(self):
        return os.path.join(self.get_directory(), self.name + ".proj")

    def get_xml_filename(self):
        return os.path.join(self.get_directory(), self.name + ".xml")

    def get_directory(self):
        return os.path.join(TEST_PROJECTS, self.directory_name)

    def get_executable(self):
        if self.mpi and not self.rpc:
            return os.path.join(self.get_directory(), self.name + "_mpi")
        else:
            return os.path.join(self.get_directory(), self.name)

    def get_main(self):
        if self.mpi and not self.rpc:
            return os.path.join(self.get_directory(), "main_mpi")
        else:
            return os.path.join(self.get_directory(), "main")

    def clean(self):
        RunProgram("/bin/sh",
                   [os.path.join(TEST_PROJECTS, "fullclean.sh")],
                   cwd=self.get_directory()).run()

    def export(self):
        args = [ CMDUTILS, "--export", self.get_filename() ]
        if self.trace:
            args.append("--trace")
        if self.lib:
            args.append("--lib")
        RunProgram("python", args).run()

    def run_ptp(self, operation=None):
        if operation is None:
            operation = "build"
        RunProgram(PTP_BIN, [ operation, self.get_xml_filename(), "--output", self.get_directory() ]).run()

    def fail_ptp(self, output, prefix=False):
        self.export()
        program = RunProgram(PTP_BIN, ["build",
                             self.get_xml_filename(),
                             "--output",
                             self.get_directory()])
        program.fail(output, prefix=prefix)

    def fail_make(self, output, args = []):
        self.export()
        self.run_ptp()
        RunProgram("make", args, cwd = self.get_directory()).fail(expected_stderr_prefix=output)

    def failed_run_main(self, result, **kw):
        self.run(result, executable = self.get_main(), fail = True, **kw)

    def make_project(self):
        if self.mpi:
            self._make(["mpi"])
        else:
            self._make()

    def _make(self, args = []):
        RunProgram("make", args, cwd = self.get_directory()).run()

    def build(self, operation=None):
        self.export()
        self.run_ptp(operation)
        self.make_project()

    def run(self,
            result=None,
            executable=None,
            processes=1,
            params={},
            extra_args=None,
            **kw):

        if executable is None:
            executable = self.get_executable()

        if self.mpi and not self.rpc:
            run_args = [ "-np", str(processes),
                         executable ]
        else:
            run_args = [ "-r{0}".format(processes) ]

        for name in params:
            run_args.append("-p{0}={1}".format(name, params[name]))

        if extra_args:
            run_args += extra_args

        if self.mpi and not self.rpc:
            self._run("mpirun", run_args, result, **kw)
        else:
            self._run(executable, run_args, result, **kw)

    def _run(self, executable, run_args, result=None, result_fn=None, repeat=1, env=None, fail=False):
        rp = RunProgram(executable, run_args, env=env, cwd=self.get_directory())
        for x in xrange(repeat):
            if fail:
                rp.fail(expected_stderr=result)
            else:
                r = rp.run(result)
                if result_fn is not None:
                    result_fn(r)

    def quick_test(self, result=None, **kw):
        self.build()
        self.run(result, **kw)

    def check_tracelog(self, output):
        filename = os.path.join(self.get_directory(), "trace.kth")
        args = [ CMDUTILS, "--tracelog", filename ]
        RunProgram("python", args).run(output)

    def build_main(self):
        self.build("lib")
        if self.mpi and not self.rpc:
            self._make(["-f", "makefile.main", "main_mpi"])
        else:
            self._make(["-f", "makefile.main"])

    def run_main(self, result, **kw):
        env = { "CACLIENT_HOST" : "localhost:14980" }
        self.run(result, executable=self.get_main(), env=env, **kw)

    def quick_test_main(self, *args, **kw):
        self.build_main()
        self.run_main(*args, **kw)

    def get_server_directory(self):
        return os.path.join(self.get_directory(), "server")

    def get_server_executable(self):
        filename = os.path.join(self.get_server_directory(), self.name + "_server")
        if self.mpi:
            filename += "_mpi"
        return filename

    def start_server(self, processes=1, params=None):
        if params is None:
            params = {}

        env = { "CASERVER_PORT" : "14980" }
        executable = self.get_server_executable()

        if self.mpi:
            run_args = [ "-np", str(processes), executable ]
        else:
            run_args = [ "-r{0}".format(processes) ]

        for name in params:
            run_args.append("-p{0}={1}".format(name, params[name]))

        if self.mpi:
            real_program = "mpirun"
        else:
            real_program = executable

        self.server = subprocess.Popen([real_program] + run_args,
                                       cwd=self.get_server_directory(),
                                       env=env)
        time.sleep(1.0) # Let's give some time to server to open socket

    def stop_server(self):
        self.server.kill()
