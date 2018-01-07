
import os
import sys
import subprocess
import shutil

PATH = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(PATH)
KROOT = os.path.dirname(ROOT)

class RunInstance:

    def __init__(self, name):
        self.name = name
        self.n = 1
        self.r = 1
        self.params = []
        self.args = []

    def mpi(self, n):
        self.n = n
        return self

    def procs(self, r):
        self.r = r
        return self

    def param(self, name, value):
        self.params.append((name, value))
        return self

    def arg(self, value):
        self.args.append(value)
        return self

    def compose(self, repetition):
        params = " ".join([ "-p{0}={1}".format(name, value) for name, value in self.params])
        args = " ".join(self.args)
        output = ".".join([ "{0}_{1}".format(name, value) for name, value in self.params])
        print "mpirun -n {0} ./{1} -r{2} {3} {4} > MPI_{0}.R_{2}.{5}.{6}.out 2>&1".format(self.n, self.name, self.r, params, args, output, repetition)

class TestGenerator:

    def __init__(self, name, repetition):
        self.name = name
        self.repetition = repetition
        self.instances = []

    @staticmethod
    def create_dir(dir):
        if not os.path.isdir(dir):
            os.makedirs(dir)
        return dir

    def add_instance(self):
        self.instances.append(RunInstance(self.name))
        return self.instances[-1]

    def remove_instances(self):
        self.instances= []

    def generate_run_script(self):
        for i in self.instances:
            for r in range(self.repetition):
                i.compose(r)

if __name__ == '__main__':

    if len(sys.argv) < 2:
        print "Provide output directory"
        exit()

    for example in os.listdir(PATH):
        if os.path.isdir(os.path.join(PATH, example)):
            # compile
            path = os.path.join(PATH, example, "statespace")
            subprocess.Popen("make clean", cwd=path, shell = True).communicate()
            subprocess.Popen("make mpi", cwd=path, shell = True).communicate()

            # copy executable
            output = TestGenerator.create_dir(os.path.join(ROOT, sys.argv[1], example))
            shutil.copy2(os.path.join(path, example + "_mpi"), os.path.join(output, example))
            shutil.copy2(os.path.join(PATH, "evaluator.py"), output)
            shutil.copy2(os.path.join(KROOT, "env.set"), output)

            # generate run script
            stdout, stderr = subprocess.Popen(
                "python {0}".format(os.path.join(PATH, example, "run.py")),
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                cwd=path, shell=True).communicate()

            files = {}
            for line in stdout.split("\n"):
                if line.startswith("mpirun -n "):
                    procs = int(line.split()[2])
                    if procs not in files:
                        files[procs] = open(os.path.join(output, "run.{0}.sh".format(procs)), "w")
                        files[procs].write("#!/bin/bash\n\n")
                        files[procs].write("cd {0}\n".format(output))
                        files[procs].write(". env.set\n\n")
                        os.chmod(files[procs].name, 0o777)
                    files[procs].write(line + "\n")

