
import os
import sys

PATH = os.path.dirname(os.path.abspath(__file__))
EXAMPLE = os.path.basename(os.path.dirname(__file__))
ROOT = os.path.dirname(PATH)
sys.path.append(ROOT)

from generator import *

if __name__ == '__main__':

    generator = TestGenerator(EXAMPLE, 2)

    for mpi in [1, 2, 4, 8, 16, 32, 64, 128, 256]:
        for procs in [2, 4, 6, 8]:
            (generator.add_instance()
             .mpi(mpi)
             .procs(procs)
             .param("SIZE", 10)
             .param("LIMIT", 200))

    generator.generate_run_script()
