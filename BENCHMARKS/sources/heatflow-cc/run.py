
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
            for x in [1, 2, 4]:
                for y in [1, 2, 4]:
                    (generator.add_instance()
                     .mpi(mpi)
                     .procs(procs)
                     .param("SIZE_X", 64 * x)
                     .param("SIZE_Y", 64 * y)
                     .param("LIMIT", 10)
                     .param("TEMP", 200)
                     .arg("-Vdisable-por"))

    generator.generate_run_script()
