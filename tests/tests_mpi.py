# -*- coding: utf-8 -*-

from testutils import Project
import unittest

class MpiBuildTest(unittest.TestCase):

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        Project("libhelloworld", mpi=True, lib=True).quick_test_main(result)

    def test_rpc(self):
        p = Project("rpc", mpi=True,rpc=True, lib=True)
        p.build_main()
        p.start_server()
        try:
            p.run_main("2000 31 31 9000 29700\n", repeat=3)
        finally:
            p.stop_server()

if __name__ == '__main__':
    unittest.main()
