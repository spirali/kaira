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

    def test_scatter1(self):
        Project("scatter1", mpi=True).quick_test("1941\n", processes=5)

    def test_scattergather(self):
        result = "0: 110.3 220.3 330.3 340.3 350.3\n" \
                 "1: MyC: { 1 1 1 1 1 } MyC: { 2 2 2 2 2 } MyC: { 3 3 3 3 3 } MyC: { 4 4 4 4 4 } MyC: { 5 5 5 5 5 }\n" \
                 "2: aaaaaaaaaaaaaaaaaaaaaaaa bb ccccccccccccccccccc  mmmmmmmmm\n"
        Project("scattergather", mpi=True).quick_test(result, processes=5, params={"SIZE": 5})

    def test_allgather(self):
        Project("allgather").quick_test("Ok\n", processes=5)

if __name__ == '__main__':
    unittest.main()
