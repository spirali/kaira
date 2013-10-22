# -*- coding: utf-8 -*-

from testutils import Project
import unittest

class MpiThreadsBuildTest(unittest.TestCase):

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        Project("libhelloworld", mpi=True, lib=True).quick_test_main(result)

if __name__ == '__main__':
    unittest.main()
