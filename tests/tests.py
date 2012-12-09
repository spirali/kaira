# -*- coding: utf-8 -*-

from testutils import Project
import unittest

class BuildTest(unittest.TestCase):

    def test_helloworld(self):
        Project("helloworld", "helloworlds").quick_test("Hello world 12\n")

    def test_helloworld2(self):
        Project("helloworld2", "helloworlds").quick_test("Hello world 5\n")

    def test_strings(self):
        Project("strings").quick_test("String\nOk\nOk\nOk\nOk\n", processes=5)

    def test_externtypes(self):
        output = "10 20\n107 207\n10 20\n257 77750 A looong string!!!!!\n10 30\n3 20003 String!!!\n"
        Project("externtypes").quick_test(output, processes=2)

    def test_packing(self):
        output = "0\n1\n2\n3\n4\n0\n1\n2\n3\n4\n5\n5\n6\n7\n8\n9\n100\n100\n"
        Project("packing").quick_test(output, processes=3)

    def test_broken1(self):
        Project("broken1", "broken").fail_ptp("*104/inscription: Expression missing\n")

    def test_broken2(self):
        Project("broken2", "broken").fail_ptp("*102/type: Type missing\n")

    def test_parameters(self):
        Project("parameters").quick_test("9 7\n",
                                         processes=10,
                                         params={ "first" : 10, "second" : 7 })

    def test_eguards(self):
        Project("eguards").quick_test("3\n")

    def test_bidirection(self):
        Project("bidirection").quick_test("11\n12\n13\n")

    def test_scheduler(self):
        def check(output):
            d = { "First" : 0, "Second" : 0 }
            for line in output.split("\n"):
                if line:
                    d[line] += 1
            self.assertTrue(d["First"] > 220)
            self.assertTrue(d["Second"] > 220)
        Project("scheduler").quick_test(result_fn=check, processes=10)

    def test_workers(self):
        def check_output(output):
            self.assertEquals(76127, sum([ int(x) for x in output.split("\n")
                                                  if x.strip() != "" ]))
        params = { "LIMIT" : "1000", "SIZE" : "20" }
        p = Project("workers")
        p.build()
        p.run(result_fn=check_output, processes=2, params=params)
        p.run(result_fn=check_output, processes=6, threads=3, params=params, repeat=30)
        p.run(result_fn=check_output, processes=6, threads=1, params=params, repeat=70)

        Project("workers_fixed", "workers").quick_test(threads=5, processes=6, result_fn = check_output, params = params, repeat=110)

    def test_functions(self):
        Project("functions").quick_test("9 9\n")

    def test_tuples(self):
        Project("tuples").quick_test("Ok\n")

    def test_doubles(self):
        Project("doubles").quick_test("Ok\n")

    def test_build(self):
        Project("build").quick_test("1: 10\n2: 20\n")

    def test_getmore(self):
        Project("getmore").quick_test("Ok 7 13\n")

    def test_broken_userfunction(self):
        Project("broken_userfunction", "broken").failed_make("*106/user_function:")

    def test_broken_externtype_function(self):
        Project("broken_externtype_function", "broken").failed_make("*106/pack:")

    def test_multicast(self):
        Project("multicast").quick_test("1800\n", processes=6)

    def test_array(self):
        Project("array").quick_test("Ok\n")

    def test_bool(self):
        Project("bool").quick_test("Ok\n")

    def test_libhelloworld(self):
        result = "40 10 Hello world\n80 10 Hello world\n"\
            "160 10 Hello world\n320 10 Hello world\n640 10 Hello world\n"
        Project("libhelloworld").quick_test_main(result)

    def test_rpc(self):
        p = Project("rpc",rpc=True)
        p.build_main()
        p.start_server()
        try:
            p.run_main("2000 31 31 9000 29700\n", repeat=3)
        finally:
            p.stop_server()

    def test_bigtoken(self):
        Project("bigtoken").quick_test("18000000\n", processes=5, threads=5)

    def test_lib_parameters(self):
        result = "1 1 1 1 \n2 1 1 1 \n4 4 1 1 \n8 8 8 1 \n16 16 16 16 \n"
        Project("parameters", "lib_parameters").quick_test_main(
			result,	processes=5, threads=5, params={"Size" : 4, "EXP" : 4})


class StateSpaceTest(unittest.TestCase):

    def get_analysis(self, report, analysis):
        for e in report.findall("analysis"):
            if e.get("name") == analysis:
                return e
        raise Exception("Analysis '{0}' not found".format(analysis))

    def get_result(self, report, analysis, result):
        for e in self.get_analysis(report, analysis).findall("result"):
            if e.get("name") == result:
                return e
        raise Exception("Result '{0}' not found".format(result))

    def test_statespace1(self):
        report = Project("statespace1").statespace(["quit"])
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "15")
        result = self.get_result(report, "Quit analysis", "Number of quit states")
        self.assertEquals(result.get("value"), "4")
        self.assertEquals(result.get("status"), "ok")
        result = self.get_result(report, "Quit analysis", "Number of dead-ends")
        self.assertEquals(result.get("value"), "0")
        self.assertEquals(result.get("status"), "ok")

    def test_statespace2(self):
        report = Project("statespace2").statespace(["quit"], processes=4)
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "268")
        result = self.get_result(report, "Quit analysis", "Number of quit states")
        self.assertEquals(result.get("value"), "15")
        self.assertEquals(result.get("status"), "ok")
        result = self.get_result(report, "Quit analysis", "Number of dead-ends")
        self.assertEquals(result.get("value"), "1")
        self.assertEquals(result.get("status"), "fail")


if __name__ == '__main__':
    unittest.main()
