# -*- coding: utf-8 -*-

from verifutils import Project
import unittest


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
        report = Project("statespace1").statespace(analyses=["deadlock"], por=False)
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "4")
        result = self.get_result(report, "Quit analysis", "Number of deadlock states")
        self.assertEquals(result.get("value"), "0")
        self.assertEquals(result.get("status"), "ok")

    def test_statespace2(self):
        report = Project("statespace2").statespace(analyses=["deadlock"],
                                                   por=False,
                                                   processes=4)
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "50")
        result = self.get_result(report, "Quit analysis", "Number of deadlock states")
        self.assertEquals(result.get("value"), "1")
        self.assertEquals(result.get("status"), "fail")

    def test_por1(self):
        Project("por1").check_por(processes=2)

    def test_por2(self):
        Project("por2").check_por(processes=2)

    def test_por3(self):
        Project("por3").check_por(processes=2)

    def test_heatflow(self):
        Project("heatflow").check_por(processes=3)

    def test_workers(self):
        params = { "LIMIT" : "20", "SIZE" : "5" }
        Project("workers").check_por(processes=3, params=params)

    def test_por_bulk(self):
        Project("por_bulk").check_por(processes=3)

    def test_por_cycles(self):
        Project("por_cycles").check_por(processes=2)


if __name__ == '__main__':
    unittest.main()
