# -*- coding: utf-8 -*-

from testutils import Project
import unittest

def parse_statespace(statespace):
    init = None
    result = {}
    map = {}
    statespace = statespace.splitlines()
    for line in statespace:
        if "label" in line:
            if "->" not in line:
                parts = line.replace('"', ' ').replace('|', ' ').split()
                map[parts[0]] = parts[2]
                result[parts[2]] = {}
            if "style" in line and init is None:
                init = parts[2]
    for line in statespace:
        if "label" in line:
            if "->" in line:
                parts = line.replace('"', ' ').split()
                result[map[parts[0]]][parts[4] + parts[5] + ' ' + parts[6]] = map[parts[2]]

    return init, result

def compare_statespace(full, reduced, init):
    def apply_path(s, path):
        for move in path:
            if move not in full[s]:
                return s
            s = full[s][move]
        return s

    def check_independency(s, played):
        for move in full[s]:
            if move not in played:
                for p in played:
                    v1 = apply_path(s, [move, p])
                    v2 = apply_path(s, [p, move])
                    if v1 != v2 and not (v1.startswith("!") and v2.startswith("!")):
                        return s
        return None

    def check_state(s):
        states = [ s ]
        added = set([ s ])
        while len(states):
            if check_independency(states[0], reduced[s]) is not None:
                return states[0]
            for move in full[states[0]]:
                if move not in reduced[s] and full[states[0]][move] not in added:
                    states.append(full[states[0]][move])
                    added.add(full[states[0]][move])
            states.pop(0)
        return None

    states = [ init ]
    added = set([ init ])
    while len(states):
        if len(full[states[0]]) > len(reduced[states[0]]):
            witness = check_state(states[0])
            if witness is not None:
                if witness == states[0]:
                    return "State " + states[0] + " can not be reduced."
                else:
                    return "State " + states[0] + " can not be reduced (predecessor of " + witness + ")."
        for s in reduced[states[0]].values():
            if s not in added:
                states.append(s)
                added.add(states[0])
        states.pop(0)
    return None

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
        report = Project("statespace1").statespace(["deadlock", "disable_partial_order"])
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "4")
        result = self.get_result(report, "Quit analysis", "Number of deadlock states")
        self.assertEquals(result.get("value"), "0")
        self.assertEquals(result.get("status"), "ok")

    def test_statespace2(self):
        report = Project("statespace2").statespace(["deadlock", "disable_partial_order"], processes=4)
        result = self.get_result(report, "Overall statistics", "Number of states")
        self.assertEquals(result.get("value"), "50")
        result = self.get_result(report, "Quit analysis", "Number of deadlock states")
        self.assertEquals(result.get("value"), "1")
        self.assertEquals(result.get("status"), "fail")

    def test_partial_order1(self):
        init, full = parse_statespace(Project("partial_order1").partial_order(["disable_partial_order"], processes=2))
        init, reduced = parse_statespace(Project("partial_order1").partial_order([], processes=2))
        result = compare_statespace(full, reduced, init)
        self.assertEqual(result, None, result)

    def test_partial_order2(self):
        init, full = parse_statespace(Project("partial_order2").partial_order(["disable_partial_order"], processes=2))
        init, reduced = parse_statespace(Project("partial_order2").partial_order([], processes=2))
        result = compare_statespace(full, reduced, init)
        self.assertEqual(result, None, result)

    def test_heatflow(self):
        init, full = parse_statespace(Project("heatflow").partial_order(["disable_partial_order"], processes=3))
        init, reduced = parse_statespace(Project("heatflow").partial_order([], processes=3))
        result = compare_statespace(full, reduced, init)
        self.assertEqual(result, None, result)

    def test_por_bulk(self):
        init, full = parse_statespace(Project("por_bulk").partial_order(["disable_partial_order"], processes=3))
        init, reduced = parse_statespace(Project("por_bulk").partial_order([], processes=3))
        result = compare_statespace(full, reduced, init)
        self.assertEqual(result, None, result)

    def test_por_cycles(self):
        init, full = parse_statespace(Project("por_cycles").partial_order(["disable_partial_order"], processes=2))
        init, reduced = parse_statespace(Project("por_cycles").partial_order([], processes=2))
        result = compare_statespace(full, reduced, init)
        self.assertEqual(result, None, result)
if __name__ == '__main__':
    unittest.main()