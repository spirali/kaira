import unittest
import xml.etree.ElementTree as xml
import os
import testutils

class Project(testutils.Project):

    def statespace(self,
                   report=True,
                   analyses=None,
                   por=True,
                   write_statespace=False,
                   **kw):
        self.build("statespace")
        extra_args = [ "-Vsilent" ]
        if not por:
            extra_args.append("-Vdisable-por")
        if write_statespace:
            extra_args.append("-Vwrite-statespace")
        if analyses:
            extra_args += [ "-V" + a for a in analyses ]
        self.run(None, extra_args=extra_args, **kw)

        if report:
            kreport = os.path.join(self.get_directory(), self.name + ".kreport")
            return xml.parse(kreport).getroot()

    def check_por(self, **kw):
        report_full = self.statespace(por=False, write_statespace=True, **kw)
        report_reduced = self.statespace(por=True, write_statespace=True, **kw)
        full = StateSpace(report_full)
        reduced = StateSpace(report_reduced)
        check_por(full, reduced)


def load_from_kreport(filename):
    root = xml.parse(filename).getroot()
    return StateSpace(root)


class StateSpace:

    def __init__(self, root):
        nodes = {}
        self.initial_node = None

        element = root.find("statespace")

        for e in element.findall("state"):
            name = e.get("hash")
            node = nodes.get(name)
            if node is None:
                node = Node(name)
                nodes[name] = node

            if e.get("initial", False):
                assert self.initial_node is None
                self.initial_node = node

            for c in e.findall("child"):
                name = c.get("hash")
                n = nodes.get(name)
                if n is None:
                    n = Node(name)
                    nodes[name] = n
                node.childs.append(n)
                aname = c.get("action")
                if aname == "fire":
                    action = ActionFire(int(c.get("process")),
                                        int(c.get("transition")))
                elif aname == "receive":
                    action = ActionFire(int(c.get("process")),
                                        int(c.get("source")))
                else:
                    raise Exception("Invalid action")
                node.actions.append(action)

            if e.get("quit", False):
               assert len(node.actions) == 0
               node.quit = True

        assert self.initial_node is not None
        self.nodes = nodes

    def get(self, name):
        return self.nodes[name]

    def all_nodes(self):
        return self.nodes.values()


class Node:

    quit = False

    def __init__(self, name):
        self.name = name
        self.childs = []
        self.actions = []

    def execute(self, action):
        if action not in self.actions:
            return None
        return self.childs[self.actions.index(action)]

    def execute_list(self, actions):
        node = self
        for action in actions:
            if node is None:
                return None
            node = node.execute(action)
        return node

    def __str__(self):
        return "{{{0}}}".format(self.name)

    def __repr__(self):
        return "{{{0}}}".format(self.name)



class ActionFire:

    action = "fire"

    def __init__(self, process, transition):
        self.process = process
        self.transition = transition

    def __eq__(self, other):
        return (isinstance(self, ActionFire) and
                self.process == other.process and
                self.transition == other.transition)

    def __hash__(self):
        return self.process * 10101 + self.transition

    def __repr__(self):
        return "F {0.process} {0.transition}".format(self)


class ActionReceive:

    action = "receive"

    def __init__(self, process, source):
        self.process = process
        self.source = source

    def __eq__(self, other):
        return (isinstance(self, ActionReceive) and
                self.process == other.process and
                self.source == other.source)

    def __hash__(self):
        return self.process * 10103 + self.source

    def __repr__(self):
        return "R {0.process} {0.source}".format(self)


def get_nodes(root, forbidden_actions):
    result = set()
    nexts = []
    nexts.append(root)
    result.add(root)
    while nexts:
        node = nexts[-1]
        nexts.pop()
        for action in node.actions:
            if action in forbidden_actions:
                continue
            new = node.execute(action)
            if new not in result:
                result.add(new)
                nexts.append(new)
    return result

def check_diamond(node, action1, action2):
    tmp = node.execute(action1)
    if tmp is None:
        return False
    if tmp.quit:
        return True
    node1 = tmp.execute(action2)
    tmp = node.execute(action2)
    if tmp is None:
        return False
    node2 = tmp.execute(action1)
    return node1 and node2 and node1.name == node2.name

class PorException(Exception):
    pass

def check_subgraph(full, reduced):

    for rnode in reduced.all_nodes():
        ample = set(rnode.actions)
        fnode = full.get(rnode.name)
        enabled = set(fnode.actions)
        assert ample.issubset(enabled)

        for a in ample:
            n1 = rnode.execute(a)
            n2 = fnode.execute(a)
            if n1.name != n2.name:
                raise PorException("Reduced statespace is not subgraph of full statespace")

def check_por(full, reduced):

    def check_node(fnode, rnode, ample):
        for node in get_nodes(fnode, ample):
            check_subtree_node(node, ample, fnode)

    def check_subtree_node(node, ample, origin):
        for a1 in node.actions:
           if a1 in ample:
               continue
           for a2 in node.actions:
               if a2 not in ample:
                   continue
               if not check_diamond(node, a1, a2):
                   raise PorException("Diamond check failed in node '{0.name}' "
                                      "action1='{1}' action2='{2}' caused by reduction in "
                                      "node '{3.name}'".format(node, a1, a2, origin))
    check_subgraph(full, reduced)
    for rnode in reduced.all_nodes():
        ample = set(rnode.actions)
        fnode = full.get(rnode.name)
        enabled = set(fnode.actions)
        if not ample and enabled:
            raise PorException("Rule C0 of POR violated in node '{0}'".format(rnode.name))
        if ample == enabled:
            continue # Fully computed state
        check_node(fnode, rnode, ample)


class VerifUtilsTest(unittest.TestCase):

    def test_load(self):
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report1.kreport")
        statespace = load_from_kreport(filename)
        self.assertEqual(len(statespace.nodes), 39)
        name = "21825B02B7C550CE763274A86BCD28D0"
        node = statespace.get(name)
        self.assertEquals(len(node.actions), 2)
        self.assertEquals(len(node.childs), 2)

    def test_por_tester1(self):
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report2.kreport")
        full = load_from_kreport(filename)
        reduced = load_from_kreport(filename)
        check_por(full, reduced)

    def test_por_tester2(self):
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report2.kreport")
        full = load_from_kreport(filename)
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report4.kreport")
        reduced = load_from_kreport(filename)
        check_por(full, reduced)

    def test_por_tester3(self):
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report2.kreport")
        full = load_from_kreport(filename)
        filename = os.path.join(testutils.TEST_PROJECTS, "kreports", "report3.kreport")
        reduced = load_from_kreport(filename)
        with self.assertRaises(PorException):
            check_por(full, reduced)


if __name__ == "__main__":
    unittest.main()
