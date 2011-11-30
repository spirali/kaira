
import unittest
from base.project import load_project
import xml.etree.ElementTree as xml
from base.expressions import ExprCall, ExprInt, ExprVar, ExprArray
from base.neltypes import t_int, t_bool, t_string

class TestProject(unittest.TestCase):

    def test_load_project(self):
        root = xml.fromstring(simple_project)
        p = load_project(root)
        self.assertEqual(p.nets[0].id, 101)
        self.assertEqual(p.nets[0].get_place(102).type, t_int)
        self.assertEqual(p.nets[0].get_transition(103).guard, ExprCall("<", [ ExprVar("x"), ExprVar("y") ]))
        self.assertEqual(p.nets[0].get_transition(111).guard, None)

        edges_in = p.nets[0].get_transition(103).edges_in
        edges_out = p.nets[0].get_transition(103).edges_out
        self.assertEqual(edges_in[0].expr, ExprVar("x"))
        self.assertEqual(edges_in[1].expr, ExprVar("y"))

        place = p.nets[0].get_place(105)
        self.assertEqual(place.init_expression, ExprArray((ExprInt(1), ExprInt(2))))
        #self.assertEqual(edges_out[0].expr, ExprCall("+", [ ExprCall("+", [ ExprVar("x"), ExprInt(1) ]), ExprParam("PARAM") ]))

    def test_transition_context(self):
        root = xml.fromstring(simple_project)
        p = load_project(root)
        tr = p.nets[0].get_transition(103)
        self.assertEqual(tr.get_context(), {"x" : t_int, "y" : t_int})

    def test_types(self):
        root = xml.fromstring(simple_project)
        p = load_project(root)
        self.assertEqual(p.get_all_types(),  set([ t_bool, t_string, t_int]))

    def test_edge(self):
        root = xml.fromstring(simple_project)
        p = load_project(root)
        tr = p.nets[0].get_transition(103)
        e = tr.edges_in[0]
        self.assertEqual(e.get_equations(), [ (ExprVar("x"), t_int)] )


simple_project = """<project extenv='C++'>
  <configuration>
    <parameter default="0" description="My param" name="PARAM" type="Int"/>
    <build-option name="CC">g++</build-option>
    <build-option name="LIBS"/>
    <build-option name="CFLAGS">-    def get_exprs(self):
        return [ e for e, _ in self.get_exprs_and_types() ]
O2</build-option>
    <force-packers value="False"/>
  </configuration>
  <description>EMPTY</description>
  <net id="101" name="Main">
    <place id="102" init-expr="" name="name" type="Int"/>
    <place id="105" init-expr="[1,2]" name="name" type="Int"/>
    <place id="107" init-expr="" name="name" type="Int"/>
    <place id="109" init-expr="" name="name" type="String"/>
    <place id="112" init-expr="" name="name" type="String"/>
    <transition guard="x &lt; y" id="103" name="">
      <edge-in expr="x" id="104" place-id="102"/>
      <edge-in expr="y" id="108" place-id="107"/>
      <edge-out expr="x + 1 + 1" id="106" place-id="105"/>
    </transition>
    <transition guard="" id="111" name="">
      <edge-in expr="y" id="113" place-id="109"/>
      <edge-out expr="y" id="114" place-id="112"/>
    </transition>
  </net>
</project>
"""


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testExprs']
    unittest.main()
