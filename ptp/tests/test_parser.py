'''
Created on Oct 4, 2011

@author: spirali
'''
import unittest

import base.expressions as e
import base.neltypes as t
from base.parser import parse_expression, parse_type
from base.parser import parse_output_inscription

class TestParser(unittest.TestCase):

    def test_literals(self):
        self.assertEqual(parse_expression("xyz"), e.ExprVar("xyz"))
        self.assertEqual(parse_expression("10"), e.ExprInt(10))
        self.assertEqual(parse_expression('"  Hi  "'), e.ExprString('  Hi  '))

    def test_parameter(self):
        self.assertEqual(parse_expression("#PARAM"), e.ExprParam("PARAM"))

    def test_call(self):
        i501 = e.ExprInt(501)
        i502 = e.ExprInt(502)
        self.assertEqual(parse_expression("fun ()"), e.ExprCall("fun", []))
        self.assertEqual(parse_expression("f(501)"), e.ExprCall("f", [i501]))
        self.assertEqual(parse_expression("f  (  501, 502  )"), e.ExprCall("f", [i501,i502]))
        self.assertEqual(parse_expression("501==502"), e.ExprCall("==", [i501,i502]))
        self.assertEqual(parse_expression("0 + 1 * 2 - 3 / 4"),
                         e.ExprCall('-',[e.ExprCall('+',
                                                [e.ExprInt(0),
                                                 e.ExprCall('*',[e.ExprInt(1), e.ExprInt(2)])
                                                ]),
                                       e.ExprCall('/',[e.ExprInt(3), e.ExprInt(4)])]))
        self.assertEqual(parse_expression("x < y"), e.ExprCall("<", [ e.ExprVar("x"), e.ExprVar("y") ]))

    def test_tuple(self):
        i1 = e.ExprInt(1)
        s1 = e.ExprString("A")
        self.assertEqual(parse_expression('((1), "A", "A")'), e.ExprTuple([i1, s1, s1]))

    def test_types(self):
        self.assertEqual(parse_type("Int"), t.t_int)
        self.assertEqual(parse_type("String"), t.t_string)
        typetuple = t.t_tuple(t.t_int, t.t_tuple(t.t_int, t.t_string), t.t_string)
        self.assertEqual(parse_type("(Int, (Int, String), String)"), typetuple)
        self.assertEqual(parse_type("Array(Int)"), t.t_array(t.t_int))

    def test_array(self):
        self.assertEqual(parse_expression("[]"), e.ExprArray(()))
        self.assertEqual(parse_expression("[1,2,3]"), e.ExprArray((e.ExprInt(1),e.ExprInt(2),e.ExprInt(3))))

    def test_output_inscription(self):
        self.assertEquals(parse_output_inscription("x_x"), ('normal', e.ExprVar("x_x"), None))
        self.assertEquals(parse_output_inscription("x_x@0"), ('normal', e.ExprVar("x_x"), e.ExprInt(0)))
        expr = ('normal', e.ExprCall("+", [ e.ExprInt(2), e.ExprVar("x")] ), e.ExprVar("y"))
        self.assertEquals(parse_output_inscription("(2 + x)    @    y"), expr)
        self.assertEquals(parse_output_inscription("~x_x"), ('packing', e.ExprVar("x_x"), None))
        self.assertEquals(parse_output_inscription("~x_x@0"), ('packing', e.ExprVar("x_x"), e.ExprInt(0)))

if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testExpression']
    unittest.main()
