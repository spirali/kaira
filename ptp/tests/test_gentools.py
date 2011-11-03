
import unittest
from base.gentools import match_expression
from base.expressions import ExprVar, ExprExtern, ExprCall, ExprTupleGet, ExprInt, ExprTuple
from base.expressions import ISet, IIf, IExtern
from base.expressions import Env
from base.neltypes import t_int, t_string, t_tuple
from base.project import order_input_edges, Edge

class TestBuilder(unittest.TestCase):

    def test_match(self):
        env = Env()
        context = { "x" : t_int, "y" : t_string, "z" : t_string }
        e = ExprTuple([ ExprInt(10), ExprVar("x"), ExprVar("y") ])
        vt = t_tuple(t_int, t_int, t_string)
        token = ExprExtern("value", vt)
        out = [
               IIf(ExprCall("!=", [ ExprInt(10), ExprTupleGet(token, 0, 3) ]), IExtern("fail")),
               IIf(ExprCall("!=", [ ExprVar("x"), ExprTupleGet(token, 1, 3) ]), IExtern("fail")),
               ISet("y", ExprTupleGet(token, 2, 3)),
              ]
        self.assertEqual(match_expression(env, context, e, set(["x"]), token), (out, set(["x", "y"])))

        e = ExprTuple([
                    ExprTuple([ExprCall("+", [ ExprVar("x"), ExprInt(1)]), ExprVar("x")]),
                    ExprTuple([ ExprVar("y"), ExprVar("y")])])
        vt = t_tuple(t_tuple(t_int, t_int), t_tuple(t_int, t_string))
        token = ExprExtern("value", vt)
        out = [
                ISet('x', ExprTupleGet(ExprTupleGet(token, 0, 2), 1, 2)),
                ISet('y', ExprTupleGet(ExprTupleGet(token, 1, 2), 0, 2)),
                IIf(ExprCall('!=',[ExprVar('y'), ExprTupleGet(ExprTupleGet(token, 1, 2), 1, 2)]),
                    IExtern("fail")),
                IIf(ExprCall('!=',[ExprCall('+',[ExprVar('x'), ExprInt(1)]),
                                   ExprTupleGet(ExprTupleGet(token, 0, 2), 0, 2)]),
                    IExtern("fail"))
                ]
        self.assertEqual(match_expression(env, context, e, set(["z"]), token), (out, set(["x", "y", "z"])))

    def test_order_edges(self):
        def edges(exprs):
            return [ Edge(0, 'normal', expr, None, None) for expr in exprs ]
        self.assertEqual(order_input_edges([]), [])
        e1 = edges([ExprVar("x"), ExprCall("+", [ ExprVar("y"), ExprVar("x") ] ), ExprVar("y")])
        e2 = edges([ExprVar("x"), ExprVar("y"), ExprCall("+", [ ExprVar("y"), ExprVar("x") ])])
        self.assertEqual(order_input_edges(e1), e2)
        e1 = edges([ExprCall("+", [ ExprVar("y"), ExprVar("x") ] ),
                    ExprTuple([ExprCall("+", [ ExprInt(1), ExprVar("y")]), ExprVar("x")]),
                    ExprVar("y")])
        e2 = [ e1[2], e1[1], e1[0] ]
        self.assertEqual(order_input_edges(e1), e2)
        e1 = edges([ ExprTuple([ExprCall("f",[ ExprVar("a")]), ExprVar("b")]),
                     ExprTuple([ExprVar("a"), ExprCall("f", [ ExprVar("b") ])])])
        self.assertRaises(Exception, order_input_edges, e1)

        e1 = edges([ ExprTuple([ExprCall("f",[ ExprVar("a")]), ExprVar("b")]),
                     ExprTuple([ExprCall("f",[ ExprVar("a") ]), ExprCall("f", [ ExprVar("b") ])])])
        self.assertRaises(Exception, order_input_edges, e1)

if __name__ == "__main__":
    unittest.main()