
import unittest

from base.parser import parse_expression
from base.expressions import Env, ExprVar, ExprInt, ExprCall, ExprTuple, ExprString, ExprTupleGet
from base.expressions import ExprCast
from base.neltypes import Type, TypeVar, t_int, t_string, t_array, t_tuple, t_bool
from base.neltypes import derive_context, fresh_typevar, unify


class TestNel(unittest.TestCase):

    def test_typevariables(self):
        t = fresh_typevar()
        self.assertEqual(len(t.get_variables()), 1)
        self.assertEqual(t_int.get_variables(), set())
        t = Type("", [ TypeVar(0), TypeVar(1), Type("", [ TypeVar(0) ])])
        self.assertEqual(t.get_variables(), set([0, 1]))
        table = { 0 : t_int }
        t2 = Type("", [ t_int, TypeVar(1), Type("", [ t_int ])])
        self.assertEqual(t.replace_vars(table), t2)

    def test_derivecontext(self):
        e = ExprVar("x")
        self.assertEqual(e.derive_context(t_int), { "x" : t_int })
        e = ExprTuple([ExprVar("x"), ExprCall("+", [ ExprVar("x"), ExprVar("z")]), ExprVar("y")])
        t = t_tuple(t_int, t_int, t_string)
        self.assertEqual(e.derive_context(t), { "x" : t_int, "y" : t_string })
        e = ExprTuple([ExprVar("x"), ExprVar("x")])
        t = t_tuple(t_int, t_int)
        self.assertEqual(e.derive_context(t), { "x" : t_int })
        t = t_tuple(t_int, t_string)
        self.assertRaises(Exception, e.derive_context, t)

    def test_context(self):
        env = Env()
        defs = [ (parse_expression("at(x, y)"), t_string) ]
        self.assertEqual(derive_context(env, defs), { "x" : t_array(t_string), "y" : t_int } )
        defs = [ (parse_expression("(x, y)"), t_tuple(t_int, t_string)) ]
        self.assertEqual(derive_context(env, defs), { "x" : t_int, "y" : t_string } )
        defs = [ (parse_expression('(x, "String") == (10, y)"'), t_bool) ]
        self.assertEqual(derive_context(env, defs), { "x" : t_int, "y" : t_string } )
        defs = [ (parse_expression('at(at(at(x, y), 50), length(z))'), t_bool),
                 (parse_expression('at(z, 10) == y'), t_bool) ]
        self.assertEqual(derive_context(env, defs),
                         { "x" : t_array(t_array(t_array(t_bool))),
                          "y" : t_int,
                          "z" : t_array(t_int) } )
        defs = [ (ExprCall( "==", [ ExprVar ("aa"), ExprCast(ExprVar("bb"), t_string)]), t_bool) ]
        self.assertEqual(derive_context(env, defs), { "aa" : t_string, "bb" : t_string } )

        t = ExprTuple([ ExprVar("x"), ExprVar("y") ])
        defs = [ (t, t_tuple(t_int, t_string)),
                 (ExprTupleGet(t, 1, 2), t_string) ]
        self.assertEqual(derive_context(env, defs), { "x" : t_int, "y" : t_string } )

        defs = [ (parse_expression('y == [1,2,x]'), t_bool) ]
        self.assertEqual(derive_context(env, defs), { "x" : t_int, "y" : t_array(t_int) } )

    def test_unify(self):
        eq = [ (t_int, t_int) ]
        self.assertEqual(unify(eq), {})
        eq = [ (TypeVar(0), TypeVar(1)), (t_string, TypeVar(0)) ]
        self.assertEquals(unify(eq), { 0: t_string, 1: t_string })
        eq = [ (t_string, TypeVar(2)), (TypeVar(0), TypeVar(1)), (TypeVar(0), TypeVar(2)) ]
        self.assertEquals(unify(eq), { 0: t_string, 1: t_string, 2: t_string })
        eq = [ (t_array(TypeVar(0)), TypeVar(1)),
              (TypeVar(1), TypeVar(2)), (t_array(t_int), TypeVar(2)) ]
        self.assertEquals(unify(eq), { 0: t_int, 1: t_array(t_int), 2: t_array(t_int) })
        self.assertRaises(Exception, unify, [(t_string, t_int)])

    def test_directvars(self):
        e = ExprVar("x")
        self.assertEqual(e.get_direct_vars(), set(["x"]))
        e = ExprCall("x", [ ExprVar("x"), ExprInt(10) ])
        self.assertEqual(e.get_direct_vars(), set())
        e = ExprTuple([ ExprVar("y"), ExprVar("x"), ExprTuple([ ExprVar("z"), ExprVar("x")]), ExprCall("name", [ ExprVar("f")]) ])
        self.assertEqual(e.get_direct_vars(), set(["x", "y", "z"]))

    def test_freevars(self):
        e = ExprVar("x")
        self.assertEqual(e.get_free_vars(), set(["x"]))
        e = ExprCall("x", [ ExprVar("x"), ExprInt(10) ])
        self.assertEqual(e.get_free_vars(), set(["x"]))
        e = ExprTuple([ ExprVar("y"), ExprVar("x"), ExprTuple([ ExprVar("z"), ExprVar("x")]), ExprCall("name", [ ExprVar("f")]) ])
        self.assertEqual(e.get_free_vars(), set(["x", "y", "z", "f"]))

    def test_get_direct_pairing(self):
        e = ExprVar("x")
        self.assertEqual(e.get_direct_pairing(ExprString("String")), [ (ExprString("String"), ExprVar("x")) ] )
        e = ExprTuple([ExprString("x"), ExprTuple([ExprVar("x"), ExprInt(325)])])
        a = [ (ExprTupleGet(ExprVar("x"), 0, 2), ExprString("x")),
              (ExprTupleGet(ExprTupleGet(ExprVar("x"), 1, 2), 0, 2), ExprVar("x")),
              (ExprTupleGet(ExprTupleGet(ExprVar("x"), 1, 2), 1, 2), ExprInt(325)) ]
        self.assertEqual(e.get_direct_pairing(ExprVar("x")), a)

    def test_subtypes(self):
        self.assertEqual(t_array(t_int).get_subtypes(), set([t_int, t_array(t_int)]))
        x = t_tuple(t_string, t_array(t_bool))
        t = t_tuple(t_int, x)
        r = set([t, t_int, t_bool, t_string, t_array(t_bool), x])
        self.assertEqual(t.get_subtypes(), r)

    def test_inject_type(self):
        env = Env()
        e = ExprVar("x")
        e.inject_types(env, { "x" : t_string })
        self.assertEqual(e.nel_type, t_string)
        e = ExprTuple( [ ExprVar("x"), ExprString("s") ])
        e.inject_types(env, { "x" : t_string })
        self.assertEqual(e.nel_type, t_tuple(t_string, t_string))

        e1 = ExprTuple( [ ExprVar("x"), ExprString("s") ])
        e2 = ExprCall("==", [ e1, ExprVar("z") ] )
        e2.inject_types(env, { "x" : t_int, "z" : t_tuple(t_int, t_string) })
        self.assertEqual(e1.nel_type, t_tuple(t_int, t_string))
        self.assertEqual(e2.nel_type, t_bool)

        e1 = ExprCall("at", [ ExprVar("abc"),  ExprInt(10) ])
        e1.inject_types(env, { "abc" : t_array(t_int)})
        self.assertEqual(e1.nel_type, t_int)

        e1 = ExprVar("tuple")
        e2 = ExprTupleGet(e1, 2)
        e2.inject_types(env, { "tuple" : t_tuple(t_int, t_string, t_int)})
        self.assertEqual(e1.nel_type, t_tuple(t_int, t_string, t_int))
        self.assertEqual(e2.nel_type, t_int)
        self.assertEqual(e2.tuplesize, 3)

if __name__ == "__main__":
    unittest.main()