

import unittest

from base.parser import parse_expression
from gen.writer import Writer
from gen.emitter import Emitter
from base.expressions import ISet, Env
from base.neltypes import t_int, t_string, t_array, t_tuple, t_bool, Type

class TestGen(unittest.TestCase):


    def test_writer(self):
        w = Writer()
        w.line("test1")
        w.indent_push()
        w.line("test{0}", 12)
        w.line("nextline")
        w.indent_push()
        w.line("line")
        w.indent_pop()
        w.emptyline()
        w.line("end")

        text = "test1\n\ttest12\n\tnextline\n\t\tline\n\n\tend\n"

        self.assertEqual(w.get_string(), text)

    def test_emit_expressions(self):
        e = Emitter()
        expr = parse_expression('g(10,"Hi!", y)')
        self.assertEqual(expr.emit(e), 'g(10, "Hi!", y)')
        expr = parse_expression('10 + x')
        self.assertEqual(expr.emit(e), '((10) + (x))')

    def test_emit_instructions(self):
        env = Env()
        i = ISet("a", parse_expression('10 + 20'))
        i.inject_types(env, {})
        e, w  = Emitter(), Writer()
        i.emit(e, w)
        self.assertEqual(w.get_string(), 'a = ((10) + (20));\n')

    def test_emit_type(self):
        e = Emitter()
        self.assertEqual(e.emit_type(t_int), "int")
        self.assertEqual(e.emit_type(t_string), "std::string")
        self.assertEqual(e.emit_type(t_array(t_int)), "std::vector<int>")
        self.assertEqual(e.emit_type(Type("__Place", [t_bool])), "CaPlace<bool>")
        self.assertEqual(e.emit_type(t_tuple(t_int, t_string)), "Tuple2_Int_String")


if __name__ == "__main__":
    #import sys;sys.argv = ['', 'Test.testWriter']
    unittest.main()