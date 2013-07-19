
from extensions import Operation
from extensions import Argument
from extensions import add_operation

from datatypes import t_tracelog

class TestOperation(Operation):
    _name = "Testing Operation"
    _description = "Operation do serves only for tests"
    _arguments = [
        Argument("Tracelog", t_tracelog),
        Argument("Tracelogs 1", t_tracelog, list=True, minimum=3),
        Argument("Tracelogs 2", t_tracelog, list=True)]

    def run(self, tracelog, tracelogs1, tracelogs2):
        print "Operation was finished."
        return None

add_operation(TestOperation)
