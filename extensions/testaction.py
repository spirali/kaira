
from extensions import Argument, Source, Operation, add_operation
from datatypes import t_tracelog, t_contseq

class TestOperation(Operation):
    _name = "Testing Operation"
    _description = "Operation do serves only for tests"
    _arguments = (
        Argument("Tracelog", t_tracelog),
        Argument("Tracelogs 1", t_tracelog, list=True, minimum=3),
        Argument("Tracelogs 2", t_tracelog, list=True))

    def run(self, tracelog, tracelogs1, tracelogs2):

        return [Source("Result 1", t_tracelog, "Hello"),
                Source("Result 2", t_contseq, "Hi")]

add_operation(TestOperation)
