
from extensions import Extension
from extensions import add_extension

from datatypes import t_tracelog

class TestExtension(Extension):

    def __init__(self):
        Extension.__init__(
            self,
            "Test action",
            "Only testin plugin\'s action")

    def init_parameters(self):
        self._dev_add_parameter("Tracelog 1", t_tracelog, list=True)
        self._dev_add_parameter("Tracelog 2", t_tracelog)

    def run(self):
        print "The testing extension was runned."

    def get_processed_data(self):
        return []

add_extension(TestExtension())
