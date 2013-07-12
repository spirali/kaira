
from extensions import Extension
from extensions import add_extension

from datatypes import repository as types_repo

class TestExtension(Extension):

    def __init__(self):
        Extension.__init__(
            self,
            "Test action",
            "Only testin plugin\'s action")

    def init_parameters(self):
        kth_type = types_repo.get_type("kth")
        self._dev_add_parameter("Tracelog 1", kth_type, list=True)
        self._dev_add_parameter("Tracelog 2", kth_type)

    def run(self):
        print "The testing extension was runned."

    def get_processed_data(self):
        return []

add_extension(TestExtension())
