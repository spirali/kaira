import actionselector
from stypes import repository as types_repo

class TestAction(actionselector.Action):

    def __init__(self):
        actionselector.Action.__init__(
            self,
            "Test action",
            "Only testin plugin\'s action")

        kth_type = types_repo.get_category("kth")
        self._add_parameter("Tracelog 1", kth_type)
        self._add_parameter("Tracelog 2", kth_type)

    def run(self):
        actionselector.Action.run() # check parameters TODO: add into origin
        print "TestAction was runned."

    def get_processed_data(self):
        return []

actionselector.add_plugin(TestAction())
