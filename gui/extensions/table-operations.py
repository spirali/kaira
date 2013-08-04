
from extensions import Argument, Source, Operation, add_operation
from datatypes import t_table

class Filter(Operation):

    name = "CSV Filter"
    description = "Data are filtered by a value(s) in specific column(s)"
    arguments = [Argument("Data", t_table)]

    def run(data):
        return Source("Filtered CSV", t_table, data)

add_operation(Filter)
