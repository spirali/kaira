#    Copyright (C) 2013, 2014 Martin Surkovsky
#                  2013 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
#

import settingswindow
import utils
from extensions import Parameter, Source, Operation, add_operation
from datatypes import t_table
from table import Table
from gtk import RESPONSE_APPLY

class Filter(Operation):

    name = "Table filter"
    description = "Filter rows of the table by values in columns"
    parameters = [Parameter("Data", t_table)]

    def run(self, app, table):
        assistant = settingswindow.BasicSettingAssistant(2,
                                                         "Filter setting",
                                                         app.window)
        assistant.set_size_request(600, 400)

        def create_page_1(setting):
            items = [(label, idx, False)
                     for idx, label in enumerate(table.header)]
            s_widget = settingswindow.SettingWidget()
            s_widget.add_checkbuttons_list(
                "selected_cols", "Columns", items, ["Column", "Select?"])
            return s_widget

        def create_page_2(setting):
            selected_columns = setting.get_value("selected_cols")
            selected_columns.sort()
            s_widget = settingswindow.SettingWidget()

            for idx, col_idx in enumerate(selected_columns):
                if idx > 0:
                    s_widget.add_separator()
                s_widget.add_entry("filter_value{0}".format(col_idx),
                                   table.header[col_idx],
                                   "")
                s_widget.add_radiobuttons("cmp_fn{0}".format(col_idx),
                                          "Compare",
                                          [("Equal", lambda x, y: x == y),
                                           ("Not equal", lambda x, y: x != y)],
                                          ncols=2)
            return s_widget

        assistant.append_setting_widget("Select columns", create_page_1)
        assistant.append_setting_widget("Set filters", create_page_2)

        if assistant.run() != RESPONSE_APPLY:
            return

        selected_columns = assistant.get_setting("selected_cols")

        filters = []
        for col_idx in selected_columns:
            cmp_function = assistant.get_setting("cmp_fn{0}".format(col_idx))
            value = assistant.get_setting("filter_value{0}".format(col_idx))
            value = utils.convert_to_type(table.types[col_idx], value)
            filters.append((table.header[col_idx], cmp_function, value))

        filtered = table.select(filters=filters)
        t = Table.create_from_data(filtered)
        return Source("Filtered table " + utils.get_timestamp_string(),
                      t_table,
                      t)

add_operation(Filter)
