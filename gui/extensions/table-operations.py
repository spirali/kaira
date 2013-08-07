
import gtk
import settingswindow
from extensions import Argument, Source, Operation, add_operation
from datatypes import t_table

class Filter(Operation):

    name = "Table filter"
    description = "Data are filtered by a value(s) in specific column(s)"
    arguments = [Argument("Data", t_table)]

    def run(self, app, data):
        header, rows = data

        assistant = settingswindow.BasicSettingAssistant(2,
                                                         "Filter setting",
                                                         app.window)

        def create_page_1(setting):
            items = [(label, idx) for idx, label in enumerate(header)]
            s_widget = settingswindow.SettingsWidget()
            s_widget.add_checkbuttons(
                "selected_cols", "Columns", items, ncols=3)
            return s_widget

        def create_page_2(setting):
            selected_columns = setting["selected_cols"]
            selected_columns.sort()
            s_widget = settingswindow.SettingsWidget()
            for idx, col_idx in enumerate(selected_columns):
                if idx > 0:
                    s_widget.add_separator()
                s_widget.add_entry(col_idx, header[col_idx], "")
                s_widget.add_checkbutton("neg{0}".format(col_idx),
                                         "Negation", False)
            return s_widget

        assistant.append_setting_widget("Select columns", create_page_1)
        assistant.append_setting_widget("Set filters", create_page_2)

        response = assistant.run()
        if response != gtk.RESPONSE_OK:
            return None

        selected_columns = assistant.collected_setting[0]["selected_cols"]
        filter_by = assistant.collected_setting[1]
        print "Filter by: ", filter_by

        filters = {}
        for col_idx in selected_columns:
            filters[col_idx] = (filter_by.get(col_idx),
                                filter_by.get("neg{0}".format(col_idx)))

        # filter data
        def f(row):
            for col_idx in selected_columns:
                filter, negation = filters[col_idx]
                if negation:
                    if row[col_idx] == filter: # negate of condition
                        return False
                else:
                    if row[col_idx] != filter:
                        return False
            return True

        filtered_data = (header, filter(f, rows))
        return Source("Filtered table", t_table, filtered_data)

add_operation(Filter)
