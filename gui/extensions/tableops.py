
import settingswindow
import utils
from extensions import Parameter, Source, Operation, add_operation
from datatypes import t_table

class Filter(Operation):

    name = "Table filter"
    description = "Filter rows of the table by values in columns"
    parameters = [Parameter("Data", t_table)]

    def run(self, app, data):
        header, rows = data

        assistant = settingswindow.BasicSettingAssistant(2,
                                                         "Filter setting",
                                                         app.window)
        assistant.set_size_request(600, 400)

        def create_page_1(setting):
            items = [(label, idx, False) for idx, label in enumerate(header)]
            s_widget = settingswindow.SettingWidget()
            s_widget.add_checkbuttons_list(
                "selected_cols", "Columns", items, ["Column", "Select?"])
            return s_widget

        def create_page_2(setting):
            selected_columns = setting["selected_cols"]
            selected_columns.sort()
            s_widget = settingswindow.SettingWidget()

            for idx, col_idx in enumerate(selected_columns):
                if idx > 0:
                    s_widget.add_separator()
                s_widget.add_entry(col_idx, header[col_idx], "")
                s_widget.add_radiobuttons("cmp_fn{0}".format(col_idx),
                                          "Compare",
                                          [("Equal", lambda x, y: x == y),
                                           ("Not equal", lambda x, y: x != y)],
                                          ncols=2)
            return s_widget

        assistant.append_setting_widget("Select columns", create_page_1)
        assistant.append_setting_widget("Set filters", create_page_2)

        if not assistant.run():
            return None

        selected_columns = assistant.collected_setting[0]["selected_cols"]
        filter_by = assistant.collected_setting[1]

        cmp_fns = {}
        for col_idx in selected_columns:
            cmp_fns[col_idx] = filter_by.get("cmp_fn{0}".format(col_idx))

        # filter data
        def f(row):
            return all(cmp_fns[idx](filter_by[idx], row[idx])
                       for idx in selected_columns)

        filtered_data = (header, filter(f, rows))
        return Source("Filtered table " + utils.get_timestamp_string(),
                      t_table,
                      filtered_data)

add_operation(Filter)
