
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
        items = [(label, idx) for idx, label in enumerate(header)]
        setting = settingswindow.SettingsWidget()
        setting.add_checkbuttons("selected_cols", "Columns", items, ncols=3)

        # choose columns for filtering
        dialog = settingswindow.BasicSettingDialog(setting, "Select columns", app.window)
        button = dialog.add_button(gtk.STOCK_GO_FORWARD, gtk.RESPONSE_OK)
        dialog.add_protected_button(button)
        response = dialog.run()
        if response != gtk.RESPONSE_OK:
            return

        dialog.destroy()
        selected_columns = setting.get("selected_cols")

        # set filters
        setting = settingswindow.SettingsWidget()
        for idx, col_idx in enumerate(selected_columns):
            if idx > 0:
                setting.add_separator()
            setting.add_entry(col_idx, header[col_idx], "")
            setting.add_checkbutton("neg{0}".format(col_idx), "Negation", False)

        dialog = settingswindow.BasicSettingDialog(setting, "Set filter", app.window)
        button  = dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
        dialog.add_protected_button(button)
        response = dialog.run()
        if response != gtk.RESPONSE_OK:
            return

        dialog.destroy()
        filters = {}
        for col_idx in selected_columns:
            filters[col_idx] = (setting.get(col_idx),
                                setting.get("neg{0}".format(col_idx)))

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
