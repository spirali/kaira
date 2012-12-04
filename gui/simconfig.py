#
#    Copyright (C) 2011 Stanislav Bohm
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

import gtk

class SimConfig:

    process_count = 2
    parameters_values = None

    def reset_param_values(self):
        self.parameters_values = None

    def set_param_values(self, values):
        self.parameters_values = values

    def set_process_count(self, value):
        self.process_count = value

class SimConfigDialog(gtk.Dialog):
    """
        This dialog is used when the simulation needs to know values of parameters
    """

    def __init__(self, parent, project):
        gtk.Dialog.__init__(self, "Simulaction configuration", parent)
        self.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
        self.ok_button = self.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
        self.ok_button.set_sensitive(False)
        self.ok_button.grab_default()

        self.table = gtk.Table()
        self.table.set_row_spacings(5)
        self.table.set_col_spacings(10)
        self.entries = {}
        self.vbox.pack_start(self.table)

        self.processes_entry = self._add_entry(0,
                                               "<b>Processes</b>",
                                               str(project.simconfig.process_count))
        self.table_index = 1
        parameters = [ parameter for parameter in project.get_parameters()
                                 if parameter.is_editable() ]
        for i, p in enumerate(parameters):
            self._add_parameter(i + 1, p)

        self.table.show_all()
        self._entry_changed(None)

    def set_simconfig(self, project):
        simconfig = project.get_simconfig()
        result = {}
        for e in self.entries:
            result[e] = self.entries[e].get_text()
        simconfig.set_param_values(result)
        p = int(self.processes_entry.get_text())
        simconfig.set_process_count(p)

    def _add_entry(self, table_index, name, default_value):
        label = gtk.Label()
        label.set_markup(name)
        self.table.attach(label, 0, 1, table_index, table_index + 1)
        entry = gtk.Entry()
        self.table.attach(entry, 1, 2, table_index, table_index + 1)
        entry.set_text(default_value)
        entry.connect("changed", self._entry_changed)
        entry.set_activates_default(True)
        return entry

    def _add_parameter(self, table_index, param):
        name = "{0} ({1})".format(param.name, param.type)
        self.entries[param.name] = self._add_entry(table_index, name, param.default)

    def _entry_changed(self, w):
        params_ok = all([ entry.get_text().strip() for entry in self.entries.values() ])
        try:
            v = int(self.processes_entry.get_text())
        except:
            v = 0
        self.ok_button.set_sensitive(params_ok and v > 0)
