#
#    Copyright (C) 2011, 2013 Stanislav Bohm
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
import gtkutils

class SimConfig:

    process_count = 4
    parameters_values = None
    sequence = None

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
        gtk.Dialog.__init__(self, "Parameters configuration", parent)
        self.table_index = 1
        self.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
        self.ok_button = self.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
        self.ok_button.set_sensitive(False)
        self.ok_button.grab_default()

        self.table = gtk.Table()
        self.table.set_row_spacings(5)
        self.table.set_col_spacings(10)
        self.entries = {}
        self.vbox.pack_start(self.table)

        self.processes_entry = self._add_entry("<b>Processes</b>",
                                               str(project.simconfig.process_count))
        parameters = [ parameter for parameter in project.get_parameters()
                                 if parameter.is_editable() ]
        if parameters:
            self._add_separator()
            values = project.get_simconfig().parameters_values
            for i, p in enumerate(parameters):
                if values:
                    value = values.get(p.name)
                else:
                    value = None
                self._add_parameter(p, value)

        self._add_separator()
        sequences = [ (None, "<None>") ] + [ (s, s.name) for s in project.sequences ]
        self.sequences_box = gtkutils.SimpleComboBox(sequences)
        self.sequences_box.set_object(project.get_simconfig().sequence)
        self._add_widget("Sequence", self.sequences_box)

        self.table.show_all()
        self._entry_changed(None)

    def set_simconfig(self, project):
        simconfig = project.get_simconfig()
        result = {}
        for e in self.entries:
            result[e] = self.entries[e].get_text()
        simconfig.set_param_values(result)
        simconfig.sequence = self.sequences_box.get_object()

        if self.processes_entry:
            p = int(self.processes_entry.get_text())
            simconfig.set_process_count(p)

    def _add_separator(self):
        self.table.attach(gtk.HSeparator(), 0, 2, self.table_index, self.table_index + 1)
        self.table_index += 1

    def _add_widget(self, name, widget):
        if name is not None:
            label = gtk.Label()
            label.set_markup(name)
            self.table.attach(label, 0, 1, self.table_index, self.table_index + 1)
        self.table.attach(widget, 1, 2, self.table_index, self.table_index + 1)
        self.table_index += 1

    def _add_entry(self, name, default_value):
        entry = gtk.Entry()
        self._add_widget(name, entry)
        entry.set_text(default_value)
        entry.connect("changed", self._entry_changed)
        entry.set_activates_default(True)
        return entry

    def _add_parameter(self, param, value):
        name = "{0} ({1})".format(param.name, param.type)
        if value is None:
            value = param.default
        self.entries[param.name] = self._add_entry(name, value)

    def _entry_changed(self, w):
        ok = all([ entry.get_text().strip() for entry in self.entries.values() ])

        if self.processes_entry:
            try:
                v = int(self.processes_entry.get_text())
            except:
                v = 0
            ok = ok and v > 0
        self.ok_button.set_sensitive(ok)
