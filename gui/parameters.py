#
#    Copyright (C) 2010 Stanislav Bohm
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
from objectlist import ObjectList

def parameters_dialog(parameter, mainwindow):
	builder = gtkutils.load_ui("parameter-dialog")
	dlg = builder.get_object("parameter-dialog")
	try:

		wname = builder.get_object("name")
		wname.set_text(parameter.get_name())

		wdesc = builder.get_object("description")
		wdesc.set_text(parameter.get_description())

		wdefault = builder.get_object("default")
		wdefault.set_text(parameter.get_default())

		dlg.set_title("Parameter")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			parameter.set_name(wname.get_text())
			parameter.set_description(wdesc.get_text())
			parameter.set_default(wdefault.get_text())
			return True
		return False
	finally:
		dlg.destroy()

class ParametersWidget(ObjectList):
	
	def __init__(self, project, mainwindow):
		defs = [("_", object), ("Name", str), ("Policy", str), ("Type", str), ("Default", str), ("Description", str) ]
		buttons = [
			(None, gtk.STOCK_ADD, self._add_parameter), 
			(None, gtk.STOCK_REMOVE, self._remove_parameter), 
			(None, gtk.STOCK_EDIT, self._edit_parameter) ]
		ObjectList.__init__(self, defs, buttons)
		self.project = project
		self.mainwindow = mainwindow
		
		self.fill(project.get_parameters())

	def object_as_row(self, parameter):
		return [ parameter, parameter.get_name(),
				"Mandatory", parameter.get_type(), parameter.get_default(),
				parameter.get_description()]

	def row_activated(self, selected):
		self._edit_parameter(selected)

	def _add_parameter(self, selected):
		param = self.project.new_parameter()
		if parameters_dialog(param, self.mainwindow):
			self.add_object(param)

	def _edit_parameter(self, selected):
		if selected and parameters_dialog(selected, self.mainwindow):
			self.update_selected(selected)

	def _remove_parameter(self, selected):
		if selected:
			param = self.get_and_remove_selected()
			self.project.remove_parameter(param)


class ParametersValueDialog(gtk.Dialog):
	"""
		This dialog is used when the simulation needs to know values of parameters
	"""

	def __init__(self, parent, parameters):
		gtk.Dialog.__init__(self, "Parameters", parent)
		self.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
		self.ok_button = self.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
		self.ok_button.set_sensitive(False)

		self.table = gtk.Table()
		self.table.set_row_spacings(5)
		self.table.set_col_spacings(10)
		self.entries = {}
		self.param_counter = 0
		self.vbox.pack_start(self.table)

		for p in parameters:
			self._add_parameter(p)

		self.table.show_all()

	def get_values(self):
		result = {}
		for e in self.entries:
			result[e] = self.entries[e].get_text()
		return result

	def _add_parameter(self, param):
		label = gtk.Label(param.get_name() + " (" + param.get_type() + ")")
		self.table.attach(label, 0, 1, self.param_counter, self.param_counter + 1)

		entry = gtk.Entry()
		self.table.attach(entry, 1, 2, self.param_counter, self.param_counter + 1)
		entry.connect("changed", self._entry_changed)
		entry.set_text(param.get_default())
		self.entries[param.get_name()] = entry

		self.param_counter += 1

	def _entry_changed(self, w):
		texts = [ entry.get_text() for entry in self.entries.values() ]
		self.ok_button.set_sensitive(all( (t.strip() != "" for t in texts) ))
