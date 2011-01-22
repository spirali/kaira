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
from objectlist import ObjectList
from codeedit import CodeEditor
import gtkutils

class FunctionsWidget(ObjectList):

	def __init__(self, project, app):
		defs = [("_", object), ("Name", str), ("Return type", str), ("Context", bool), ("Parameters", str) ]
		buttons = [
			(None, gtk.STOCK_ADD, self._add_function),
			(None, gtk.STOCK_REMOVE, self._remove_function),
			(None, gtk.STOCK_EDIT, self._edit_function),
			("Edit _code", None, self._edit_function_code)
		]

		ObjectList.__init__(self, defs, buttons)
		self.project = project
		self.app = app

		self.fill(project.functions)

	def object_as_row(self, obj):
		return [obj, obj.get_name(), obj.get_return_type(), obj.get_with_context(), obj.get_parameters()]

	def row_activated(self, selected):
		self._edit_function_code(selected)

	def _dummy(self):
		pass

	def _add_function(self, selected):
		obj = self.project.new_function()
		if function_dialog(obj, self.app.window):
			self.add_object(obj)

	def _edit_function(self, selected):
		if selected and function_dialog(selected, self.app.window):
			self.update_selected(selected)

	def _remove_function(self, selected):
		if selected:
			obj = self.get_and_remove_selected()
			self.project.remove_function(obj)

	def _edit_function_code(self, obj):
		if obj is not None:
			if obj.check_definition():
				self.app.function_edit(obj)
			else:
				self.app.console_write("Invalid definition of function '%s'\n"
					% obj.get_name(), "error")


class EventsWidget(ObjectList):
	
	def __init__(self, project, app):
		defs = [("_", object), ("Name", str) ]
		buttons = [
			("Edit code", None, self._edit_code) ]

		ObjectList.__init__(self, defs, buttons)
		self.project = project
		self.app = app

		self.fill(project.get_events())

	def object_as_row(self, obj):
		return [obj, obj.get_name()]

	def row_activated(self, selected):
		self._edit_code(selected)

	def _edit_code(self, obj):
		if obj is not None:
			self.app.function_edit(obj)

class FunctionEditor(CodeEditor):

	def __init__(self, function):
		self.function = function
		declaration = function.get_function_declaration()
		code = function.get_function_code()
		CodeEditor.__init__(self, declaration + "\n{\n", code, "}\n", (2, 1))

	def buffer_changed(self, buffer):
		self.function.set_function_code(self.get_text())


def function_dialog(function, mainwindow):
	builder = gtkutils.load_ui("function-dialog")
	dlg = builder.get_object("function-dialog")
	try:
		wname = builder.get_object("name")
		wname.set_text(function.get_name())

		wreturn = builder.get_object("return")
		wreturn.set_text(function.get_return_type())

		wparam = builder.get_object("parameters")
		wparam.set_text(function.get_parameters())

		wcontext = builder.get_object("context")
		wcontext.set_active(function.get_with_context())

		dlg.set_title("Function")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			function.set_name(wname.get_text())
			function.set_return_type(wreturn.get_text())
			function.set_parameters(wparam.get_text())
			function.set_with_context(wcontext.get_active())
			return True
		return False
	finally:
		dlg.destroy()

