#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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

def extern_type_dialog(obj, mainwindow):
	builder = gtkutils.load_ui("externtype-dialog")
	dlg = builder.get_object("externtype-dialog")
	try:

		wname = builder.get_object("name")
		wname.set_text(obj.get_name())

		wrtype = builder.get_object("raw_type")
		wrtype.set_text(obj.get_raw_type())

		mode_disabled = builder.get_object("mode_disabled")
		mode_direct = builder.get_object("mode_direct")
		mode_custom = builder.get_object("mode_custom")

		if obj.get_transport_mode() == "Custom":
			mode_custom.set_active(True)
		elif obj.get_transport_mode() == "Direct":
			mode_direct.set_active(True)
		else:
			mode_disabled.set_active(True)

		dlg.set_title("Extern type")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			obj.set_name(wname.get_text())
			obj.set_raw_type(wrtype.get_text())
			if mode_custom.get_active():
				obj.set_transport_mode("Custom")
			elif mode_direct.get_active():
				obj.set_transport_mode("Direct")
			else:
				obj.set_transport_mode("Disabled")

			return True
		return False
	finally:
		dlg.destroy()

class ExternTypesWidget(ObjectList):

	def __init__(self, project, app):
		defs = [("_", object), ("Name", str), ("Raw type", str), ("Transport", str), ("Functions", str) ]
		buttons = [
			(None, gtk.STOCK_ADD, self._add_type),
			(None, gtk.STOCK_REMOVE, self._remove_type),
			(None, gtk.STOCK_EDIT, self._edit_type) ]

		rpanel = gtk.VButtonBox()
		rpanel.set_layout(gtk.BUTTONBOX_START)
		self.fbuttons = {}
		for name in ["getstring", "getsize", "pack", "unpack"]:
			button = gtk.Button(name)
			button.set_sensitive(False)
			button.connect("clicked", self._fbutton_callback(name))
			self.fbuttons[name] = button
			rpanel.add(button)

		ObjectList.__init__(self, defs, buttons, rpanel = rpanel)
		self.project = project
		self.app = app
		self.fill(project.get_extern_types())

	def row_activated(self, selected):
		self._edit_type(selected)

	def _fbutton_callback(self, name):
		def update(extern_type, fn_name):
			self.update(extern_type)
		return lambda w: self.app.extern_type_function_edit(self.selected_object(), name, update)

	def object_as_row(self, obj):
		return [obj, obj.get_name(), obj.get_raw_type(), obj.get_transport_mode(), obj.get_function_list_string()]

	def _add_type(self, selected):
		obj = self.project.get_exttype_class()()
		if extern_type_dialog(obj, self.app.window):
			self.add_object(obj)
			self.project.add_extern_type(obj)

	def _edit_type(self, selected):
		if selected and extern_type_dialog(selected, self.app.window):
			self.update_selected(selected)
			self._fbuttons_update(selected)

	def _remove_type(self, selected):
		if selected:
			for b in self.fbuttons.values():
				b.set_sensitive(False)
			obj = self.get_and_remove_selected()
			self.project.remove_extern_type(obj)

	def cursor_changed(self, obj):
		self._fbuttons_update(obj)

	def _fbuttons_update(self, obj):
		if obj is None:
			for button in self.fbuttons.values():
				button.set_sensitive(False)
		else:
			for name, button in self.fbuttons.items():
				button.set_sensitive(obj.is_function_allowed(name))

class ExternTypeEditor(CodeEditor):

	def __init__(self, project, extern_type, fn_name, change_callback):
		self.extern_type = extern_type
		self.fn_name = fn_name
		self.change_callback = change_callback
		declaration = extern_type.get_function_declaration(fn_name)
		code = extern_type.get_function_code(fn_name)
		start = "\n{\n"
		end = "}\n"
		CodeEditor.__init__(self, project.get_syntax_highlight_key(), declaration + start, code, end, (2, 1))

	def buffer_changed(self, buffer):
		self.extern_type.set_function_code(self.fn_name, self.get_text())
		self.change_callback(self.extern_type, self.fn_name)
