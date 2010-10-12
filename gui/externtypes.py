
import gtk
import gtkutils
from objectlist import ObjectList
from codeedit import CodeEditor

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


		for obj in project.get_extern_types():
			self.add_object(obj)

	def _fbutton_callback(self, name):
		def update(extern_type, fn_name):
			self.update(extern_type)
		return lambda w: self.app.extern_type_function_edit(self.selected_object(), name, update)

	def object_as_row(self, obj):
		return [obj, obj.get_name(), obj.get_raw_type(), obj.get_transport_mode(), obj.get_function_list_string()]

	def _add_type(self, selected):
		obj = self.project.new_extern_type()
		if extern_type_dialog(obj, self.app.window):
			self.add_object(obj)

	def _edit_type(self, selected):
		if selected and extern_type_dialog(selected, self.app.window):
			self.update_selected(selected)

	def _remove_type(self, selected):
		if selected:
			for b in self.fbuttons.values():
				b.set_sensitive(False)
			obj = self.get_and_remove_selected()
			self.project.remove_extern_type(obj)

	def cursor_changed(self, obj):
		if obj is None:
			state = False
			self.fbuttons["getstring"].set_sensitive(False)
		else:
			state = obj.get_transport_mode() == "Custom"
			self.fbuttons["getstring"].set_sensitive(True)
		self.fbuttons["getsize"].set_sensitive(state)
		self.fbuttons["pack"].set_sensitive(state)
		self.fbuttons["unpack"].set_sensitive(state)

class ExternTypeEditor(CodeEditor):

	def __init__(self, extern_type, fn_name, change_callback):
		self.extern_type = extern_type
		self.fn_name = fn_name
		self.change_callback = change_callback
		declaration = extern_type.get_function_declaration(fn_name)
		code = extern_type.get_function_code(fn_name)
		CodeEditor.__init__(self, declaration + "\n{\n", code, "}\n", (2, 1))

	def buffer_changed(self, buffer):
		self.extern_type.set_function_code(self.fn_name, self.get_text())
		self.change_callback(self.extern_type, self.fn_name)
