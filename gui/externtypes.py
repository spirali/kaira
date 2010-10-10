
import gtk
import gtkutils
from objectlist import ObjectList

def extern_type_dialog(obj, mainwindow):
	builder = gtkutils.load_ui("externtype-dialog")
	dlg = builder.get_object("externtype-dialog")
	try:

		wname = builder.get_object("name")
		wname.set_text(obj.get_name())

		wrtype = builder.get_object("raw_type")
		wrtype.set_text(obj.get_raw_type())

		dlg.set_title("Extern type")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			obj.set_name(wname.get_text())
			obj.set_raw_type(wrtype.get_text())
			return True
		return False
	finally:
		dlg.destroy()

class ExternTypesWidget(ObjectList):
	
	def __init__(self, project, mainwindow):
		defs = [("_", object), ("Name", str), ("Raw type", str), ("Transport", str) ]
		buttons = [
			(None, gtk.STOCK_ADD, self._add_type), 
			(None, gtk.STOCK_REMOVE, self._remove_type), 
			(None, gtk.STOCK_EDIT, self._edit_type) ]
		ObjectList.__init__(self, defs, buttons)
		self.project = project
		self.mainwindow = mainwindow
		
		for obj in project.get_extern_types():
			self.add_object(obj)

	def object_as_row(self, obj):
		return [obj, obj.get_name(), obj.get_raw_type(), obj.get_transport_mode()]

	def _add_type(self, selected):
		obj = self.project.new_extern_type()
		if extern_type_dialog(obj, self.mainwindow):
			self.add_object(obj)

	def _edit_type(self, selected):
		if selected and extern_type_dialog(selected, self.mainwindow):
			self.update_selected(selected)

	def _remove_type(self, selected):
		if selected:
			obj = self.get_and_remove_selected()
			self.project.remove_extern_type(obj)

