
import gtk
import gtkutils

def parameters_dialog(parameter, mainwindow):
	builder = gtkutils.load_ui("parameter-dialog")
	dlg = builder.get_object("parameter-dialog")
	try:

		wname = builder.get_object("name")
		wname.set_text(parameter.get_name())

		wdesc = builder.get_object("description")
		wdesc.set_text(parameter.get_description())

		dlg.set_title("Parameter")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			parameter.set_name(wname.get_text())
			parameter.set_description(wdesc.get_text())
			return True
		return False
	finally:
		dlg.hide()

class ParametersWidget(gtk.VBox):
	
	def __init__(self, project, mainwindow):
		gtk.VBox.__init__(self)
		self.project = project
		self.mainwindow = mainwindow
		self.pack_start(self._buttons(), False, False)
		
		self.list = gtkutils.SimpleList(
			[("_", object), ("Name", str), ("Policy", str), ("Type", str), ("Default", str), ("Description", str) ])
		self.pack_start(self.list)

		for p in project.get_parameters():
			self.list.append(self._param_as_list(p))

		self.show_all()

	def _buttons(self):
		box = gtk.HButtonBox() 
		box.set_layout(gtk.BUTTONBOX_START)

		button = gtk.Button(stock=gtk.STOCK_ADD)
		button.connect("clicked", self._add_parameter)
		box.add(button)
		button = gtk.Button(stock=gtk.STOCK_REMOVE)
		button.connect("clicked", self._remove_parameter)
		box.add(button)
		button = gtk.Button(stock=gtk.STOCK_EDIT)
		button.connect("clicked", self._edit_parameter)
		box.add(button)
	
		box.show_all()
		return box

	def _add_parameter(self,w):
		param = self.project.new_parameter()
		if parameters_dialog(param, self.mainwindow):
			self.list.append(self._param_as_list(param))

	def _edit_parameter(self, w):
		param = self.list.get_selection(0)
		if param is None:
			return
		if parameters_dialog(param, self.mainwindow):
			self.list.set_selection_all(self._param_as_list(param))

	def _remove_parameter(self, w):
		param = self.list.get_and_remove_selection(0)
		if param:
			self.project.remove_parameter(param)

	def _param_as_list(self, parameter):
		return [parameter, parameter.get_name(), "Mandatory", parameter.get_type(), "", parameter.get_description()]

class ParametersValueDialog(gtk.Dialog):

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

	def _add_parameter(self,param):
		label = gtk.Label(param.get_name() + " (" + param.get_type() + ")")
		self.table.attach(label, 0, 1, self.param_counter, self.param_counter + 1)

		entry = gtk.Entry()
		self.table.attach(entry, 1, 2, self.param_counter, self.param_counter + 1)
		entry.connect("changed", self._entry_changed)
		self.entries[param.get_name()] = entry

		self.param_counter += 1

	def _entry_changed(self, w):
		texts = [ entry.get_text() for entry in self.entries.values() ]
		self.ok_button.set_sensitive(all( (t.strip() != "" for t in texts) ))
