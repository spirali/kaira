
import gtk
import gtkutils

def parameters_dialog(builder, parameter, mainwindow):

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
	
	def __init__(self, builder, project, mainwindow):
		gtk.VBox.__init__(self)
		self.builder = builder
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
		if parameters_dialog(self.builder, param, self.mainwindow):
			self.list.append(self._param_as_list(param))

	def _edit_parameter(self, w):
		param = self.list.get_selection(0)
		if param is None:
			return
		if parameters_dialog(self.builder, param, self.mainwindow):
			self.list.set_selection_all(self._param_as_list(param))

	def _remove_parameter(self, w):
		param = self.list.get_and_remove_selection(0)
		if param:
			self.project.remove_parameter(param)

	def _param_as_list(self, parameter):
		return [parameter, parameter.get_name(), "Mandatory", parameter.get_type(), "", parameter.get_description()]
