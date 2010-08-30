import gtk

import project
from mainwindow import MainWindow
from netview import NetView
from codeedit import TransitionCodeEditor
from codeedit import PlaceCodeEditor
from parameters import ParametersWidget

class App:
	
	def __init__(self):
		self.builder = gtk.Builder()
		self.builder.add_from_file("gui.glade")
		self.window = MainWindow(self)
		self.nv = None
		self.tabtable = {}
		self.set_project(project.new_empty_project())

	def run(self):
		self.window.show()
		gtk.main()

	def set_project(self, project):
		self.project = project
		self.project.set_change_callback(self._project_changed)
		self.init_tabs()

		self._project_changed(self.project)

	def init_tabs(self):
		if self.nv:
			self.window.close_tab(self.nv)
		for t in self.tabtable:
			self.window.close_tab(self.tabtable[t])
		self.nv = NetView(self.project.net)
		self.nv.transition_edit_callback = self.transition_edit
		self.nv.place_edit_callback = self.place_edit
		self.window.add_tab("Network", self.nv)
		self.tabtable = {}

	def load_project(self, widget):
		dialog = gtk.FileChooserDialog("Open project", self.window, gtk.FILE_CHOOSER_ACTION_OPEN,
				(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		try:
			self._add_project_file_filters(dialog)
			response = dialog.run()
			if response == gtk.RESPONSE_OK:
				filename = dialog.get_filename()
				if filename[-5:] != ".proj":
					filename = filename + ".proj"
					
				p = self._catch_io_error(project.load_project, filename)		
				if p:
					# TODO: set statusbar
					self.set_project(p)
		finally:
			dialog.destroy()


	def save_project_as(self, widget):
		dialog = gtk.FileChooserDialog("Save net", self.window, gtk.FILE_CHOOSER_ACTION_SAVE,
				(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_SAVE, gtk.RESPONSE_OK))
		try:
			dialog.set_default_response(gtk.RESPONSE_OK)
			self._add_project_file_filters(dialog)
		
			response = dialog.run()
			if response == gtk.RESPONSE_OK:
				filename = dialog.get_filename()
				if filename[-5:] != ".proj":
					filename = filename + ".proj"
			
				if self._catch_io_error(self.project.save, filename, True, False):
					# TODO: set status bar
					pass
		finally:
			dialog.destroy()

	def build_project(self, widget):
		self.project.export("../out/project.xml")
		print "Exported"

	def _catch_io_error(self, fcn, filename, return_on_ok = None, return_on_err = None):
		try:
			result = fcn(filename)	
			if return_on_ok == None:
				return result
			else:
				return return_on_ok
		except IOError, e:
			self.show_error_dialog(str(e))
			return return_on_err

	def _add_file_filters(self, dialog, filters, all_files):
		if all_files:
			filters += (("All files", "*"),)
		for f in filters:
			ffilter = gtk.FileFilter()
			ffilter.set_name(f[0])
			ffilter.add_pattern(f[1])
			dialog.add_filter(ffilter)

	def _add_project_file_filters(self, dialog):
		self._add_file_filters(dialog, (("Projects", "*.proj"),), all_files = True)

	def switch_to_tab(self, widget):
		self.window.switch_to_tab(widget)

	def transition_edit(self, transition):
		if transition in self.tabtable:
			self.switch_to_tab(self.tabtable[transition])
			return

		if transition.get_name() != "":
			name = "T:" + transition.get_name()
		else:
			name = "T: <unnamed" + str(transition.get_id()) + ">"
		editor = TransitionCodeEditor(transition)
		self.add_tab(name, editor, transition)

	def place_edit(self, place):
		if place in self.tabtable:
			self.switch_to_tab(self.tabtable[place])
			return

		name = "P: " + str(place.get_id())
		editor = PlaceCodeEditor(place)
		self.add_tab(name, editor, place)

	def parameters_edit(self):
		if "params" in self.tabtable:
			self.switch_to_tab(self.tabtable["params"])
			return
		w = ParametersWidget(self.builder, self.project)
		self.add_tab("Parameters", w, "params")

	def add_tab(self, name, w, obj):
		""" Open new tab labeled with "name" with content "w" and register this tab for "obj" """
		self.tabtable[obj] = w
		self.window.add_tab("Parameters", w, lambda x: self.close_tab_for_obj(obj))
		self.switch_to_tab(w)
	
	def close_tab_for_obj(self, obj):
		if obj in self.tabtable:
			self.window.close_tab(self.tabtable[obj])
			del self.tabtable[obj]

	def show_error_dialog(self, text):
		error_dlg = gtk.MessageDialog( \
			parent=self.window, \
			type=gtk.MESSAGE_ERROR, \
			message_format=text, \
			buttons=gtk.BUTTONS_OK)
		try:
			error_dlg.run()
		finally:
			error_dlg.destroy()

	def _project_changed(self, project):
		self.nv.net_changed()

app = App()
app.run()
