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

import project
import os
import sys
import subprocess
import gtkutils
import paths
from mainwindow import MainWindow
from netview import NetView
from simview import SimView
from codeedit import TransitionCodeEditor
from codeedit import PlaceCodeEditor
from codeedit import CodeFileEditor
from parameters import ParametersValueDialog
from externtypes import ExternTypeEditor
from projectconfig import ProjectConfig
from simulation import Simulation, SimulationException
from functions import FunctionEditor
from drawing import VisualConfig
import process
import utils
import cairo

class App:
	
	def __init__(self, args):
		self.window = MainWindow(self)
		self.window.project_is_active(False)
		self.nv = None
		self.tabtable = {}
		self._open_welcome_tab()
		self.grid_size = 1

		if args:
			if os.path.isfile(args[0]):
				self.set_project(project.load_project(args[0]))
			else:
				self.console_write("Project file '%s' not found\n" % args[0], "error")

	def run(self):
		gtk.gdk.threads_init()
		self.window.show()
		gtk.main()

	def set_project(self, project):
		self.project = project
		self.project.set_callback("changed", self._project_changed)
		self.project.set_callback("filename_changed", self._project_filename_changed)
		self.project.write_project_files()
		self.init_tabs()
		self.window.console.reset()
		self._project_changed()
		self._project_filename_changed()
		self.window.project_is_active(True)

	def init_tabs(self):
		if self.nv:
			self.window.close_tab(self.nv)
		for t in self.tabtable:
			widget, callback = self.tabtable[t]
			self.window.close_tab(widget)
			if callback:
				callback(t)
		self.window.close_all_tabs()
		self.nv = NetView(self, self.project, self.project.net)
		self.nv.transition_edit_callback = self.transition_edit
		self.nv.place_edit_callback = self.place_edit
		self.window.add_tab("Network", self.nv)
		self.tabtable = {}

	def new_project(self):
		def project_name_changed(w = None):
			name = builder.get_object("newproject-name").get_text().strip()
			builder.get_object("newproject-dir").set_text(os.path.join(directory[0], name))
			builder.get_object("newproject-ok").set_sensitive(name != "")
		def change_directory(w):
			d = self._directory_choose_dialog("Select project directory")
			if d is not None:
				directory[0] = d
				project_name_changed()
		builder = gtkutils.load_ui("newproject-dialog")
		dlg = builder.get_object("newproject-dialog")
		dlg.set_transient_for(self.window)
		builder.get_object("newproject-name").connect("changed", project_name_changed)
		directory = [os.getcwd()]
		project_name_changed()
		builder.get_object("newproject-dirbutton").connect("clicked", change_directory)
		try:
			if dlg.run() == gtk.RESPONSE_OK:
				dirname = builder.get_object("newproject-dir").get_text()
				if os.path.exists(dirname):
					self.show_error_dialog("Path '%s' already exists" % dirname)
					return
				p = self._catch_io_error(lambda: project.new_empty_project(dirname))
				if p is not None:
					self.set_project(p)
		finally:
			dlg.hide()

	def load_project(self):
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
					
				p = self._catch_io_error(lambda: project.load_project(filename))
				if p:
					# TODO: set statusbar
					self.set_project(p)
		finally:
			dialog.destroy()

	def save_project(self):
		if self.project.get_filename() is None:
			self.save_project_as()
		else:
			if self._catch_io_error(self.project.save, True, False):
				self.console_write("Project saved as '%s'\n" % self.project.get_filename(), "success")

	def save_project_as(self):
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
				self.project.set_filename(filename)
				if self._catch_io_error(self.project.save, True, False):
					self.console_write("Project saved as '%s'\n" % self.project.get_filename(), "success")
		finally:
			dialog.destroy()

	def build_project(self):
		self._start_build(self.project, lambda p: self.console_write("Build OK\n", "success"))

	def get_grid_size(self):
		return self.grid_size

	def set_grid_size(self, grid_size):
		self.grid_size = grid_size

	def _catch_io_error(self, fcn, return_on_ok = None, return_on_err = None):
		try:
			result = fcn()	
			if return_on_ok == None:
				return result
			else:
				return return_on_ok
		except IOError as e:
			self.show_error_dialog(str(e))
			return return_on_err
		except OSError as e:
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

	def switch_to_tab(self, tabtag):
		w, callback = self.tabtable[tabtag]
		self.window.switch_to_tab(w)

	def transition_edit(self, transition):
		if transition in self.tabtable:
			self.switch_to_tab(transition)
			return

		def open_tab(stdout):
			if transition.get_name() != "":
				name = "T:" + transition.get_name()
			else:
				name = "T: <unnamed" + str(transition.get_id()) + ">"
			editor = TransitionCodeEditor(transition, [ line for line in stdout if line.strip() != "" ])
			self.add_tab(name, editor, transition)
		self._start_ptp(self.project, open_tab, extra_args = [ "--transition-vars", str(transition.get_id()) ])

	def place_edit(self, place):
		if place in self.tabtable:
			self.switch_to_tab(place)
			return

		def open_tab(stdout):
			name = "P: " + str(place.get_id())
			editor = PlaceCodeEditor(place, stdout[0].strip())
			self.add_tab(name, editor, place)
		self._start_ptp(self.project, open_tab, extra_args = [ "--place-type", str(place.get_id())])

	def extern_type_function_edit(self, extern_type, fn_name, callback):
		tag = (extern_type, fn_name)
		if tag in self.tabtable:
			self.switch_to_tab(tag)
			return
		name = extern_type.get_name() + "/" + fn_name
		editor = ExternTypeEditor(extern_type, fn_name, callback)
		self.add_tab(name, editor, tag)

	def function_edit(self, function):
		if function in self.tabtable:
			self.switch_to_tab(function)
			return
		editor = FunctionEditor(function)
		self.add_tab(function.get_name(), editor, function)

	def project_config(self):
		if "project-config" in self.tabtable:
			self.switch_to_tab("project-config")
			return
		w = ProjectConfig(self)
		self.add_tab("Project", w, "project-config")

	def edit_sourcefile(self, filename):
		tab_tag = "file:" + filename
		if tab_tag in self.tabtable:
			self.switch_to_tab(tab_tag)
			return
		w = CodeFileEditor(app, filename)
		self.add_tab(os.path.basename(filename), w, tab_tag, lambda x: w.shutdown())

	def edit_headfile(self):
		self.edit_sourcefile(self.project.get_head_filename())

	def simulation_start(self, try_reuse_params):
		def project_builded(project):
			try:
				simulation = Simulation(project, param_values)
				w = SimView(self, simulation)
				self.add_tab("Simulation", w, simulation, lambda s: simulation.shutdown())
			except SimulationException as e:
				self.console_write(str(e), "error")

		project, idtable = self.project.copy()
		transtable = utils.inverse_dict(idtable) # Table new_id -> old_id

		if project.get_parameters(): # Project has parameters
			cache = self.project.get_param_value_cache()
			if try_reuse_params and cache is not None:
				param_values = cache
			else:
				dialog = ParametersValueDialog(self.window, project.get_parameters())
				try:
					if dialog.run() == gtk.RESPONSE_OK:
						param_values = dialog.get_values()
						self.project.set_param_values_cache(param_values)
					else:
						return
				finally:
					dialog.destroy()
		else:
			param_values = {}
		self._start_build(project, project_builded, translation_table = transtable)

	def add_tab(self, name, w, obj, callback = None):
		""" Open new tab labeled with "name" with content "w" and register this tab for "obj" """
		self.tabtable[obj] = (w, callback)
		self.window.add_tab(name, w, lambda x: self.close_tab_for_obj(obj))
		self.switch_to_tab(obj)
	
	def close_tab_for_obj(self, obj):
		if obj in self.tabtable:
			widget, callback = self.tabtable[obj]
			self.window.close_tab(widget)
			del self.tabtable[obj]
			if callback:
				callback(obj)

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

	def console_write(self, text, tag_name = "normal"):
		self.window.console.write(text, tag_name)

	def console_write_link(self, text, callback):
		self.window.console.write_link(text, callback)

	def export_project(self, proj = None):
		if proj is None:
			proj = self.project
		try:
			proj.export(proj.get_exported_filename())
		except project.ExportException as e:
			self.console_write(str(e) + "\n", "error")
			return False
		return True

	def hide_error_messages(self):
		self.project.set_error_messages({})

	def export_to_svg(self):
		surface = cairo.SVGSurface("network.svg", 1000, 1000)
		try:
			context = cairo.Context(surface)
			self.project.net.draw(context, VisualConfig())
		finally:
			surface.finish()
		self.console_write("Network exported to 'network.svg'.", "success")

	def _project_changed(self):
		self.nv.net_changed()

	def _project_filename_changed(self):
		self.window.set_title("Kaira (" + self.project.get_name() + ")")

	def _run_makefile(self, project, translation_table, build_ok_callback = None, target = None):
		def on_exit(code):
			if build_ok_callback and code == 0:
				build_ok_callback(project)
		def on_line(line):
			self._process_error_line(line, None, translation_table)
			return True
		p = process.Process("make",on_line, on_exit)
		p.cwd = project.get_directory()
		if target is None:
			p.start()
		else:
			p.start([target])

	def _start_build(self, proj, build_ok_callback, translation_table = None):
		extra_args = [ proj.get_emitted_source_filename() ]
		self._start_ptp(proj, lambda lines: self._run_makefile(proj, translation_table, build_ok_callback), 
			translation_table, extra_args = extra_args)

	def _start_ptp(self, proj, build_ok_callback = None, translation_table = None, extra_args = []):
		stdout = []
		def on_exit(code):
			error_messages = {}
			if build_ok_callback and code == 0:
				self.project.set_error_messages(error_messages)
				build_ok_callback(stdout)
			else:
				for line in stdout:
					self._process_error_line(line, error_messages, translation_table)
				self.project.set_error_messages(error_messages)
				self.console_write("Building failed\n", "error")
		def on_line(line):
			stdout.append(line)
			return True
		if not self.export_project(proj):
			return
		p = process.Process(paths.PTP_BIN, on_line, on_exit)
		p.cwd = proj.get_directory()
		p.start([proj.get_exported_filename()] + extra_args)

	def _try_make_error_with_link(self, item_id, pos, message):
		item = self.project.get_item(item_id)
		if pos == "function" and item.is_transition():
			self.console_write_link(str(item_id) + "/" + pos, lambda: self.transition_edit(item))
			self.console_write(":" + message)
			return True
		if pos == "init_function" and item.is_place():
			self.console_write_link(str(item_id) + "/" + pos, lambda: self.place_edit(item))
			self.console_write(":" + message)
			return True
		return False

	def _process_error_line(self, line, error_messages, translation_table):
		if line.startswith("*"):
			sections = line[1:].split(":",1)
			item_id, pos = sections[0].split("/")
			item_id = int(item_id)
			if translation_table:
				item_id = translation_table[item_id]
			if self._try_make_error_with_link(item_id, pos, sections[1]):
				return
			if error_messages is None:
				self.console_write(line)
			else:
				d = error_messages.setdefault(item_id, {})
				lines = d.setdefault(pos, [])
				lines.append(sections[1].strip())
		else:
			self.console_write(line)

	def _directory_choose_dialog(self, title):
		dialog = gtk.FileChooserDialog(title, self.window, gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
				(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
		dialog.set_default_response(gtk.RESPONSE_OK)
		try:
			if dialog.run() == gtk.RESPONSE_OK:
				return dialog.get_filename()
			else:
				return None
		finally:
			dialog.destroy()

	def _open_welcome_tab(self):
		label = gtk.Label()
		line1 = "<span size='xx-large'>Kaira</span>\nv0.1\n\n"
		line2 = "News &amp; documentation can be found at\n"
		line3 = "<a href='http://verif.cs.vsb.cz/kaira'>http://verif.cs.vsb.cz/kaira</a>"
		label.set_markup(line1 + line2 + line3)
		label.set_justify(gtk.JUSTIFY_CENTER)
		label.set_size_request(400,300)
		self.window.add_tab("Welcome", label)

if __name__ == "__main__":
	args = sys.argv[1:] # Remove "app.py"
	app = App(args)
	app.run()
