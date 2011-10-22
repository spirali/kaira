#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                  2011       Ondrej Garncarz
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
from canvas import NetCanvas
from drawing import VisualConfig
import gtkutils
import mainwindow
import objectlist

class SimViewTab(mainwindow.Tab):
	def __init__(self, app, simulation, tabname = "Simulation"):
		self.simulation = simulation
		simview = SimView(app, simulation)
		mainwindow.Tab.__init__(self, tabname, simview, None)

	def close(self):
		mainwindow.Tab.close(self)
		self.simulation.shutdown()

class NetRunView(gtk.HBox):
	def __init__(self, instances, vconfig):
		gtk.HBox.__init__(self)
		vbox = gtk.VBox()
		vbox.pack_start(self._tree(instances[0].get_perspectives()))
		vbox.pack_start(self._instances_list(instances))
		self.pack_start(vbox, False, False)
		self.canvas_sc = gtk.ScrolledWindow()
		self.canvas = self._create_canvas(vconfig)
		self.canvas.set_size_and_viewport_by_net()
		self.canvas_sc.add_with_viewport(self.canvas)

		self.pack_start(self.canvas_sc)
		self.show_all()

	def redraw(self):
		self.canvas.redraw()

	def get_perspective(self):
		return self.tree.get_selection(0)

	def get_instance(self):
		return self.instances.selected_object()

	def _create_canvas(self, vconfig):
		c = NetCanvas(self.get_instance().net, None, vconfig, zoom = 1)
		c.set_callback("button_down", self._button_down)
		c.show()
		return c

	def _refresh_tree(self, perspectives):
		p = self.get_perspective()
		self.tree.clear()
		for pe in perspectives:
			i = self.tree.append((pe, str(pe.get_name())))
			if p == pe:
				self.tree.select_iter(i)
		if p not in perspectives:
			self.tree.select_first()

	def _refresh_instances(self, instances):
		selected_instance = self.get_instance()
		self.instances.clear()
		selected = False
		for instance in instances:
			self.instances.add_object(instance)
			if selected_instance and instance.id == selected_instance.id:
				self.instances.select_object(instance)
				selected = True
		if not selected:
			self.instances.select_first()
		self._refresh_tree(self.get_instance().get_perspectives())


	def _tree(self, perspectives):
		self.tree = gtkutils.SimpleList((("_", object), ("Views",str)))
		self.tree.set_size_request(80,10)
		self._refresh_tree(perspectives)
		self.tree.select_first()
		self.tree.connect_view("cursor-changed", self._path_changed);
		return self.tree

	def _instances_list(self, instances):
		self.instances = objectlist.ObjectList((("_", object), ("Instances",str)))
		self.instances.cursor_changed = lambda s, obj: self.redraw()
		self.instances.object_as_row = lambda obj: (obj, obj.get_name())
		self.instances.fill(instances)
		self.instances.select_first()
		return self.instances

	def _path_changed(self, w):
		self.redraw()

	def _button_down(self, event, pos):
		pass

class SimView(NetRunView):

	def __init__(self, app, simulation):
		NetRunView.__init__(self, simulation.instances, SimVisualConfig(self))
		self.simulation = simulation
		self.app = app
		simulation.set_callback("changed", self._simulation_changed)

	def _simulation_changed(self):
		self._refresh_instances(self.simulation.instances)
		self.redraw()

	def _button_down(self, event, pos):
		item = self.get_instance().net.get_item_at_position(pos)
		if item is None:
			return
		if item.is_transition():
			self.get_perspective().fire_transition(item)
		elif item.is_place():
			self.open_tokens_tab(item)

	def open_tokens_tab(self, place):
		text_buffer = gtk.TextBuffer()
		tokens = '\n'.join(map(str, self.get_perspective().get_tokens(place.get_id())))
		text_buffer.insert(text_buffer.get_end_iter(), tokens)
		text_area = gtk.TextView()
		text_area.set_buffer(text_buffer)
		text_area.set_editable(False)

		sw = gtk.ScrolledWindow()
		sw.add(text_area)
		vbox = gtk.VBox()
		vbox.pack_start(sw)
		vbox.show_all()

		label = "Tokens of " + place.get_name()
		self.app.window.add_tab(mainwindow.Tab(label, vbox))


class SimVisualConfig(VisualConfig):

	def __init__(self, simview):
		self.simview = simview

	def transition_drawing(self, item):
		d = VisualConfig.transition_drawing(self, item)
		if self.simview.simulation.running and self.simview.get_perspective().is_enabled(item):
			d.set_highlight((0.1,0.90,0.1,0.5))
		return d

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		tokens = map(str, self.simview.get_perspective().get_tokens(item))
		d.set_tokens(tokens)
		return d

def connect_dialog(mainwindow):
	builder = gtkutils.load_ui("connect-dialog")
	dlg = builder.get_object("connect-dialog")
	try:

		host = builder.get_object("host")
		port = builder.get_object("port")
		port.set_value(10000)

		dlg.set_title("Connect")
		dlg.set_transient_for(mainwindow)
		if dlg.run() == gtk.RESPONSE_OK:
			return (host.get_text(), int(port.get_value()))
		return None
	finally:
		dlg.destroy()

