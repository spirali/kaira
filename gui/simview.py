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

from canvas import NetCanvas
from drawing import VisualConfig
import gtkutils
import mainwindow

class SimViewTab(mainwindow.Tab):
	def __init__(self, app, simulation, tabname = "Simulation"):
		self.simulation = simulation
		simview = SimView(app, simulation)
		mainwindow.Tab.__init__(self, tabname, simview, None)

	def close(self):
		mainwindow.Tab.close(self)
		self.simulation.shutdown()

class SimView(gtk.HBox):
	def __init__(self, app, simulation):
		gtk.HBox.__init__(self)
		self.simulation = simulation

		self.pack_start(self._panel(), False, False)
		self.canvas_sc = gtk.ScrolledWindow()
		self.canvas = self._create_canvas()
		self.canvas.set_size_and_viewport_by_net()
		self.canvas_sc.add_with_viewport(self.canvas)

		self.pack_start(self.canvas_sc)
		self.show_all()

		simulation.set_callback("changed", self._simulation_changed)

	def redraw(self):
		self.canvas.redraw()

	def get_net(self):
		return self.simulation.get_net()

	def get_instance(self):
		path = self.tree.get_selection(0)
		if path is None:
			return self.simulation.get_overview()
		else:
			return self.simulation.get_instance(path)

	def _create_canvas(self):
		c = NetCanvas(self.get_net(), None, SimVisualConfig(self), zoom = 1)
		c.set_callback("button_down", self._button_down)
		c.show()
		return c

	def _refresh_tree(self):
		selected_path = self.tree.get_selection(0)
		self.tree.clear()
		self.tree.append((None, "Overview"))
		if selected_path is None:
			self.tree.select_first()
		paths = self.simulation.running_paths()
		paths.sort()
		for path in paths:
			i = self.tree.append((path, str(path)))
			if path == selected_path:
				self.tree.select_iter(i)

	def _panel(self):
		self.tree = gtkutils.SimpleList((("_", object), ("Path",str)))
		self.tree.set_size_request(80,10)
		self._refresh_tree()
		self.tree.select_first()
		self.tree.connect_view("cursor-changed", self._path_changed);
		return self.tree

	def _path_changed(self, w):
		self.redraw()

	def _simulation_changed(self):
		self._refresh_tree()
		self.redraw()

	def _button_down(self, event, pos):
		item = self.simulation.get_net().get_item_at_position(pos, lambda i: i.is_transition())
		if item:
			self.get_instance().fire_transition(item)


class SimVisualConfig(VisualConfig):

	def __init__(self, simview):
		self.simview = simview

	def transition_drawing(self, item):
		d = VisualConfig.transition_drawing(self, item)
		running = self.simview.simulation.network_running
		if running and self.simview.get_instance().is_enabled(item):
			d.set_highlight((0.1,0.90,0.1,0.5))
		return d

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		tokens = self.simview.get_instance().get_tokens(item)
		if tokens:
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

