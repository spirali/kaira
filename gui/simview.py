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

from canvas import NetCanvas, MultiCanvas
from drawing import VisualConfig
import gtkutils
import utils
import mainwindow

class SimViewTab(mainwindow.Tab):
	def __init__(self, app, simulation, tabname = "Simulation"):
		self.simulation = simulation
		simview = SimView(app, simulation)
		mainwindow.Tab.__init__(self, tabname, simview, None)

	def close(self):
		mainwindow.Tab.close(self)
		self.simulation.shutdown()

class SimView(gtk.VBox):
	def __init__(self, app, simulation):
		gtk.VBox.__init__(self)
		self.app = app
		self.simulation = simulation

		self.pack_start(self._buttons(), False, False)
		self.canvas_sc = gtk.ScrolledWindow()
		self.canvas = self._create_canvas(1)
		self.canvas.set_size_and_viewport_by_net()
		self.canvas_sc.add_with_viewport(self.canvas)

		self.instance_canvas_sc = gtk.ScrolledWindow()
		self.instance_canvas = self._create_instances_canvas()
		self.instance_canvas_sc.add_with_viewport(self.instance_canvas)
		self.instance_canvas.show_all()

		self.pack_start(self.canvas_sc)
		self.show_all()
		self.pack_start(self.instance_canvas_sc)

		simulation.set_callback("changed", self._simulation_changed)
		self._visualconfigs()

	def redraw(self):
		self.instance_canvas.redraw()
		self.canvas.redraw()

	def get_net(self):
		return self.simulation.get_net()

	def _buttons(self):
		button1 = gtk.ToggleButton("Instances")
		button1.connect("toggled", self._view_change)

		toolbar = gtk.Toolbar()
		toolbar.add(button1)
		toolbar.show_all()
		return toolbar

	def _view_change(self, button):
		if button.get_active():
			self.instance_canvas_sc.show()
			self.canvas_sc.hide()
		else:
			self.instance_canvas_sc.hide()
			self.canvas_sc.show()

	def _create_canvas(self, zoom):
		c = NetCanvas(self.get_net(), None, VisualConfig(), zoom = 1)
		c.set_callback("button_down", self._button_down)
		c.show()
		return c

	def _create_instances_canvas(self):
		c = MultiCanvas()
		return c

	def tokens_inside(self, place):
		tokens = self.simulation.get_tokens_of_place(place)
		r = []
		for iid in tokens:
			r += [ t + "@" + str(iid) for t in tokens[iid] ]
		if len(r) < 7:
			return

		text_buffer = gtk.TextBuffer()
		text_buffer.insert(text_buffer.get_end_iter(), '\n'.join(r))
		text_area = gtk.TextView()
		text_area.set_buffer(text_buffer)
		text_area.set_editable(False)
		text_area.set_cursor_visible(False)

		sw = gtk.ScrolledWindow()
		sw.add(text_area)
		vbox = gtk.VBox()
		vbox.pack_start(sw)
		vbox.show_all()

		self.app.window.add_tab(mainwindow.Tab("Tokens inside", vbox))

	def _button_down(self, event, position):
		if event.button == 3:
			self._context_menu(event, position)
			return
		net = self.simulation.get_net()
		item = net.get_item_at_position(position, lambda i: not i.is_area())
		if item == None:
			return
		if item.is_transition():
			self.simulation.fire_transition_random_instance(item)
		elif item.is_place():
			self.tokens_inside(item)

	def _context_menu(self, event, position):
		def fire_fn(i):
			return lambda w: self.simulation.fire_transition(t, i)
		t = self.simulation.get_net().get_transition_at_position(position)
		if t:
			iids = self.simulation.enabled_instances_of_transition(t)
			if iids:
				gtkutils.show_context_menu([("Fire " + str(i), fire_fn(i)) for i in iids ], event)

	def _on_instance_click(self, position, area, i):
		net = self.simulation.get_net()
		item = net.get_item_at_position(position, lambda i: not i.is_area())
		if item == None:
			return
		if item.is_transition():
			self.simulation.fire_transition(item, i)
		elif item.is_place():
			self.tokens_inside(item)

	def _instance_draw(self, cr, width, height, vx, vy, vconfig, area, i):
		self.simulation.get_net().draw(cr, vconfig)
		cr.set_source_rgba(0.3,0.3,0.3,0.5)
		cr.rectangle(vx,vy,width, 15)
		cr.fill()
		cr.move_to(vx + 10, vy + 11)
		cr.set_source_rgb(1.0,1.0,1.0)
		cr.show_text("node=%s   iid=%s" % (self.simulation.get_instance_node(area, i), i))
		cr.stroke()

		if not self.simulation.is_instance_running(area, i):
			cr.move_to(vx + 10, vy + 26)
			cr.set_source_rgb(1.0,0.1,0.1)
			cr.show_text("HALTED")
			cr.stroke()

	def _visualconfigs(self):
		def area_callbacks(area, i):
			vconfig = InstanceVisualConfig(self.simulation, area, i)
			draw_fn = lambda cr,w,h,vx,vy: self._instance_draw(cr, w, h, vx, vy, vconfig, area, i)
			click_fn = lambda position: self._on_instance_click(position, area, i)
			return (draw_fn, click_fn)

		for area in self.get_net().areas():
			callbacks = [ area_callbacks(area, i) for i in xrange(self.simulation.get_area_instances_number(area)) ]
			sz, pos = self._view_for_area(area)
			self.instance_canvas.register_line(sz, pos, callbacks)
		self.instance_canvas.end_of_registration()
		self.canvas.set_vconfig(OverviewVisualConfig(self.simulation))

	def _view_for_area(self, area):
		sz = utils.vector_add(area.get_size(), (80, 95))
		pos = utils.vector_diff(area.get_position(), (40, 55))
		return (sz, pos)

	def _simulation_changed(self):
		self.redraw()


class OverviewVisualConfig(VisualConfig):

	def __init__(self, simulation):
		self.simulation = simulation

	def transition_drawing(self, item):
		d = VisualConfig.transition_drawing(self, item)
		if len(self.simulation.enabled_instances_of_transition(item)) > 0:
			d.set_highlight((0.1,0.90,0.1,0.5))
		return d

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		tokens = self.simulation.get_tokens_of_place(item)
		r = []
		for iid in tokens:
			r += [ t + "@" + str(iid) for t in tokens[iid] ]
		d.set_tokens(r)
		return d


class InstanceVisualConfig(VisualConfig):

	def __init__(self, simulation, area, iid):
		self.simulation = simulation
		self.area = area
		self.iid = iid

	def transition_drawing(self, item):
		d = VisualConfig.transition_drawing(self, item)
		if self.simulation.is_transition_enabled(item, self.iid):
			d.set_highlight((0.1,0.90,0.1,0.5))
		return d

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		if self.area.is_inside(item):
			tokens = self.simulation.get_tokens_of_place(item)
			r = tokens[self.iid]
			if len(r) > 6:
				r = r[:6] + ["..."]
			d.set_tokens(r)
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

