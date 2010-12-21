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
import gtkutils
import nettools
import paths
import os
from canvas import NetCanvas
from drawing import VisualConfig

action_cursor = { 
	"none" : None,
	"move" : gtk.gdk.FLEUR, 
	"resize" : gtk.gdk.BOTTOM_RIGHT_CORNER,
	"select" : gtk.gdk.CROSSHAIR,
	"scroll" : gtk.gdk.HAND2,
}

def get_cursor(name):
	if name is None:
		return None
	stock = action_cursor[name]
	if stock:
		return gtk.gdk.Cursor(stock)
	else:
		return None

class NetViewVisualConfig(VisualConfig):

	def __init__(self, project):
		self.selected = None
		self.project = project

	def highlight(self, item):
		self.selected = item

	def highlight_off(self):
		self.selected = None

	def drawing_init(self, i, drawing):
		if self.project.has_error_messages(i):
			drawing.set_highlight((1.0, 0.0, 0.0, 0.5))
			drawing.set_error_messages(self.project.get_error_messages(i))
		if i == self.selected:
			drawing.set_highlight((0.86,0.86,0.0,1.0))

	def transition_drawing(self, item):
		d = VisualConfig.transition_drawing(self, item)
		self.drawing_init(item, d)
		return d

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		self.drawing_init(item, d)
		return d

	def edge_drawing(self, item):
		d = VisualConfig.edge_drawing(self, item)
		self.drawing_init(item, d)
		return d

	def area_drawing(self, item):
		d = VisualConfig.area_drawing(self, item)
		self.drawing_init(item, d)
		return d

class NetView(gtk.VBox):

	def __init__(self, project, net):
		gtk.VBox.__init__(self)
		self.project = project
		self.net = net
		self.tool = None
		self.entry_types = []
		self.set_size_request(500,400)

		self.pack_start(self._controls(), False)
		self.pack_start(self._editarea(), False)
		self.drawarea = self._net_canvas()
		self.pack_start(self.drawarea)

		self.transition_edit_callback = None
		self.place_edit_callback = None
		self.set_tool(nettools.TransitionTool(self))

	def set_tool(self, tool):
		if self.tool:
			self.tool.stop()
		self.tool = tool
		tool.start()
		self.focus_entry()
	
	def focus_entry(self):
		self.entry.grab_focus()

	def set_viewport(self, viewport):
		self.drawarea.set_viewport(viewport)

	def get_viewport(self):
		return self.drawarea.get_viewport()

	def redraw(self):
		self.drawarea.redraw()

	def set_cursor(self,action_name):
		if self.drawarea.window:
			self.drawarea.window.set_cursor(get_cursor(action_name))

	def net_changed(self):
		self.redraw()
		if self.tool:
			self.tool.net_changed()

	def highlight(self, item):
		self.vconfig.highlight(item)
		self.redraw()

	def highlight_off(self):
		self.vconfig.highlight_off()
		self.redraw()

	def _controls(self):
		icon_transition = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "transition.png"))
		icon_place = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "place.png"))
		icon_arc = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "arc.png"))
		icon_area = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "area.png"))
 
		button1 = gtk.RadioToolButton(None,None)
		button1.connect("toggled", lambda w: self.set_tool(nettools.TransitionTool(self)))
		button1.set_icon_widget(icon_transition)

		button2 = gtk.RadioToolButton(button1,None)
		button2.connect("toggled", lambda w: self.set_tool(nettools.PlaceTool(self)))
		button2.set_icon_widget(icon_place)

		button3 = gtk.RadioToolButton(button1,None)
		button3.connect("toggled", lambda w: self.set_tool(nettools.EdgeTool(self)))
		button3.set_icon_widget(icon_arc)

		button4 = gtk.RadioToolButton(button1,None)
		button4.connect("toggled", lambda w: self.set_tool(nettools.AreaTool(self)))
		button4.set_icon_widget(icon_area)

		toolbar = gtk.Toolbar()
		toolbar.add(button1)
		toolbar.add(button2)
		toolbar.add(button3)
		toolbar.add(button4)

		vbox = gtk.VBox()
		vbox.pack_start(toolbar)
		vbox.show_all()

		return vbox

	def _net_canvas(self):
		self.vconfig = NetViewVisualConfig(self.project)
		c = NetCanvas(self.net, self._draw, self.vconfig)
		c.set_callback("button_down", self._button_down)
		c.set_callback("button_up", self._button_up)
		c.set_callback("mouse_move", self._mouse_move)
		c.show()
		return c

	def _draw(self, cr, w, h):
		if self.tool:
			self.tool.draw(cr)

	def _editarea(self):
		vbox = gtk.VBox()
		self.entry = gtk.Entry()
		self.entry.connect("changed", self._entry_changed)

		self.entry_switch = gtk.combo_box_new_text()
		self.entry_switch.append_text("Inscription")
		self.entry_switch.connect("changed", self._entry_switch_changed)

		vbox = gtk.HBox()
		vbox.pack_start(self.entry_switch, False, False)
		vbox.pack_start(self.entry, True, True)

		vbox.show_all()
		return vbox


	def _button_down(self, event, position):
		if self.tool:
			if event.button == 1:
				self.tool.left_button_down(event, position)
			elif event.button == 3:
				self.tool.right_button_down(event, position)

	def _button_up(self, event, position):
		if self.tool:
			if event.button == 1:
				self.tool.left_button_up(event, position)
			elif event.button == 3:
				self.tool.right_button_up(event, position)

	def _mouse_move(self, event, position):
		if self.tool:
			self.tool.mouse_move(event, position)

	def set_entry_types(self, etypes):
		self.entry_types = etypes
		names = [ x[0] for x in etypes ]
		self.entry_switch.get_model().clear()
		for name in names:
			self.entry_switch.append_text(name)
		self.entry.set_sensitive(bool(names))
		
		if names:
			self.entry_switch.set_active(0)
			name, get, set = self.active_entry_type()
			self.entry.set_text(get())
			self.focus_entry()

	def active_entry_type(self):
		text = self.entry_switch.get_active_text()
		for e in self.entry_types:
			if e[0] == text:
				return e

	def _entry_changed(self, w):
		if self.entry_types and self.entry_switch.get_active_text():
			name, get, set = self.active_entry_type()
			set(self.entry.get_text())

	def _entry_switch_changed(self, w):
		if self.entry_types and self.entry_switch.get_active_text():
			name, get, set = self.active_entry_type()
			self.entry.set_text(get())
