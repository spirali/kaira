
import gtk
import gtkutils
import nettools

action_cursor = { 
	"none" : None,
	"move" : gtk.gdk.FLEUR, 
	"resize" : gtk.gdk.BOTTOM_RIGHT_CORNER,
	"select" : gtk.gdk.CROSSHAIR,
}

def get_cursor(name):
	if name is None:
		return None
	stock = action_cursor[name]
	if stock:
		return gtk.gdk.Cursor(stock)
	else:
		return None



class NetView(gtk.VBox):

	def __init__(self, net):
		gtk.VBox.__init__(self)
		self.net = net
		self.tool = None
		self.entry_types = []
		self.set_size_request(500,400)

		self.pack_start(self._controls(), False)
		self.pack_start(self._editarea(), False)
		self.drawarea = self._drawarea()
		self.pack_start(self.drawarea)

		self.transition_edit_callback = None
		self.place_edit_callback = None

	def set_tool(self, tool):
		if self.tool:
			self.tool.stop()
		self.tool = tool
		tool.start()
		self.focus_entry()
	
	def focus_entry(self):
		self.entry.grab_focus()


	def redraw(self):
		self.drawarea.queue_draw()

	def set_cursor(self,action_name):
		self.drawarea.window.set_cursor(get_cursor(action_name))

	def net_changed(self):
		self.redraw()
		if self.tool:
			self.tool.net_changed()

	def _controls(self):
		icon1 = gtk.image_new_from_file("icons/select.png")
		icon2 = gtk.image_new_from_file("icons/area.png")
		icon3 = gtk.image_new_from_file("icons/place.png")
		icon4 = gtk.image_new_from_file("icons/transition.png")
		icon5 = gtk.image_new_from_file("icons/arc.png")
		#hbox = gtk.HBox()
		#hbox.pack_start(gtk.Button("test"), False)
		#hbox.show_all()
		button1 = gtk.RadioToolButton(None,None)
		#button1.connect("toggled", lambda w: view.set_tool(tools.SelectTool(self)))
		button1.set_icon_widget(icon1)

		button2 = gtk.RadioToolButton(button1,None)
		button2.connect("toggled", lambda w: self.set_tool(nettools.AreaTool(self)))
		button2.set_icon_widget(icon2)

		button3 = gtk.RadioToolButton(button1,None)
		button3.connect("toggled", lambda w: self.set_tool(nettools.PlaceTool(self)))
		button3.set_icon_widget(icon3)

		button4 = gtk.RadioToolButton(button1,None)
		button4.connect("toggled", lambda w: self.set_tool(nettools.TransitionTool(self)))
		button4.set_icon_widget(icon4)

		button5 = gtk.RadioToolButton(button1,None)
		button5.connect("toggled", lambda w: self.set_tool(nettools.EdgeTool(self)))
		button5.set_icon_widget(icon5)

		toolbar = gtk.Toolbar()
		toolbar.insert(button1, 0)
		toolbar.insert(button2, 1)
		toolbar.insert(button3, 2)
		toolbar.insert(button4, 3)
		toolbar.insert(button5, 4)

		vbox = gtk.VBox()
		vbox.pack_start(toolbar)
		vbox.show_all()

		return vbox

	def _drawarea(self):
		area = gtk.DrawingArea()
		area.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK | gtk.gdk.POINTER_MOTION_MASK)
		area.connect("expose_event", self._expose_area)
		area.connect("button_press_event", self._button_down)
		area.connect("button_release_event", self._button_up)
		area.connect("motion_notify_event", self._mouse_move)
		area.show()
		return area


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

	def _expose_area(self, w, event):
		cr = self.drawarea.window.cairo_create()
		cr.rectangle(event.area.x, event.area.y,
				event.area.width, event.area.height)
		cr.clip()
		self._draw(cr, *self.drawarea.window.get_size())

	def _draw(self, cr, width, height):
		cr.set_source_rgb(0.8, 0.8, 0.8)
		cr.rectangle(0, 0, width, height)
		cr.fill()
		self.net.draw(cr)
		if self.tool:
			self.tool.draw(cr)

	def _button_down(self, w, event):
		if self.tool:
			position = (event.x, event.y)
			if event.button == 1:
				self.tool.button_down(position)
			elif event.button == 3:
				self.tool.right_button(event, position)

	def _button_up(self, w, event):
		position = (event.x, event.y)
		if self.tool:
			self.tool.button_up(position)

	def _mouse_move(self, w, event):
		position = (event.x, event.y)
		if self.tool:
			self.tool.mouse_move(position)

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

	def show_context_menu(self, event, menu_actions):
		menu = gtkutils.build_menu(menu_actions)
		menu.show_all()
		menu.popup(None, None, None, event.button, event.get_time())


	def _entry_changed(self, w):
		if self.entry_types and self.entry_switch.get_active_text():
			name, get, set = self.active_entry_type()
			set(self.entry.get_text())

	def _entry_switch_changed(self, w):
		if self.entry_types and self.entry_switch.get_active_text():
			name, get, set = self.active_entry_type()
			self.entry.set_text(get())
