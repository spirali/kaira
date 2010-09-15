
import gtk

class NetCanvas(gtk.DrawingArea):

	def __init__(self, net, draw_cb, vconfig):
		gtk.DrawingArea.__init__(self);
		self.net = net
		self.vconfig = vconfig
		self.draw_cb = draw_cb
		self.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK | gtk.gdk.POINTER_MOTION_MASK)
		self.connect("expose_event", self._expose)

	def set_vconfig(self, vconfig):
		self.vconfig = vconfig
		self.redraw()

	def redraw(self):
		self.queue_draw()

	def _expose(self, w, event):
		cr = self.window.cairo_create()
		cr.rectangle(event.area.x, event.area.y,
				event.area.width, event.area.height)
		cr.clip()
		self._draw(cr, *self.window.get_size())

	def _draw(self, cr, width, height):
		cr.set_source_rgb(0.8, 0.8, 0.8)
		cr.rectangle(0, 0, width, height)
		cr.fill()
		self.net.draw(cr, self.vconfig)
		if self.draw_cb:
			self.draw_cb(cr, width, height)

class MultiCanvas(gtk.DrawingArea):

	def __init__(self):
		gtk.DrawingArea.__init__(self)
		self.lines = []
		self.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK | gtk.gdk.POINTER_MOTION_MASK)
		self.connect("expose_event", self._expose)

	def register_line(self, view_size, view_pos, draw_callbacks):
		self.lines.append( (view_size, view_pos, draw_callbacks) )

	def end_of_registration(self):
		if self.lines:
			self.size_x = max([ s[0] * len(cbs) for s,v,cbs in self.lines ])
			self.size_y = sum([ s[1] for s,v,cbs in self.lines ])
		else:
			self.size_x = 0
			self.size_y = 0
		self.set_size_request(self.size_x, self.size_y)

	def _expose(self, w, event):
		cr = self.window.cairo_create()
		cr.rectangle(event.area.x, event.area.y,
				event.area.width, event.area.height)
		cr.clip()
		self._draw(cr, *self.window.get_size())

	def _draw(self, cr, width, height):
		cr.set_source_rgb(0.8, 0.8, 0.8)
		cr.rectangle(0, 0, width, height)
		cr.fill()
		if not self.lines:
			return
		self._draw_grid(cr)

		y = 0
		for (sx,sy), (vx, vy), callbacks in self.lines:
			for i,callback in enumerate(callbacks):
				cr.save()
				cr.rectangle(sx * i, y, sx, sy)
				cr.clip()
				cr.translate(sx * i - vx, y - vy)
				callback(cr)
				cr.restore()
			y += sy

	def _draw_grid(self,cr):
		y = 0
		cr.set_line_width(1.5)
		cr.set_source_rgb(0.1,0.1,0.1)

		for (sx,sy), view_pos, callbacks in self.lines:
			y2 = y + sy
			cr.move_to(0, y2)
			cr.line_to(self.size_x, y2)
			cr.stroke()
			for i in xrange(1, len(callbacks) + 1):
				cr.move_to(i * sx, y)
				cr.line_to(i * sx, y2)
				cr.stroke()
			y = y2
