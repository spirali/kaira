
import gtk

class NetCanvas(gtk.DrawingArea):
	"""
		Events: button_down, button_up, mouse_move, expose 
	"""

	def __init__(self, net, draw_cb):
		gtk.DrawingArea.__init__(self);
		self.net = net
		self.draw_cb = draw_cb
		self.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK | gtk.gdk.POINTER_MOTION_MASK)
		self.connect("expose_event", self._expose_area)

	def redraw(self):
		self.queue_draw()

	def _expose_area(self, w, event):
		cr = self.window.cairo_create()
		cr.rectangle(event.area.x, event.area.y,
				event.area.width, event.area.height)
		cr.clip()
		self._draw(cr, *self.window.get_size())

	def _draw(self, cr, width, height):
		cr.set_source_rgb(0.8, 0.8, 0.8)
		cr.rectangle(0, 0, width, height)
		cr.fill()
		self.net.draw(cr)
		if self.draw_cb:
			self.draw_cb(cr, width, height)
