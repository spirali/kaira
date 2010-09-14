import gtk

import project
from netcanvas import NetCanvas

class SimView(gtk.VBox):
	def __init__(self, app, simulation):
		gtk.VBox.__init__(self)
		self.simulation = simulation
		self.canvas = self._create_canvas()
		self.pack_start(self.canvas)

		simulation.set_callback("changed", self.redraw)
		simulation.set_callback("output", lambda line: app.console_write(line, "output"))

	def redraw(self):
		self.canvas.redraw()

	def _create_canvas(self):
		c = NetCanvas(self.simulation.get_net(), None)
		c.connect("button_press_event", self._button_down)
		c.connect("button_release_event", self._button_up)
		c.connect("motion_notify_event", self._mouse_move)
		c.show()
		return c

	def _button_down(self, w, event):
		position = (event.x, event.y)
		net = self.simulation.get_net()
		t = net.get_transition(position)
		if t:
			self.simulation.fire_transition_random_instance(t)

	def _button_up(self, w, event):
		pass

	def _mouse_move(self, w, event):
		pass
