import gtk

import project
from netcanvas import NetCanvas

class SimView(gtk.VBox):
	def __init__(self, app, simulation):
		gtk.VBox.__init__(self)
		self.simulation = simulation
		self.canvas = NetCanvas(simulation.get_net(), None)
		self.canvas.show()
		self.pack_start(self.canvas)

		simulation.set_callback("changed", self.redraw)
		simulation.set_callback("output", lambda line: app.console_write(line))

	def redraw(self):
		self.canvas.redraw()

	def _button_down(self, w, event):
		pass

	def _button_up(self, w, event):
		pass

	def _mouse_move(self, w, event):
		pass
