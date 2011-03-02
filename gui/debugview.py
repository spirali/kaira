
import gtk
from canvas import NetCanvas, MultiCanvas
from drawing import VisualConfig
import utils

class DebugView(gtk.VBox):
	def __init__(self, app, debuglog):
		gtk.VBox.__init__(self)
		self.debuglog = debuglog

		self.pack_start(self._buttons(), False, False)
		self.canvas_sc = gtk.ScrolledWindow()
		self.canvas = self._create_canvas()
		self.canvas.set_size_and_viewport_by_net()
		self.canvas_sc.add_with_viewport(self.canvas)

		self.instance_canvas_sc = gtk.ScrolledWindow()
		self.instance_canvas = self._create_instances_canvas()
		self.instance_canvas_sc.add_with_viewport(self.instance_canvas)
		self.instance_canvas.show_all()

		self.pack_start(self.canvas_sc)
		self.show_all()
		self.pack_start(self.instance_canvas_sc)

	def _buttons(self):
		button1 = gtk.ToggleButton("Instances")
		button1.connect("toggled", self._view_change)

		toolbar = gtk.Toolbar()
		toolbar.add(button1)
		toolbar.show_all()
		return toolbar

	def _create_canvas(self):
		c = NetCanvas(self.debuglog.project.get_net(), None, OverviewVisualConfig(self.debuglog))
		c.show()
		return c

	def _instance_draw(self, cr, width, height, vx, vy, vconfig, area, i):
		self.debuglog.project.get_net().draw(cr, vconfig)
		cr.set_source_rgba(0.3,0.3,0.3,0.5)
		cr.rectangle(vx,vy,width, 15)
		cr.fill()
		cr.move_to(vx + 10, vy + 11)
		cr.set_source_rgb(1.0,1.0,1.0)
		cr.show_text("node=%s   iid=%s" % (self.debuglog.get_instance_node(area, i), i))
		cr.stroke()

	def _view_for_area(self, area):
		sz = utils.vector_add(area.get_size(), (80, 95))
		pos = utils.vector_diff(area.get_position(), (40, 55))
		return (sz, pos)

	def _create_instances_canvas(self):
		def area_callbacks(area, i):
			vconfig = InstanceVisualConfig(self.debuglog, area, i)
			draw_fn = lambda cr,w,h,vx,vy: self._instance_draw(cr, w, h, vx, vy, vconfig, area, i)
			click_fn = lambda position: self._on_instance_click(position, area, i)
			return (draw_fn, click_fn)
		c = MultiCanvas()
		for area in self.debuglog.project.get_net().areas():
			callbacks = [ area_callbacks(area, i) for i in xrange(self.debuglog.get_area_instances_number(area)) ]
			sz, pos = self._view_for_area(area)
			c.register_line(sz, pos, callbacks)
		c.end_of_registration()
		return c

	def _view_change(self, button):
		if button.get_active():
			self.instance_canvas_sc.show()
			self.canvas_sc.hide()
		else:
			self.instance_canvas_sc.hide()
			self.canvas_sc.show()

class OverviewVisualConfig(VisualConfig):

	def __init__(self, debuglog):
		self.debuglog = debuglog

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		tokens = self.debuglog.frame.get_tokens(item)
		r = []
		for iid in tokens:
			r += [ t + "@" + str(iid) for t in tokens[iid] ]
		d.set_tokens(r)
		return d


class InstanceVisualConfig(VisualConfig):

	def __init__(self, debuglog, area, iid):
		self.debuglog = debuglog
		self.area = area
		self.iid = iid

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		if self.area.is_inside(item):
			d.set_tokens(self.debuglog.frame.get_tokens(item, self.iid))
		return d
