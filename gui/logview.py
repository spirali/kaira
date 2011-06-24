#
#    Copyright (C) 2011 Stanislav Bohm
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
from drawing import VisualConfig
import gtkutils
import chart
import runlog
import simview

class LogNetView(simview.NetRunView):

	def __init__(self, logview):
		simview.NetRunView.__init__(self, logview.log.project.get_net(), logview.log.get_paths(), LogviewVisualConfig(self))
		self.logview = logview

	def get_frame(self):
		return self.logview.frame


class LogView(gtk.VBox):
	def __init__(self, app, log):
		gtk.VBox.__init__(self)
		self.log = log
		self.statistics = self.log.get_statistics()
		self.frame = log.get_frame(0)

		self.netview = LogNetView(self)
		self.show_all()

		self.views = [
			("Network", self.netview),
			("Processes", self._processes_utilization()),
			("Transitions", self._transitions_utilization()),
			("Places", self._place_chart()),
		]

		self.pack_start(self._controlls(), False, False)
		for name, item in self.views:
			self.pack_start(item)
		self.netview.show_all()

	def get_frame_pos(self):
		return int(self.scale.get_value())

	def _controlls(self):
		self.scale = gtk.HScale(gtk.Adjustment(value=0, lower=0, 
			upper=self.log.frames_count(), step_incr=1, page_incr=1, page_size=1))
		toolbar = gtk.HBox(False)

		combo = gtk.combo_box_new_text()
		for name, item in self.views:
			combo.append_text(name)
		combo.set_active(0)
		combo.connect("changed", self._view_change)
		toolbar.pack_start(combo, False, False)

		button = gtk.Button("<<")
		button.connect("clicked", lambda w: self.scale.set_value(max(0, self.get_frame_pos() - 1)))
		toolbar.pack_start(button, False, False)

		self.counter_label = gtk.Label()
		toolbar.pack_start(self.counter_label, False, False)

		button = gtk.Button(">>")
		button.connect("clicked", lambda w: 
			self.scale.set_value(min(self.log.frames_count() - 1, self.get_frame_pos() + 1)))
		toolbar.pack_start(button, False, False)

		self.scale.set_draw_value(False)
		self.scale.connect("value-changed", lambda w: self.goto_frame(int(w.get_value())))
		toolbar.pack_start(self.scale)

		self.info_label = gtk.Label()
		toolbar.pack_start(self.info_label, False, False)

		self.update_labels()
		toolbar.show_all()
		return toolbar

	def goto_frame(self, frame_pos):
		self.frame = self.log.get_frame(frame_pos)
		self.update_labels()
		self.redraw()

	def update_labels(self):
		def format(num, max):
			if num is not None:
				return "{0:0>{1}}".format(num, len(str(max)))
			else:
				return "-" * len(str(max))

		max = str(self.log.frames_count() - 1)
		self.counter_label.set_text("{0:0>{2}}/{1}".format(self.get_frame_pos(), max, len(max)))
		time = self.log.get_time_string(self.frame)
		colors = { "I": "gray", "S" : "green", "E" : "#cc4c4c", "R": "lightblue" }
		name = self.frame.name
		process = format(self.frame.get_process_id(self.log), self.log.process_count)
		thread = format(self.frame.get_thread_id(self.log), self.log.threads_count)
		text = "{3}:{4}<span font_family='monospace' background='{2}'>{0}</span>{1}".format(name, time, colors[name], process, thread)
		self.info_label.set_markup(text)

	def redraw(self):
		self.netview.redraw()

	def get_path(self):
		return self.netview.get_path()

	def _place_chart(self):
		vbox = gtk.HBox()

		values = self.statistics["tokens"]
		names = self.statistics["tokens_names"]

		placelist = gtkutils.SimpleList((("Place|foreground", str),("_", str)))

		colors = chart.color_names
		for i, name in enumerate(names):
			placelist.append((name,colors[i % len(colors)]))
		vbox.pack_start(placelist, False, False)

		c = chart.TimeChart(values, min_time = 0, max_time = self.log.maxtime, min_value = 0)
		c.xlabel_format = lambda x: runlog.time_to_string(x)[:-7]
		w = chart.ChartWidget(c)
		vbox.pack_start(w, True, True)

		return vbox

	def _utilization_chart(self, values, names, colors, legend):
		sc = gtk.ScrolledWindow()
		sc.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
		c = chart.UtilizationChart(values, names, legend, colors, (0, self.log.maxtime))
		c.xlabel_format = lambda x: runlog.time_to_string(x)[:-7]
		w = chart.ChartWidget(c)
		w.set_size_request(*c.get_size_request())
		w.show_all()
		sc.add_with_viewport(w)
		return sc

	def _processes_utilization(self):
		colors = [ ((0.2,0.5,0.2), (0.0,0.9,0.0)) ]
		values = self.statistics["processes"]
		names = self.statistics["processes_names"]
		legend = [ (0, "Running") ]
		return self._utilization_chart(values, names, colors, legend)

	def _transitions_utilization(self):
		colors = [ ((0.6,0.6,0.2), (0.85,0.85,0.0)), ((0.2,0.5,0.2), (0.0,0.9,0.0)) ]
		values = self.statistics["transitions"]
		names = self.statistics["transitions_names"]
		legend = [ (0, "Enabled"), (1, "Running") ]
		return self._utilization_chart(values, names, colors, legend)

	def _view_change(self, combo):
		text = combo.get_active_text()
		for name, item in self.views:
			if name == text:
				item.show_all()
			else:
				item.hide()

color_running = ((0.7,0.7,0.1,0.5))
color_started = ((0.1,0.9,0.1,0.5))
color_ended = ((0.8,0.3,0.3,0.5))

class LogviewVisualConfig(VisualConfig):

	def __init__(self, lognetview):
		self.lognetview = lognetview

	def place_drawing(self, item):
		d = VisualConfig.place_drawing(self, item)
		tokens = self.lognetview.get_frame().get_tokens(item, self.lognetview.get_path())
		if tokens:
			d.set_tokens(tokens)
		return d

	def transition_drawing(self, item):
		def transition_filter(items, path):
			return filter (lambda i: i[1] == item.get_id() and (path is None or i[0] == path), items)
		d = VisualConfig.transition_drawing(self, item)
		frame = self.lognetview.get_frame()
		path = self.lognetview.get_path()
		if transition_filter(frame.running, path):
			d.set_highlight(color_running)
		if transition_filter(frame.ended, path):
			d.set_highlight(color_ended)
		if transition_filter(frame.started, path):
			d.set_highlight(color_started)

		return d
