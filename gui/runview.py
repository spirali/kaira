#
#    Copyright (C) 2012 Stanislav Bohm
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
import mainwindow
import events as evt
from canvas import NetCanvas
import chart
from newcharts import ChartWidget, Data2DChart
from matplotlib.ticker import FuncFormatter
import matplotlib

class RunView(gtk.VBox):

    def __init__(self, app, tracelog):
        gtk.VBox.__init__(self)
        self.tracelog = tracelog

        self.netinstance_view = NetInstanceView(app)
        self.netinstance_view.set_runinstance(self.tracelog.first_runinstance)

        self.views = [
            ("Replay", self.netinstance_view),
            ("Processes", self._processes_utilization()),
            ("Transitions", self._transitions_utilization()),
            ("Places", self._place_chart()),
            ("Processes histogram", self._processes_histogram()),
            ("Transitions histogram", self._transitions_histogram())
        ]

        self.pack_start(self._controlls(), False, False)
        for name, item in self.views:
            self.pack_start(item)

    def _controlls(self):
        self.scale = gtk.HScale(gtk.Adjustment(value=0, lower=0,
            upper=self.tracelog.get_runinstances_count(), step_incr=1, page_incr=1, page_size=1))
        toolbar = gtk.HBox(False)

        combo = gtk.combo_box_new_text()
        for name, item in self.views:
            combo.append_text(name)
        combo.set_active(0)
        combo.connect("changed", self._view_change)
        toolbar.pack_start(combo, False, False)

        button = gtk.Button("<<")
        button.connect("clicked", lambda w: self.scale.set_value(max(0, self.get_event_index() - 1)))
        toolbar.pack_start(button, False, False)

        self.counter_label = gtk.Label()
        toolbar.pack_start(self.counter_label, False, False)

        button = gtk.Button(">>")
        button.connect("clicked", lambda w:
            self.scale.set_value(min(self.tracelog.get_runinstances_count() - 1,
                                     self.get_event_index() + 1)))

        toolbar.pack_start(button, False, False)

        self.scale.set_draw_value(False)
        self.scale.connect("value-changed", lambda w: self.show_runinstance())
        toolbar.pack_start(self.scale)

        self.info_label = gtk.Label()
        toolbar.pack_start(self.info_label, False, False)

        self.update_labels()
        toolbar.show_all()
        return toolbar

    def get_event_index(self):
        return int(self.scale.get_value())

    def show_runinstance(self):
        index = self.get_event_index()
        runinstance = self.tracelog.get_event_runinstance(index)
        self.netinstance_view.set_runinstance(runinstance)
        self.update_labels()

    def _view_change(self, w):
        text = w.get_active_text()
        for name, item in self.views:
            if name == text:
                item.show_all()
                if isinstance(item, ChartWidget):
                    # set focus on graph canvas
                    item.get_figure().canvas.grab_focus()
            else:
                item.hide()

    def update_labels(self):
        def format(num, max):
            if num is not None:
                return "{0:0>{1}}".format(num, len(str(max)))
            else:
                return "-" * len(str(max))

        index = self.get_event_index()
        last_index = self.tracelog.get_runinstances_count() - 1
        m = str(last_index)
        maxtime = time_to_string(self.tracelog.get_event_time(last_index))
        self.counter_label.set_text("{0:0>{2}}/{1}".format(index, m, len(m)))
        time = "{0:0>{1}}".format(time_to_string(self.tracelog.get_event_time(index)),
                                                 len(maxtime))
        text = "<span font_family='monospace'>{0}/{1} {3} {2}</span>".format(
            self.tracelog.get_event_process(index),
            self.tracelog.get_event_thread(index),
            self.tracelog.get_event_name(index),
            time)
        self.info_label.set_markup(text)

    def _utilization_chart(self, values, names, colors, legend):
        sc = gtk.ScrolledWindow()
        sc.set_policy(gtk.POLICY_NEVER, gtk.POLICY_AUTOMATIC)
        maxtime = self.tracelog.get_max_time()
        c = chart.UtilizationChart(values, names, legend, colors, (0, maxtime))
        c.xlabel_format = lambda x: time_to_string(x)[:-7]
        w = chart.ChartWidget(c)
        w.set_size_request(*c.get_size_request())
        w.show_all()
        sc.add_with_viewport(w)
        return sc

    def _processes_utilization(self):
        colors = ["#00aa00"]
        values = self.tracelog.statistics["threads"]
        names =  [ "process {0}".format(p) for p in xrange(self.tracelog.process_count) ]

        # Transform data for chart
        values.reverse()
        names.reverse()
        lines = []
        for i, line in enumerate(values):
            l2 = line[0]
            l2.remove(l2[0])
            size = len(l2) - 1
            l1 = []
            for j in range(0, size, 2):
                l1.append((l2[j][0], (l2[j+1][0] - l2[j][0])))
            lines.append(l1)

        # Create chart
        chart_widget = ChartWidget()
        figure = chart_widget.get_figure()
        w, h = figure.get_size_inches()
        dpi = figure.get_dpi()
        # resize figure, it depends on count of lines (processes)
        figure.set_size_inches(w, len(lines) * 0.8) 
        # resize canvas, it depends on count of lines (processes)
        figure.canvas.set_size_request(int(w * dpi), int(len(lines) * 0.7 * dpi))

        chart_id = chart_widget.create_new_chart(ChartWidget.UTILIZATION_CHART)
        chart = chart_widget.get_chart(chart_id)
        chart.fill_data(lines, names, colors)

        # set basic properties
        chart.xaxis.grid(True, linestyle="-", which='major', color='black', alpha=0.7)
        chart.xaxis.set_major_formatter(FuncFormatter(
            lambda time, pos: time_to_string(time)[:-7]))
        chart.set_xlim(xmin=0)
        chart.get_figure().tight_layout()

        chart.set_title("The running time of each processes")
        chart.set_xlabel("Time")
        chart.set_ylabel("Process")
        return chart_widget

    def _transitions_utilization(self):
        colors = ["#00aa00"]
        names = self.tracelog.statistics["transition_names"]
        values = self.tracelog.statistics["transition_values"]

        # Transform data for chart
        values.reverse()
        names.reverse()
        lines = []
        for i, line in enumerate(values):
            l2 = line[0]
            size = len(l2) - 1
            l1 = []
            for j in range(0, size, 2):
                l1.append((l2[j][0], (l2[j+1][0] - l2[j][0])))
            lines.append(l1)

        # Create chart
        chart_widget = ChartWidget()
        figure = chart_widget.get_figure()
        w, h = figure.get_size_inches()
        dpi = figure.get_dpi()
        # resize figure, it depends on count of lines (processes)
        figure.set_size_inches(w, len(lines) * 0.8) 
        # resize canvas, it depends on count of lines (processes)
        figure.canvas.set_size_request(int(w * dpi), int(len(lines) * 0.7 * dpi))

        chart_id = chart_widget.create_new_chart(ChartWidget.UTILIZATION_CHART)
        chart = chart_widget.get_chart(chart_id)
        chart.fill_data(lines, names, colors)

        # set basic properties
        chart.xaxis.grid(True, linestyle="-", which='major', color='black', alpha=0.7)
        chart.xaxis.set_major_formatter(FuncFormatter(
            lambda time, pos: time_to_string(time)[:-7]))
        chart.set_xlim(xmin=0)
        chart.get_figure().tight_layout()

        chart.set_title("The running time of each transitions")
        chart.set_xlabel("Time")
        chart.set_ylabel("Transition")

        if isinstance(chart, evt.EventSource):
            chart.set_callback("change_slider", 
                    lambda time: self.scale.set_value(
                        self.tracelog.get_index_from_time(time)))

        return chart_widget

    def _place_chart(self):
        values = self.tracelog.statistics["tokens_values"]
        names = self.tracelog.statistics["tokens_names"]

        data = Data2DChart((names, values))
        chart_widget = ChartWidget()
        chart_id = chart_widget.create_new_chart(ChartWidget.PLACE_CHART)
        chart = chart_widget.get_chart(chart_id)
        chart.fill_data(data)

        # nastavovaci funkce musi byt az po vykresleni
        chart.xaxis.set_major_formatter(FuncFormatter(
            lambda time, pos: time_to_string(time)[:-7]))
        chart.set_xlim(xmin = 0)
        chart.get_figure().tight_layout()
        chart.set_title("Cout of tokens in places in time")
        chart.set_xlabel("Time")
        chart.set_ylabel("Count")
        return chart_widget

    def _transitions_histogram(self):
        values = self.tracelog.statistics["tr_hist_values"]
        names = self.tracelog.statistics["tr_hist_names"]

        chart_widget = ChartWidget()
        chart_id = chart_widget.create_new_chart(ChartWidget.HISTOGRAM_CHART)
        chart = chart_widget.get_chart(chart_id)
        chart.fill_data(names, values)

        # set basic properties
        chart.yaxis.set_major_formatter(FuncFormatter(
            lambda time, pos: time_to_string(time)[:10]))
        chart.set_title("Histogram of sum time of each transitions")
        chart.set_xlabel("Transitions")
        chart.set_ylabel("Time SUM")
        return chart_widget

    def _processes_histogram(self):
        values = self.tracelog.statistics["proc_hist_values"]
        names = self.tracelog.statistics["proc_hist_names"]

        chart_widget = ChartWidget()
        chart_id = chart_widget.create_new_chart(ChartWidget.HISTOGRAM_CHART)
        chart = chart_widget.get_chart(chart_id)
        chart.fill_data(names, values)

        # set basic properties
        chart.yaxis.set_major_formatter(FuncFormatter(
            lambda time, pos: time_to_string(time)[:10]))
        chart.set_title("Histogram of sum time of each processes")
        chart.set_xlabel("Processes")
        chart.set_ylabel("Time SUM")
        return chart_widget


class NetInstanceView(gtk.HPaned):

    def __init__(self, app):
        gtk.HPaned.__init__(self)
        self.app = app
        self.pack1(self._perspectives(), False)
        self.canvas_sc = gtk.ScrolledWindow()
        self.canvas = self._create_canvas(None)
        self.canvas_sc.add_with_viewport(self.canvas)

        self.pack2(self.canvas_sc, True)
        self.show_all()

    def redraw(self):
        self.canvas.redraw()

    def get_perspective(self):
        return self.perspectives.get_selection(0)

    def set_runinstance(self, runinstance):
        self.runinstance = runinstance
        self.canvas.set_net(runinstance.net)
        self._refresh_perspectives(runinstance.get_perspectives())

    def _create_canvas(self, vconfig):
        c = NetCanvas(None, None, vconfig, zoom = 1)
        c.set_callback("button_down", self._button_down)
        c.show()
        return c

    def _refresh_perspectives(self, perspectives):
        p = self.get_perspective()
        self.perspectives.clear()
        for pe in perspectives:
            i = self.perspectives.append((pe, str(pe.name)))
            if p == pe:
                self.perspectives.select_iter(i)
        if p not in perspectives:
            self.perspectives.select_first()
        self._perspectives_changed(None)

    def _perspectives(self):
        self.perspectives = gtkutils.SimpleList((("_", object), ("Views",str)))
        self.perspectives.set_size_request(80,10)
        self.perspectives.connect_view("cursor-changed", self._perspectives_changed);
        return self.perspectives

    def _perspectives_changed(self, w):
        perspective = self.get_perspective()
        if perspective is not None:
            self.canvas.set_vconfig(perspective.get_visual_config())
        else:
            self.canvas.set_vconfig(None)
        self.redraw()

    def _button_down(self, event, pos):
        if self.runinstance.net is None:
            return
        item = self.runinstance.net.get_item_at_position(pos)
        if item is None:
            return
        self.on_item_click(item)

    def on_item_click(self, item):
        if item.is_place():
            self.open_tokens_tab(item)

    def open_tokens_tab(self, place):
        text_buffer = gtk.TextBuffer()
        tokens = "\n".join(map(str, self.get_perspective().get_tokens(place)))
        text_buffer.insert(text_buffer.get_end_iter(), tokens)
        text_area = gtk.TextView()
        text_area.set_buffer(text_buffer)
        text_area.set_editable(False)

        sw = gtk.ScrolledWindow()
        sw.add(text_area)
        vbox = gtk.VBox()
        vbox.pack_start(sw)
        vbox.show_all()

        label = "Tokens of " + place.get_name()
        self.app.window.add_tab(mainwindow.Tab(label, vbox))


def time_to_string(nanosec):
    s = int(nanosec) / 1000000000
    nsec = nanosec % 1000000000
    sec = s % 60
    minutes = (s / 60) % 60
    hours = s / 60 / 60
    return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)
