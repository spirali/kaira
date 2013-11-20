#
#    Copyright (C) 2012-2013 Stanislav Bohm,
#                       Martin Surkovsky
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
import charts
import utils
import netview

class RunView(gtk.VBox):

    def __init__(self, app, tracelog):
        gtk.VBox.__init__(self)
        self.tracelog = tracelog

        button = gtk.Button("Export sequence")
        button.connect("clicked", lambda w:
                app.save_sequence_into_project(self.export_sequence()))

        self.netinstance_view = netview.NetView(app, None, other_widgets=[button])
        self.netinstance_view.set_config(
            netview.NetViewCanvasConfig(self.netinstance_view))
        self.netinstance_view.set_runinstance(self.tracelog.first_runinstance)

        self.views = [
            ("Replay", self.netinstance_view),
            ("Utilizations of processes", self._processes_utilization()),
            ("Utilizations of transitions", self._transitions_utilization()),
            ("Utilizations of trans. (by threads)", self._transition_utilization_group_threads()),
            ("Numbers of tokens", self._place_chart()),
            # TET = Transition Execution Time
            ("TETs per process", self._processes_histogram()),
            ("Summations of TETs per process", self._processes_time_sum()),
            ("Summations of TETs per transition", self._transitions_time_sum())
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

    def export_tracelog_table(self, filename):
        self.tracelog.bigtable.export(filename)

    def export_sequence(self):
        return self.tracelog.export_sequence(self.get_event_index())

    def _view_change(self, w):
        text = w.get_active_text()
        for name, item in self.views:
            if name == text:
                item.show_all()
                if isinstance(item, charts.ChartWidget):
                    # set focus on graph canvas
                    item.get_figure().canvas.grab_focus()
            else:
                item.hide()

    def save_as_svg(self, filename):
        self.netinstance_view.save_as_svg(filename)

    def update_labels(self):
        def format(num, max):
            if num is not None:
                return "{0:0>{1}}".format(num, len(str(max)))
            else:
                return "-" * len(str(max))

        index = self.get_event_index()
        last_index = self.tracelog.get_runinstances_count() - 1
        m = str(last_index)
        maxtime = utils.time_to_string(self.tracelog.get_event_time(last_index))
        self.counter_label.set_text("{0:0>{2}}/{1}".format(index, m, len(m)))
        time = "{0:0>{1}}".format(utils.time_to_string(self.tracelog.get_event_time(index)),
                                  len(maxtime))
        text = "<span font_family='monospace'>{0}/{1} {3} {2}</span>".format(
            self.tracelog.get_event_process(index),
            self.tracelog.get_event_thread(index),
            self.tracelog.get_event_name(index),
            time)
        self.info_label.set_markup(text)

    def _processes_utilization(self):
        values = self.tracelog.statistics["threads"]
        idles = self.tracelog.statistics["idles"]

        names = []
        for p in range(self.tracelog.process_count):
            for t in range(self.tracelog.threads_count):
                names.append("process {0}`{1}".format(p, t))

        names.reverse()
        values.reverse()
        idles.reverse()
        return charts.utilization_chart(
                   names,
                   values,
                   "Utilizations of processes", "Time", "Processes",
                   idles=idles)

    def _transitions_utilization(self):
        names = self.tracelog.statistics["transition_names"]
        values = self.tracelog.statistics["transition_values"]

        values.reverse()
        names.reverse()
        return charts.utilization_chart(
                   names,
                   values,
                   "Utilizations of transitions",
                   "Time",
                   "Transition")

    def _transition_utilization_group_threads(self):
        names = self.tracelog.statistics["trans_gthreads_names"]
        values = self.tracelog.statistics["trans_gthreads_values"]

        values.reverse()
        names.reverse()
        return charts.utilization_chart(
                names,
                values,
                "Utilization of transitions (group by threads)",
                "Time",
                "Transition", self.tracelog.threads_count)

    def _place_chart(self):
        values = self.tracelog.statistics["tokens_values"]
        names = self.tracelog.statistics["tokens_names"]

        return charts.place_chart(
                names,
                values,
                "Number of tokens in places",
                "Time",
                "Number of tokens")

    def _processes_histogram(self):
        names = self.tracelog.statistics["proc_hist_names"]
        values = self.tracelog.statistics["proc_hist_values"]
        return charts.histogram(
                names,
                values,
                "Histograms of transition execution times",
                "Time",
                "Density")


    def _transitions_time_sum(self):
        values = self.tracelog.statistics["tr_tsum_values"]
        names = self.tracelog.statistics["tr_tsum_names"]
        return charts.time_sum_chart(
                names,
                values,
                "Summations of transition execution times",
                "Transitions",
                "Sum of times")

    def _processes_time_sum(self):
        values = self.tracelog.statistics["proc_tsum_values"]
        names = self.tracelog.statistics["proc_tsum_names"]
        return charts.time_sum_chart(
                names,
                values,
                "Summations of transition execution times for each processes",
                "Processes",
                "Sum of times")
