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
from exportri import place_counter_name

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
        self.netinstance_view.set_runinstance(tracelog.first_runinstance)

        table = tracelog.data

        net = tracelog.project.nets[0]
        processes = range(tracelog.process_count)
        transitions = [ t for t in net.transitions() if t.trace_fire ]
        places = [ p for p in net.places() if p.trace_tokens ]

        self.views = [ ("Replay", self.netinstance_view) ]
        self.views.append(process_utilization(table, processes))
        self.views.append(transition_utilization(table, processes, transitions))
        self.views.append(tet_per_processes_and_transitions_histogram(
            table, processes, transitions))
        self.views.append(tet_per_processes_histogram(table, processes))
        self.views.append(tet_per_transitions_histogram(table, transitions))
        self.views.append(tokens_count(table, processes, places))

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
        text = "<span font_family='monospace'>{0} {2} {1}</span>".format(
            self.tracelog.get_event_process(index),
            self.tracelog.get_event_name(index),
            time)
        self.info_label.set_markup(text)

def process_utilization(table, processes):
    required = ["Event", "Process", "Time", "Duration"]
    header = table.header

    if not all(item in header for item in required):
        return

    f_eq = lambda x, y: x == y

    columns = ["Time", "Duration"]
    # collect idles
    filters = [("Event", f_eq, 'I')]
    idles = []
    for p in processes:
        idles.append(table.select(columns, filters + [("Process", f_eq, p)]))

    # collect TETs
    filters = [("Event", f_eq, 'T')]
    names, values = [], []
    for p in processes:
        names.append(str(p))
        values.append(table.select(columns, filters + [("Process", f_eq, p)]))

    names.reverse()
    values.reverse()
    if idles is not None:
        idles.reverse()
    return ("Utilization of processes",
            charts.utilization_chart(
                names, values,
                "Utilization of processes", "Time", "Process", idles))

def transition_utilization(table, processes, transitions):
    required = ["Event", "ID", "Time", "Duration"]
    header = table.header

    if not all(item in header for item in required):
        return

    f_eq = lambda x, y: x == y
    columns = ["Time", "Duration"]
    filters = [("Event", f_eq, 'T')]
    if "Process" in header:
        names, values = [], []
        for p in processes:
            pnames, pvalues = [], []
            for t in transitions:
                pnames.append("{0} {1}".format(t.get_name_or_id(), p))
                pvalues.append(table.select(
                    columns,
                    filters + [("ID", f_eq, t.id), ("Process", f_eq, p)]))
            names.append(pnames)
            values.append(pvalues)

        # reorder by processes; process zero at the top
        values.reverse()
        names.reverse()
        # concatenate names on each process
        f_concate = lambda x, y: x + y
        values = reduce(f_concate, values, [])
        names = reduce(f_concate, names, [])
    else:
        names, values = [], []
        for t in transitions:
            names.append(t.get_name_or_id())
            values.append(table.select(
                columns, filters + [("ID", f_eq, t.id)]))

    return ("Utilization of transitions",
            charts.utilization_chart(
                names, values,
                "Utilization of transitions", "Time", "Transition"))

def tet_per_processes_and_transitions_histogram(table, processes, transitions):
    required = ["Event", "Process", "Duration", "ID"]
    header = table.header

    if not all(item in header for item in required):
       return

    f_eq = lambda x, y: x == y
    columns = ["Duration"]
    filters = [("Event", f_eq, 'T')]
    names, values = [], []
    for tran in transitions:
        f = filters + [("ID", f_eq, tran.id)]
        for p in processes:
            names.append("{0}`{1}".format(tran.get_name_or_id(), p))
            tets = table.select(columns, f + [("Process", f_eq, p)])

            if len(tets) == 0: # tets is a numpy array
                tets = [0] # data for a histogram chart must not be empty
            values.append(tets)

    return ("Transition execution times (TETs)",
            charts.histogram(
                names, values, "Histogram of transition execution times",
                "Duration [ms]", "Count"))

def tet_per_processes_histogram(table, processes):
    required = ["Event", "Process", "Duration"]
    header = table.header

    if not all(item in header for item in required):
       return

    f_eq = lambda x, y: x == y
    columns = ["Duration"]
    filters = [("Event", f_eq, 'T')]
    names, values = [], []
    for p in processes:
        names.append("Process {0}".format(p))
        tets = table.select(columns, filters + [("Process", f_eq, p)])

        if len(tets) == 0:
            tets = [0]
        values.append(tets)

    return ("TETs (grouped by processes)",
            charts.histogram(
                names, values, "Histogram of transition execution times grouped by processes",
                "Duration [ms]", "Count"))

def tet_per_transitions_histogram(table, transitions):
    required = ["Event", "Duration", "ID"]
    header = table.header

    if not all(item in header for item in required):
       return

    f_eq = lambda x, y: x == y
    columns = ["Duration"]
    filters = [("Event", f_eq, 'T')]
    names, values = [], []
    for t in transitions:
        names.append(t.get_name_or_id())
        tets = table.select(columns, filters + [("ID", f_eq, t.id)])

        if len(tets) == 0:
            tets = [0]
        values.append(tets)

    return ("TETs (grouped by transitions)",
            charts.histogram(
                names, values, "Histogram of transition execution times grouped by transitions",
                "Duration [ms]", "Count"))

def tokens_count(table, processes, places, collapse=True):
    required = ["Event", "Process", "Time"] + \
               [place_counter_name(place) for place in places]
    header = table.header

    if not all(item in header for item in required):
        return

    f_eq = lambda x, y: x == y
    filters = [("Event", f_eq, 'C')]
    names, values = [], []
    for place in places:
        columns = ["Time", place_counter_name(place)]
        for p in processes:
            names.append("{0}@{1}".format(place.get_name_or_id(), p))
            counts = table.select(columns, filters + [("Process", f_eq, p)])
            values.append((counts[columns[0]], counts[columns[1]]))

    return ("Number of tokens",
            charts.place_chart(
                names, values, "Number of tokens in places", "Time", "Count"))



