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
import gtkutils
import mainwindow
from canvas import Canvas
import charts
import utils
import cconfig
import citems

class NetInstanceCanvasConfig(cconfig.NetCanvasConfig):

    selection = False

    def __init__(self, view):
        cconfig.NetCanvasConfig.__init__(self)
        self.perspective = None
        self.view = view
        self.token_boxes = {} # Canvas items for tokens by place.id
        self.activations = {} # Canvas items for activations by transition.id

    def set_perspective(self, perspective):
        self.perspective = perspective
        self.set_net(self.perspective.runinstance.net)
        self.configure()

    def configure_item(self, item):
        cconfig.NetCanvasConfig.configure_item(self, item)

    def collect_items(self):
        items = cconfig.NetCanvasConfig.collect_items(self)
        for item in items:
            item.action = None
        if self.net is not None:
            items += self.get_token_items()
            items += self.get_activation_items()
        return items

    def get_activation_items(self):
        result = []
        for transition in self.perspective.runinstance.net.transitions():
            activations = self.activations.get(transition.id)
            if activations is None:
                position = utils.vector_add(
                    transition.box.get_position(), (0, transition.box.size[1] + 10))
                activations = citems.TransitionActivations(
                    None, "activations", citems.AbsPlacement(position))
                self.activations[transition.id] = activations
            activations.values = self.perspective.get_activations_values(transition)
            result.append(activations)
        return result

    def get_token_items(self):
        places = self.perspective.runinstance.net.places()
        result = []
        for place in places:
            token_box = self.token_boxes.get(place.id)
            if token_box is None:
                sx, sy = place.box.size
                position = utils.vector_add(place.box.get_position(),
                                            (sx + 20, sy / 2))
                token_box = citems.TokenBox(None, "tokenbox", citems.AbsPlacement(position))
                self.token_boxes[place.id] = token_box
            token_box.set_tokens(self.perspective.get_tokens(place),
                                 self.perspective.get_new_tokens(place),
                                 self.perspective.get_removed_tokens(place))
            result.append(token_box)
        return result

    def on_item_click(self, item, position):
        if item.kind == "box":
            if item.owner.is_place():
                self.view.open_tokens_tab(item.owner)
            if item.owner.is_transition():
                self.view.open_transition_tab(item.owner)

class RunView(gtk.VBox):

    def __init__(self, app, tracelog):
        gtk.VBox.__init__(self)
        self.tracelog = tracelog

        self.netinstance_view = NetInstanceView(app, None)
        self.netinstance_view.set_config(NetInstanceCanvasConfig(self.netinstance_view))
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
                "Densitiy")


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


class NetInstanceView(gtk.HPaned):

    def __init__(self, app, config=None):
        gtk.HPaned.__init__(self)
        self.app = app
        self.pack1(self._perspectives(), False)
        self.config = config
        self.canvas = Canvas(self.config, zoom=1)
        self.pack2(self.canvas, True)
        self.show_all()

    def set_config(self, config):
        self.config = config
        self.canvas.set_config(config)

    def redraw(self):
        self.canvas.redraw()

    def get_perspective(self):
        return self.perspectives.get_selection(0)

    def set_runinstance(self, runinstance):
        self.runinstance = runinstance
        self._refresh_perspectives(runinstance.get_perspectives())

    def _refresh_perspectives(self, perspectives):
        p = self.get_perspective()
        if p is not None:
            name = self.get_perspective().name
        else:
            name = None
        selected = False
        self.perspectives.clear()
        for p in perspectives:
            i = self.perspectives.append((p, str(p.name)))
            if name == p.name:
                self.perspectives.select_iter(i)
                selected = True
        if not selected:
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
            self.config.set_perspective(perspective)
        else:
            self.config.set_perspective(None)
        self.redraw()

    def open_tokens_tab(self, place):
        text_buffer = gtk.TextBuffer()
        t = self.get_perspective().get_removed_tokens(place)
        removed_tokens = "\n".join("consumed: " + token for token in map(str, t))
        tokens = "\n".join(map(str, self.get_perspective().get_tokens(place)))
        new_tokens = "\n".join(map(str, self.get_perspective().get_new_tokens(place)))

        if removed_tokens != "" and tokens != "":
            removed_tokens = removed_tokens + "\n"
        if tokens != "" and new_tokens != "":
            tokens = tokens + "\n"

        tag_removed = text_buffer.create_tag('removed', font="Italic")
        tag_new = text_buffer.create_tag('new', font="Bold")
        text_buffer.insert_with_tags(text_buffer.get_end_iter(), removed_tokens, tag_removed)
        text_buffer.insert(text_buffer.get_end_iter(), tokens)
        text_buffer.insert_with_tags(text_buffer.get_end_iter(), new_tokens, tag_new)
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

    def open_transition_tab(self, transition):
        values = self.get_perspective().get_transition_trace_values(transition)
        if values:
            text_buffer = gtk.TextBuffer()
            text_buffer.insert(text_buffer.get_end_iter(), "\n".join(map(str, values)))
            text_area = gtk.TextView()
            text_area.set_buffer(text_buffer)
            text_area.set_editable(False)

            sw = gtk.ScrolledWindow()
            sw.add(text_area)
            vbox = gtk.VBox()
            vbox.pack_start(sw)
            vbox.show_all()

            if transition.get_name() == "":
                label = "Traced values in T" + str(transition.get_id())
            else:
                label = "Traced values in " + transition.get_name()
            self.app.window.add_tab(mainwindow.Tab(label, vbox))
