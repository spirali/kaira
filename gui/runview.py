#
#    Copyright (C) 2012 Stanislav Bohm,
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

import os
import gtk
import gtkutils
import mainwindow
import events as evt
from canvas import NetCanvas
import charts
from charts import ChartWidget
from matplotlib.ticker import FuncFormatter
import matplotlib
import utils


class RunView(gtk.VBox):

    def __init__(self, app, tracelog):
        gtk.VBox.__init__(self)
        self.tracelog = tracelog

        self.netinstance_view = NetInstanceView(app)
        self.netinstance_view.set_runinstance(self.tracelog.first_runinstance)

        self.views = [
            ("Replay", self.netinstance_view),
            ("Process utilization", self._processes_utilization()),
            ("Transitions utilization", self._transitions_utilization()),
            ("Transitions utilization threads group", self._transition_utilization_group_threads()),
            ("Numbers of tokens", self._place_chart()),
            # TET = Transition Execution Time
            ("TETs per process", self._processes_histogram()),
            ("Total TETs per process", self._processes_time_sum()),
            ("Total TETs per transition", self._transitions_time_sum())
        ]

        self.pack_start(self._controlls(), False, False)
        for name, item in self.views:
            self.pack_start(item)

    def _controlls(self):
        self.scale = gtk.HScale(gtk.Adjustment(value=0, lower=0,
            upper=self.tracelog.get_runinstances_count(), step_incr=1, page_incr=1, page_size=1))
        toolbar = gtk.HBox(False)

        button = gtk.Button("Export")
        button.connect(
            "clicked", self._export_bigtable)
        toolbar.pack_start(button, False, False)

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

    def _export_bigtable(self, w):
        chooser = gtk.FileChooserDialog(
                title="Export big table", action=gtk.FILE_CHOOSER_ACTION_SAVE,
                buttons=(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                         gtk.STOCK_SAVE, gtk.RESPONSE_OK))

        chooser.set_current_folder(os.getcwd())
        chooser.set_current_name("bigtable.csv")

        filefilter = gtk.FileFilter()
        filefilter.set_name("Coma separated values")
        filefilter.add_mime_type("text/csv")
        filefilter.add_pattern("*.csv")
        chooser.add_filter(filefilter)

        try:
            response = chooser.run()

            if response == gtk.RESPONSE_OK:
                filename = chooser.get_filename()
                save = True
                if os.path.isfile(filename):
                    confirm_dialog = gtk.Dialog(
                        "Confirm save", chooser, 0,
                        (gtk.STOCK_NO, gtk.RESPONSE_NO,
                            gtk.STOCK_OK, gtk.RESPONSE_OK))
                    confirm_dialog.set_default_size(150, 100)
                    label = gtk.Label(
                        "File '{0}' in the chosen directory already exists!\nDo you want to replace it?".format(
                            os.path.basename(filename)))

                    confirm_dialog.get_content_area().add(label)
                    confirm_dialog.show_all()
                    try:
                        confirm_response = confirm_dialog.run()
                        if confirm_response == gtk.RESPONSE_NO:
                            save = False
                    finally:
                        confirm_dialog.destroy()
                if save:
                    self.tracelog.bigtable.export(chooser.get_filename())
                    info_dialog = gtk.Dialog(
                        "Export OK", chooser, 0, (gtk.STOCK_OK, gtk.RESPONSE_OK))
                    label = gtk.Label("The big table was exported.")
                    info_dialog.get_content_area().add(label)
                    info_dialog.show_all()
                    info_dialog.run()
                    info_dialog.destroy()
        finally:
            chooser.destroy()

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
        names = []
        for p in range(self.tracelog.process_count):
            for t in range(self.tracelog.threads_count):
                names.append("process {0}`{1}".format(p, t))

        names.reverse()
        values.reverse()
        return charts.utilization_chart(
                   names,
                   values,
                   "The running time of each processes", "Time", "Process")

    def _transitions_utilization(self):
        names = self.tracelog.statistics["transition_names"]
        values = self.tracelog.statistics["transition_values"]

        values.reverse()
        names.reverse()
        return charts.utilization_chart(
                   names,
                   values,
                   "The running time of each transitions",
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
                "The running time of each transitions",
                "Time",
                "Transition", self.tracelog.threads_count)

    def _place_chart(self):
        values = self.tracelog.statistics["tokens_values"]
        names = self.tracelog.statistics["tokens_names"]

        return charts.place_chart(
                names,
                values,
                "Cout of tokens in places in time",
                "Time",
                "Cout")

    def _processes_histogram(self):
        names = self.tracelog.statistics["proc_hist_names"]
        values = self.tracelog.statistics["proc_hist_values"]
        return charts.histogram(
                names,
                values,
                "Histogram of spent time",
                "Time range",
                "Count")


    def _transitions_time_sum(self):
#        values = self.tracelog.statistics["tr_tsum_values"]
#        names = self.tracelog.statistics["tr_tsum_names"]
#        return charts.time_sum_chart(
#                names,
#                values,
#                "Sum times of each transitions",
#                "Transition",
#                "Time SUM")
        return gtk.VBox()

    def _processes_time_sum(self):
#        values = self.tracelog.statistics["proc_tsum_values"]
#        names = self.tracelog.statistics["proc_tsum_names"]
#        return charts.time_sum_chart(
#                names,
#                values,
#                "Sum times of each processes",
#                "Process",
#                "Time SUM")
        return gtk.VBox()


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
        elif item.is_transition():
            self.open_transition_tab(item)

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
