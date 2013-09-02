#
#    Copyright (C) 2012-2013 Stanislav Bohm
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
import textview
import xml.etree.ElementTree as xml
import mainwindow
import controlseq

class ReportWidget(gtk.ScrolledWindow):

    def __init__(self, app, report):
        gtk.ScrolledWindow.__init__(self)
        self.app = app
        self.report = report
        self.textview = textview.TextViewWithLinks()
        self.add(self.textview)
        self.show_all()

        self.textview.create_tag("h1", scale=2)
        self.textview.create_tag("h2", scale=1.3, weight=700)
        self.textview.create_tag("h3", scale=1.1, weight=500)
        self.textview.create_tag("success", foreground="green", weight=700)
        self.textview.create_tag("fail", foreground="red", weight=700)
        self.report.write(self)

    def write(self, *args, **kw):
        self.textview.write(*args, **kw)

    def write_link(self, *args, **kw):
        self.textview.write_link(*args, **kw)

    def open_textview(self, text, label):
        text_buffer = gtk.TextBuffer()
        text_buffer.insert(text_buffer.get_end_iter(), text)
        text_area = gtk.TextView()
        text_area.set_buffer(text_buffer)
        text_area.set_editable(False)
        sw = gtk.ScrolledWindow()
        sw.add(text_area)
        sw.show_all()
        self.app.window.add_tab(mainwindow.Tab(label, sw))

    def export_control_sequence(self, name, element):
        sequence = controlseq.ControlSequence(element=element)
        sequence.name = name
        self.app.save_sequence_into_project(sequence)

class Report:

    def __init__(self, filename):
        doc = xml.parse(filename)
        self.root = doc.getroot()

    def write(self, widget):
        widget.write("Report\n\n", "h1")
        for e in self.root.findall("analysis"):
            self.write_analysis(widget, e)

    def write_analysis(self, widget, element):
        widget.write("{0}\n".format(element.get("name")), "h2")
        for e in element.findall("result"):
            self.write_result(widget, e)
        widget.write("\n")

    def write_state(self, widget, state):
        name = state.get("name")
        sequence_element = state.find("control-sequence")
        widget.write("  - {0} (distance={1}) ".format(
            name,
            state.get("distance")))
        widget.write("   ")
        widget.write_link("Hash",
                          lambda: widget.open_textview(state.get("hash"), "States"))
        widget.write("   ")
        if sequence_element is not None:
            widget.write_link("Export sequence",
                              lambda: widget.export_control_sequence(name,
                                                                     sequence_element))
        widget.write("\n")

    def write_result(self, widget, element):
        widget.write(element.get("name"))
        if element.get("value"):
            widget.write(" : {0}".format(element.get("value")))
        status = element.get("status")
        if status is not None:
            widget.write("   [")
            if status == "ok":
                t = "success"
            elif status == "fail":
                t = "fail"
            else:
                t = "normal"
            widget.write(status, t)
            widget.write("]")
        if element.get("text"):
            widget.write("   {0}".format(element.get("text")), t)
        widget.write("\n\n")
        states = element.find("states")
        if states is not None:
            widget.write("States\n", "h3")
            for state in states.findall("state"):
                self.write_state(widget,state)
        widget.write("\n")
