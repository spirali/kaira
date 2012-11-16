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
import xml.etree.ElementTree as xml

class ReportWidget(gtk.ScrolledWindow):

    def __init__(self, report):
        gtk.ScrolledWindow.__init__(self)
        self.report = report
        textview = gtk.TextView()
        textview.set_editable(False)
        self.add(textview)
        self.buffer = textview.get_buffer()
        self.show_all()

        self.buffer.create_tag("normal")
        self.buffer.create_tag("h1", scale=2)
        self.buffer.create_tag("h2", scale=1.2, weight=700)
        self.buffer.create_tag("success", foreground="green", weight=700)
        self.buffer.create_tag("fail", foreground="red", weight=700)

        self.report.write(self)

    def write(self, text, tag_name="normal"):
        self.buffer.insert_with_tags_by_name(self.buffer.get_end_iter(), text, tag_name)

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
        widget.write("\n")
