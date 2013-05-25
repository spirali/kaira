#
#    Copyright (C) 2013 Martin Surkovsky
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

class InputView(gtk.Alignment):

    def __init__(self):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        self.set_padding(5, 0, 10, 10)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        table = gtk.Table(2, 3, True)
        table.set_border_width(2)
        table.attach(gtk.Button("Cls"), 0, 1, 0, 1)
        table.attach(gtk.Button("Cls"), 2, 3, 0, 2)
        frame.add(table)

        self.add(frame)

class TriColumnsWidget(gtk.VBox):

    def __init__(self):
        gtk.VBox.__init__(self)

        # toolbar
        toolbar = gtk.HBox(False)
        toolbar.set_border_width(5)
        button = gtk.Button("Load")
        toolbar.pack_start(button, False, False)

        button = gtk.Button("Store")
        toolbar.pack_start(button, False, False)
        self.pack_start(toolbar, False, False)
        
        #labels
        labels = gtk.HBox(False)

        title = gtk.Label("Inputs:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 0, 5, 0)
        haling.add(title)
        labels.pack_start(haling)

        title = gtk.Label("Actions:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 0, 20, 0)
        haling.add(title)
        labels.pack_start(haling)

        title = gtk.Label("Outputs:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 0, 10, 0)
        haling.add(title)
        labels.pack_start(haling)

        self.pack_start(labels, False, False)

        # columns
        columns = gtk.HBox(False)

        a = []
        for i in range(0, 20):
            a.append(InputView())
        c_inputs = self._column(a)
        c_inputs.set_border_width(5)
        columns.pack_start(c_inputs)

        a = []
        for i in range(0, 20):
            a.append(gtk.Button("Ahoj"))

        c_actions = self._column(a)
        c_actions.set_border_width(5)
        columns.pack_start(c_actions)

        c_outputs = self._column()
        c_outputs.set_border_width(5)
        columns.pack_start(c_outputs)

        self.pack_start(columns, True, True)
        self.show_all()
    
    def _column(self, items=[]):
        scw = gtk.ScrolledWindow()
        column = gtk.VBox(False)
        for item in items:
            column.pack_start(item, False, False)
        scw.add_with_viewport(column)
        return scw
