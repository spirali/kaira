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

class SourceType:

    def __init__(self, standard_extension, display_name):
        self.standard_extension = standard_extension
        self.display_name = display_name
        # TODO: also should be there reference to "standard loader" and
        # "view - optional"

    def get_display_name(self):
        return self.display_name

    def get_standard_extension(self):
        return self.standard_extension

class Source:

    def __init__(self, name, type, data=None):
        self.name = name
        self.type = type
        self.data = data

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

class SourceView(gtk.Alignment):

    def __init__(self, input_src):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        self.set_padding(5, 0, 10, 10)

        self.input_src = input_src

        def f(w, e):
            print e.x

        self.lbl_name = gtk.Label()
        self.lbl_name.set_alignment(0, 1)

        self.lbl_type = gtk.Label()
        self.lbl_type.set_alignment(0, 0)

        if input_src.data is not None:
            self.lbl_name.set_markup(input_src.name)
            self.lbl_type.set_markup(
                "<i>{0}</i>".format(input_src.type.get_display_name()))
        else:
            self.lbl_name.set_markup(
                "<span color='gray'>{0}</span>".format(input_src.name))
            self.lbl_type.set_markup(
                "<span color='gray'><i>{0}</i></span>".format(
                    input_src.type.get_display_name()))

        self.btn_load = gtk.Button("Load")
        self.btn_show = gtk.Button("Show")
        self.btn_attach = gtk.Button("Attach")

        self.btn_show.set_sensitive(False)
        self.btn_attach.set_sensitive(False)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        table = gtk.Table(2, 3, False)
        table.set_border_width(2)
        table.set_col_spacing(0, 10)
        # attach(left_attach, right_attach, top_attach, bottom_attach,
        #        xoptions=gtk.EXPAND|gtk.FILL, yoptions=gtk.EXPAND|gtk.FILL,
        #        xpadding=0, ypadding=0)

        table.attach(self.lbl_name, 0, 1, 0, 1)
        table.attach(self.lbl_type, 0, 1, 1, 2)
        table.attach(self.btn_load, 1, 2, 0, 1, xoptions=0)
        table.attach(self.btn_show, 1, 2, 1, 2, xoptions=0)
        table.attach(self.btn_attach, 2, 3, 0, 2, xoptions=0)

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

        kth_type = SourceType("kth", "kaira tracelog header")

        paned = gtk.HPaned()

        a = []
        for i in range(0, 20):
            input_src = Source("Process", kth_type)
            a.append(SourceView(input_src))
        c_sources = self._column(a)

        paned.pack1(c_sources, resize=True)

        paned2 = gtk.VPaned()

        a = []
        for i in range(0, 20):
            a.append(gtk.Button("Ahoj"))
        c_actions = self._column(a)
        paned2.pack1(c_actions, resize=False)

#        c_outputs = self._column()
        c_outputs = gtk.HPaned()
        c_outputs.pack1(self._column())
        c_outputs.pack2(self._column())
        paned2.pack2(c_outputs, resize=False)

        paned.pack2(paned2, resize=True)
        self.pack_start(paned)

        #labels
#        labels = gtk.HBox(False)
#
#        title = gtk.Label("Sources:")
#        haling = gtk.Alignment(0, 0, 0, 0)
#        haling.set_padding(0, 0, 5, 0)
#        haling.add(title)
#        labels.pack_start(haling)
#
#        title = gtk.Label("Actions:")
#        haling = gtk.Alignment(0, 0, 0, 0)
#        haling.set_padding(0, 0, 20, 0)
#        haling.add(title)
#        labels.pack_start(haling)
#
#        title = gtk.Label("Outputs:")
#        haling = gtk.Alignment(0, 0, 0, 0)
#        haling.set_padding(0, 0, 10, 0)
#        haling.add(title)
#        labels.pack_start(haling)
#
#        self.pack_start(labels, False, False)


#        # columns
#        columns = gtk.HBox(False)
#
#        kth_type = SourceType("kth", "kaira tracelog header")
#        txt_type = SourceType("txt", "plain text")
#        a = []
#        for i in range(0, 20):
#            if i == 2:
#                input_src = Source("Read", txt_type)
#            else:
#                input_src = Source("Process", kth_type)
#            a.append(SourceView(input_src))
#        c_inputs = self._column(a)
#        c_inputs.set_border_width(5)
#        columns.pack_start(c_inputs)
#
#        a = []
#        for i in range(0, 20):
#            a.append(gtk.Button("Ahoj"))
#        c_actions = self._column(a)
#        c_actions.set_border_width(5)
#        columns.pack_start(c_actions)
#
#        c_outputs = self._column()
#        c_outputs.set_border_width(5)
#        columns.pack_start(c_outputs)
#
#        self.pack_start(columns, True, True)

        self.show_all()

    def _column(self, items=[]):
        scw = gtk.ScrolledWindow()
        column = gtk.VBox(False)
        for item in items:
            column.pack_start(item, False, False)
        scw.add_with_viewport(column)
        return scw
