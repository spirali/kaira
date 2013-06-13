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
import math
import cairo

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
        self.type = type # SourceType
        self.data = data

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

class Action:

    def __init__(self, name, description):
        self.name = name
        self.description = description

    def get_name(self):
        return self.name

    def get_description(self):
        return self.description

    def get_required_sources(self): # interface
        return [] # array of sources

    def get_processed_data(self): # interface
        return None

    def run(self): # interface
        pass

class MyIcon(gtk.DrawingArea):

    def __init__(self, state):
        self.icon_state = state
        gtk.DrawingArea.__init__(self)
        self.set_size_request(70, 70)
        self.connect("expose_event", self._expose)

    def _expose(self, widget, event):
        cr = widget.window.cairo_create()
        rect = self.get_allocation()
        self._draw(cr, rect.width, rect.height)

    def _draw(self, cr, width, height):
        # clear background
        cr.set_source_rgba(1,1,1,1)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        # draw
        x = width / 2
        y = height / 2
        radius = min(width / 2, height / 2) - 5

#        cr.arc(x, y, radius, 0, 2 * math.pi)
#        cr.set_source_rgb(1, 0, 0)
#        cr.fill()

        radial = cairo.RadialGradient(0, 0, 0.0, x, y, radius)
        radial.add_color_stop_rgb(0, 1, 0, 1)
        radial.add_color_stop_rgb(0, 1, 0, 0)
        cr.set_source(radial)
        cr.fill()

        cr.set_line_width(1.5)
        cr.arc(x, y, radius, 0, 2 * math.pi)
        cr.set_source_rgb(0, 0, 0)
        cr.stroke()

class ActionViewShort(gtk.Alignment):

    def __init__(self, action):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        self.set_padding(5, 0, 5, 5)

        hbox = gtk.HBox(False)

        # ready | incomplete | incorrect
        icon = MyIcon("incorrect")
        hbox.pack_start(icon, False, False)

        lbl_name = gtk.Label()
        lbl_name.set_alignment(0, 0.5)
        lbl_name.set_padding(2, 0)
        lbl_name.set_markup("<b>{0}</b>".format(action.get_name()))
        hbox.pack_start(lbl_name, True, True)

        btn_choose = gtk.Button("Choose")
        hbox.pack_start(btn_choose, False, False)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        frame.add(hbox)
        self.add(frame)

class ActionViewFull(gtk.VBox):

    def __init__(self, action):
        gtk.VBox.__init__(self, False)
        self.action = action

        # line with name and run button
        hbox = gtk.HBox(False)

        lbl_name = gtk.Label()
        lbl_name.set_alignment(0, 1)
        lbl_name.set_markup("<b>{0}</b>".format(action.get_name()))
        halign = gtk.Alignment(0, 0.5, 0, 0)
        halign.set_padding(0, 0, 2, 0)
        halign.add(lbl_name)
        hbox.pack_start(halign, True, True)

        self.btn_run = gtk.Button("Run")
        self.btn_run.set_sensitive(False)
        hbox.pack_start(self.btn_run, False, False)

        self.pack_start(hbox, False, False)

        # description
        if action.description is not None or action.description != "":
            def cb_allocate(label, allocation ):
                label.set_size_request(allocation.width - 2, -1)

            frame = gtk.Frame()
            frame.set_label("Description")
            lbl_description = gtk.Label()
            lbl_description.set_alignment(0, 1)
            lbl_description.set_max_width_chars(15)
            lbl_description.set_line_wrap(True)
            lbl_description.set_markup("<i>{0}</i>".format(action.description))
            lbl_description.connect( "size-allocate", cb_allocate )
            frame.add(lbl_description)
            self.pack_start(frame, False, False)

        required_sources = action.get_required_sources()
        for source in required_sources:
            hbox = gtk.HBox(False)
            lbl_src_name = gtk.Label()
            lbl_src_name.set_alignment(0, 0.5)
            lbl_src_name.set_text(source.get_name())
            hbox.pack_start(lbl_src_name, False, False)

            lbl_src_type = gtk.Label()
            lbl_src_type.set_alignment(0, 0.5)
            lbl_src_type.set_markup(" <i>(*.{0})</i>".format(
                source.get_type().get_standard_extension()))
            hbox.pack_start(lbl_src_type, True, True)

            btn_choose = gtk.Button("Load")
            hbox.pack_start(btn_choose, False, False)

            self.pack_start(hbox, False, False)

class SourceView(gtk.Alignment):

    def __init__(self, input_src):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        self.set_padding(5, 0, 10, 10)

        self.input_src = input_src

        self.lbl_name = gtk.Label()
        self.lbl_name.set_alignment(0, 1)

        self.lbl_type = gtk.Label()
        self.lbl_type.set_alignment(0, 0)
        self.lbl_name.set_markup("<b>{0}</b>".format(input_src.get_name()))
        self.lbl_type.set_markup(
            "<i>{0}</i>".format(input_src.type.get_display_name()))

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

        table.attach(self.lbl_name,   0, 1, 0, 1)
        table.attach(self.lbl_type,   0, 1, 1, 2)
        table.attach(self.btn_load,   1, 2, 0, 1, xoptions=0)
        table.attach(self.btn_show,   1, 2, 1, 2, xoptions=0)
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

        # tri-columns
        kth_type = SourceType("kth", "kaira tracelog header")

        paned = gtk.HPaned()

        column1 = gtk.VBox(False)

        title = gtk.Label("Sources:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        column1.pack_start(haling, False, False)

        a = []
        for i in range(0, 20):
            input_src = Source("Process", kth_type)
            a.append(SourceView(input_src))
        c_sources = self._column(a)
        column1.pack_start(c_sources, True, True)

        paned.pack1(column1, resize=True)

        paned2 = gtk.HPaned()

        column2 = gtk.VBox(False)
        title = gtk.Label("Actions:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        column2.pack_start(haling, False, False)

        action = Action("Show net", "Tato komponenta dela neco hrozme moc sloziteho :D")
        action.get_required_sources = lambda: [Source("Process", kth_type)];
        a = []
        for i in range(0, 20):
            a.append(ActionViewShort(action))
        c_actions = self._column(a)
        column2.pack_start(c_actions)

        pp = gtk.VPaned()
        pp.pack1(column2, resize=True)
        pp.pack2(ActionViewFull(action), resize=True)

        paned2.pack1(pp, resize=True)

        column3 = gtk.VBox(False)
        title = gtk.Label("Outputs:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        column3.pack_start(haling, False, False)

        c_outputs = self._column()
        column3.pack_start(c_outputs)
        paned2.pack2(column3, resize=True)

        paned.pack2(paned2, resize=True)
        self.pack_start(paned)

        self.show_all()

    def _column(self, items=[]):
        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        column = gtk.VBox(False)
        for item in items:
            column.pack_start(item, False, False)
        scw.add_with_viewport(column)
        return scw
