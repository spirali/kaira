#
#    Copyright (C) 2010 Stanislav Bohm
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
import nettools
import paths
import os
from canvas import NetCanvas
from drawing import VisualConfig
from objectlist import ObjectTree
from net import Net
import cairo
import gtkutils
import glib

action_cursor = {
    "none" : None,
    "move" : gtk.gdk.FLEUR,
    "resize_rbottom" : gtk.gdk.BOTTOM_RIGHT_CORNER,
    "resize_lbottom" : gtk.gdk.BOTTOM_LEFT_CORNER,
    "resize_rtop" : gtk.gdk.TOP_RIGHT_CORNER,
    "resize_ltop" : gtk.gdk.TOP_LEFT_CORNER,
    "select" : gtk.gdk.CROSSHAIR,
    "scroll" : gtk.gdk.HAND2,
}

def get_cursor(name):
    if name is None:
        return None
    stock = action_cursor[name]
    if stock:
        return gtk.gdk.Cursor(stock)
    else:
        return None

def netname_dialog(net, mainwindow):
    builder = gtkutils.load_ui("netname-dialog")
    dlg = builder.get_object("netname-dialog")
    try:
        name = builder.get_object("name")
        name.set_text(net.get_name())
        name.select_region(0, -1)
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            net.set_name(name.get_text())
            return True
        return False
    finally:
        dlg.destroy()

class NetViewVisualConfig(VisualConfig):

    def __init__(self, project):
        self.selected = None
        self.project = project
        self.mouseover_highlighted = None
        self.show_tracing = False

    def highlight(self, item):
        self.selected = item

    def highlight_off(self):
        self.selected = None

    def set_mouseover_highlight(self, item):
        """ Return True if need redraw """
        if item == self.mouseover_highlighted:
            return False
        self.mouseover_highlighted = item
        return True

    def preprocess(self, item, drawing):
        if self.show_tracing and (item.is_transition() or item.is_place()):
            drawing.trace_text = item.get_trace_text()
        if item == self.mouseover_highlighted:
            drawing.set_highlight((0.6,0.6,0.8,8.0))
        if self.project.has_error_messages(item):
            drawing.set_highlight((1.0, 0.0, 0.0, 0.5))
            drawing.set_error_messages(self.project.get_error_messages(item))
        if item == self.selected:
            drawing.set_highlight((0.86,0.86,0.0,1.0))
        return drawing

class NetView(gtk.VBox):

    def __init__(self, app, project):
        gtk.VBox.__init__(self)
        self.project = project
        self.app = app
        self.tool = None
        self.entry_types = []
        self.set_size_request(500,400)


        self.pack_start(self._controls(), False)
        self.pack_start(self._editarea(), False)

        paned = gtk.HPaned()
        self.pack_start(paned)

        self.netlist = NetList(project, self)
        self.netlist.set_size_request(100, 100)
        paned.pack1(self.netlist, False, True)
        self.canvas = self._net_canvas()
        paned.pack2(self.canvas, True, True)
        paned.show_all()

        self.netlist.hide()

        self.transition_edit_callback = None
        self.place_edit_callback = None
        self.set_tool(nettools.SelectTool(self))

    def get_grid_size(self):
        return self.app.get_grid_size()

    def set_tool(self, tool):
        if self.tool:
            self.tool.stop()
        self.tool = tool

        if tool:
            tool.start()
            self.focus_entry()

    def get_net(self):
        return self.netlist.selected_object()

    def set_show_tracing(self, value):
        self.vconfig.show_tracing = value
        self.redraw()

    def switch_to_net(self, net, select_in_netlist = True):
        if select_in_netlist:
            self.netlist.select_object(net)
        self.tool.set_net(net)
        self.canvas.set_net(net)

    def get_zoom(self):
        return self.canvas.get_zoom()

    def focus_entry(self):
        self.entry.grab_focus()

    def set_viewport(self, viewport):
        self.canvas.set_viewport(viewport)

    def get_viewport(self):
        return self.canvas.get_viewport()

    def redraw(self):
        self.canvas.redraw()

    def set_cursor(self,action_name):
        if self.canvas.window:
            self.canvas.window.set_cursor(get_cursor(action_name))

    def net_changed(self):
        self.redraw()
        if self.tool:
            self.tool.net_changed()

    def highlight(self, item):
        self.vconfig.highlight(item)
        self.redraw()

    def highlight_off(self):
        self.vconfig.highlight_off()
        self.redraw()

    def mouseover_highlight(self, item):
        if self.vconfig.set_mouseover_highlight(item):
            self.redraw()

    def mouseover_highlight_off(self):
        if self.vconfig.set_mouseover_highlight(None):
            self.redraw()

    def netlist_show(self, v):
        if v:
            self.netlist.show()
        else:
            self.netlist.hide()

    def _controls(self):
        icon_arrow = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "arrow.png"))
        icon_transition = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "transition.png"))
        icon_place = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "place.png"))
        icon_arc = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "arc.png"))
        icon_area = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "area.png"))
        icon_trace = gtk.image_new_from_file(os.path.join(paths.ICONS_DIR, "trace.png"))

        toolbar = gtk.Toolbar()

        button1 = gtk.ToggleToolButton(None)
        button1.connect("toggled", lambda w: self.netlist_show(w.get_active()))
        button1.set_stock_id(gtk.STOCK_INDEX)
        toolbar.add(button1)

        button1 = gtk.ToggleToolButton(None)
        button1.connect("toggled", lambda w: self.set_show_tracing(w.get_active()))
        button1.set_icon_widget(icon_trace)
        toolbar.add(button1)

        toolbar.add(gtk.SeparatorToolItem())

        button1 = gtk.RadioToolButton(None,None)
        button1.connect("toggled", lambda w: self.set_tool(nettools.SelectTool(self)))
        button1.set_icon_widget(icon_arrow)

        button2 = gtk.RadioToolButton(button1,None)
        button2.connect("toggled", lambda w: self.set_tool(nettools.TransitionTool(self)))
        button2.set_icon_widget(icon_transition)

        button3 = gtk.RadioToolButton(button1,None)
        button3.connect("toggled", lambda w: self.set_tool(nettools.PlaceTool(self)))
        button3.set_icon_widget(icon_place)

        button4 = gtk.RadioToolButton(button1,None)
        button4.connect("toggled", lambda w: self.set_tool(nettools.EdgeTool(self)))
        button4.set_icon_widget(icon_arc)

        button5 = gtk.RadioToolButton(button1,None)
        button5.connect("toggled", lambda w: self.set_tool(nettools.AreaTool(self)))
        button5.set_icon_widget(icon_area)

        toolbar.add(button1)
        toolbar.add(button2)
        toolbar.add(button3)
        toolbar.add(button4)
        toolbar.add(button5)

        toolbar.add(gtk.SeparatorToolItem())

        button1 = gtk.ToolButton()
        button1.connect("clicked", lambda w: self.canvas.zoom_in())
        button1.set_stock_id(gtk.STOCK_ZOOM_IN)

        button2 = gtk.ToolButton()
        button2.connect("clicked", lambda w: self.canvas.zoom_out())
        button2.set_stock_id(gtk.STOCK_ZOOM_OUT)

        toolbar.add(button1)
        toolbar.add(button2)

        vbox = gtk.VBox()
        vbox.pack_start(toolbar)
        vbox.show_all()

        return vbox

    def _net_canvas(self):
        self.vconfig = NetViewVisualConfig(self.project)
        c = NetCanvas(self.get_net(), self._draw, self.vconfig)
        c.set_callback("button_down", self._button_down)
        c.set_callback("button_up", self._button_up)
        c.set_callback("mouse_move", self._mouse_move)
        c.show()
        return c

    def _draw(self, cr, w, h):
        if self.tool:
            self.tool.draw(cr)

    def _editarea(self):
        vbox = gtk.VBox()
        self.entry = gtk.Entry()
        self.entry.connect("changed", self._entry_changed)

        self.entry_switch = gtk.combo_box_new_text()
        self.entry_switch.append_text("Inscription")
        self.entry_switch.connect("changed", self._entry_switch_changed)

        vbox = gtk.HBox()
        vbox.pack_start(self.entry_switch, False, False)
        vbox.pack_start(self.entry, True, True)

        vbox.show_all()
        return vbox


    def _button_down(self, event, position):
        if self.tool and self.tool.net:
            if event.button == 1:
                self.tool.left_button_down(event, position)
            elif event.button == 3:
                self.tool.right_button_down(event, position)

    def _button_up(self, event, position):
        if self.tool and self.tool.net:
            if event.button == 1:
                self.tool.left_button_up(event, position)
            elif event.button == 3:
                self.tool.right_button_up(event, position)

    def _mouse_move(self, event, position):
        if self.tool and self.tool.net:
            self.tool.mouse_move(event, position)

    def set_entry_types(self, etypes):
        self.entry_types = etypes
        names = [ x[0] for x in etypes ]
        self.entry_switch.get_model().clear()
        for name in names:
            self.entry_switch.append_text(name)
        self.entry.set_sensitive(bool(names))

        if names:
            self.entry_switch.set_active(0)
            name, get, set = self.active_entry_type()
            self.entry.set_text(get())
            self.focus_entry()

    def active_entry_type(self):
        text = self.entry_switch.get_active_text()
        for e in self.entry_types:
            if e[0] == text:
                return e

    def _entry_changed(self, w):
        if self.entry_types and self.entry_switch.get_active_text():
            name, get, set = self.active_entry_type()
            set(self.entry.get_text())

    def _entry_switch_changed(self, w):
        if self.entry_types and self.entry_switch.get_active_text():
            name, get, set = self.active_entry_type()
            self.entry.set_text(get())

class NetList(ObjectTree):

    def __init__(self, project, netview):
        defs = [("_", object), ("Network|markup", str)]
        ObjectTree.__init__(self, defs, has_context_menu = True)
        self.hide_headers()
        self.project = project
        self.netview = netview
        self.setup()
        self.select_first()
        project.set_callback("netlist_changed", self.setup)

    def get_context_menu(self):
        obj = self.selected_object()
        menu = [ ("Add", [ ("Module", self._add_module), ("Test", self._add_test) ]) ]

        if isinstance(obj, str):
            return menu

        menu += [
            ("-", None),
            ("Copy net", self._copy),
            ("Rename net", self._rename),
            ("-", None),
            ("Tracing", [
                ("Trace everything", self._trace_everything),
                ("Trace nothing", self._trace_nothing) ]),
            ("-", None),
            ("Export to SVG", self._export_svg)
        ]

        if not obj.is_module():
            menu.append(("-", None))
            menu.append(("Select for simulations", self._set_simulator_net))

        return menu

    def setup(self):
        objs = []
        if self.project.get_main_net():
            objs.append(self.project.get_main_net())
        objs += self.project.get_modules()

        tests = self.project.get_tests()
        if tests:
            objs.append(("Tests", tests))
        self.refresh(objs)

    def _add_module(self, w):
        net = Net(self.project, "module", "Net_{0}".format(self.project.new_id()))
        net.add_interface_box((20, 20), (400, 300))
        self._add_net(net)

    def _add_test(self, w):
        net = Net(self.project, "test", "Net_{0}".format(self.project.new_id()))
        if self.project.get_simulator_net() is None:
            self.project.set_simulator_net(net)
        self._add_net(net)

    def _add_net(self, net):
        if netname_dialog(net, self.netview.app.window):
            self.project.add_net(net)
            self.netview.switch_to_net(net)

    def _set_simulator_net(self, w):
         obj = self.selected_object()
         self.project.set_simulator_net(obj)

    def _remove(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        if obj.is_main():
            self.netview.app.show_info_dialog("Net 'Main' cannot be removed.")
        else:
            self.project.remove_net(obj)
            net = self.project.get_nets()[0]
            self.netview.switch_to_net(net)

    def _rename(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        if obj.is_main():
            self.netview.app.show_info_dialog("Net 'Main' cannot be renamed.")
        else:
            netname_dialog(obj, self.netview.app.window)
            self.update(obj)

    def _copy(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        net = obj.copy()
        net.name = obj.name + "_copy"
        self.project.add_net(net)

    def _export_svg(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        surface = cairo.SVGSurface("net.svg", 1000, 1000)
        try:
            context = cairo.Context(surface)
            obj.draw(context, VisualConfig())
        finally:
            surface.finish()
        self.netview.app.console_write("Net exported to 'net.svg'.\n", "success")

    def _trace_nothing(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        obj.trace_nothing()
        self.netview.redraw()

    def _trace_everything(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        obj.trace_everything()
        self.netview.redraw()

    def object_as_row(self, obj):
        if isinstance(obj, str):
            return (obj, obj)
        else:
            name = glib.markup_escape_text(obj.get_name())
            if obj.is_simulator_net():
                return (obj, "<b>{0}</b>".format(name))
            else:
                return (obj, name)

    def cursor_changed(self, obj):
        if not isinstance(obj, str):
            self.netview.switch_to_net(obj, False)
        else:
            self.netview.switch_to_net(None, False)
