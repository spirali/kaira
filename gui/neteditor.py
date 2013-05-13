#
#    Copyright (C) 2010-2013 Stanislav Bohm
#                  2012 Martin Kozubek
#                  2012 Lukas Tomaszek
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
import paths
import os
from canvas import Canvas
from objectlist import ObjectTree
from net import Net
import cairo
import gtkutils
import glib
import neteditcc
import undo

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


class NetEditor(gtk.VBox):

    def __init__(self, app, project):
        gtk.VBox.__init__(self)
        self.project = project
        self.app = app
        self.net = None
        self.last_active_entry_text = None
        self.last_active_entry_get = None
        self.last_active_entry_set = None
        self.entry_types = []
        self.undo_manager = None
        self.set_size_request(500,400)
        self.canvas = None

        self.show_tracing = False

        self.pack_start(self._controls(), False)
        self.pack_start(self._editarea(), False)

        paned = gtk.HPaned()
        self.pack_start(paned)

        self.netlist = NetList(project, self)
        self.netlist.set_size_request(100, 100)
        paned.pack1(self.netlist, False, True)
        self.canvas = Canvas(None)
        paned.pack2(self.canvas, True, True)
        paned.show_all()

        self.netlist.hide()
        self.transition_edit_callback = None
        self.place_edit_callback = None
        self.set_tool("selection")
        self.connect("key_press_event", self._key_press)
        self.switch_to_net(self.get_net(), False)
        self.on_undomanager_changed()

    def get_net(self):
        return self.netlist.selected_object()

    def set_show_tracing(self, value):
        self.show_tracing = value
        self.canvas.config.configure()
        self.redraw()

    def switch_to_net(self, net, select_in_netlist = True):
        self.net = net
        if self.canvas: # Bootstrap problem
            self.canvas.config.set_net(net, None)
        if net is None:
            self.undo_manager = None
            return

        net.change_item_callback = lambda n, i: self.redraw()
        if select_in_netlist:
            self.netlist.select_object(net)
        self.undo_manager = net.undo_manager

    def set_tool(self, name, set_button=False):
        if name == "selection":
            self.canvas.set_config(neteditcc.SelectionCanvasConfig(self))
            if set_button:
                self.button_selection.set_active(True)
        elif name == "transition":
            self.canvas.set_config(neteditcc.NewTransitionCanvasConfig(self))
        elif name == "place":
            self.canvas.set_config(neteditcc.NewPlaceCanvasConfig(self))
        elif name == "edge":
            self.canvas.set_config(neteditcc.NewEdgeCanvasConfig(self))
        elif name == "area":
            self.canvas.set_config(neteditcc.NewAreaCanvasConfig(self))
        else:
            raise Exception("Invalid tool")
        self.canvas.config.set_net(self.net, self.canvas.viewport)

    def add_undo_action(self, action):
        self.undo_manager.add_action(action)
        self.on_undomanager_changed()

    def on_undomanager_changed(self):
        if self.undo_manager is None:
            self.button_undo.set_sensitive(False)
            self.button_redo.set_sensitive(False)
        else:
            self.button_undo.set_sensitive(self.undo_manager.has_undo())
            self.button_redo.set_sensitive(self.undo_manager.has_redo())

    def save_as_svg(self, filename):
        self.canvas.save_as_svg(filename)

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

    def net_changed(self):
        self.canvas.config.configure()
        self.redraw()

    def netlist_show(self, v):
        if v:
            self.netlist.show()
        else:
            self.netlist.hide()

    def undo(self):
        if self.undo_manager.has_undo():
            self.undo_manager.perform_undo()
            self.on_undomanager_changed()
            self.net.changed()

    def redo(self):
        if self.undo_manager.has_redo():
            self.undo_manager.perform_redo()
            self.on_undomanager_changed()
            self.net.changed()

    def _controls(self):
        icon_arrow = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "arrow.svg"))
        icon_transition = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "transition.svg"))
        icon_place = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "place.svg"))
        icon_arc = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "arc.svg"))
        icon_area = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "area.svg"))
        icon_trace = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "trace.svg"))

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

        self.button_undo = gtk.ToolButton()
        self.button_undo.connect("clicked", lambda w: self.undo())
        self.button_undo.set_stock_id(gtk.STOCK_UNDO)

        self.button_redo = gtk.ToolButton()
        self.button_redo.connect("clicked", lambda w: self.redo())
        self.button_redo.set_stock_id(gtk.STOCK_REDO)

        toolbar.add(self.button_undo)
        toolbar.add(self.button_redo)
        toolbar.add(gtk.SeparatorToolItem())

        button1 = gtk.RadioToolButton(None,None)
        button1.connect("toggled", lambda w: self.set_tool("selection"))
        button1.set_icon_widget(icon_arrow)
        self.button_selection = button1

        button2 = gtk.RadioToolButton(button1,None)
        button2.connect("toggled", lambda w: self.set_tool("transition"))
        button2.set_icon_widget(icon_transition)

        button3 = gtk.RadioToolButton(button1,None)
        button3.connect("toggled", lambda w: self.set_tool("place"))
        button3.set_icon_widget(icon_place)

        button4 = gtk.RadioToolButton(button1,None)
        button4.connect("toggled", lambda w: self.set_tool("edge"))
        button4.set_icon_widget(icon_arc)

        button5 = gtk.RadioToolButton(button1,None)
        button5.connect("toggled", lambda w: self.set_tool("area"))
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

    def _key_press(self, w, event):
        if event.state & gtk.gdk.CONTROL_MASK and \
               gtk.gdk.keyval_name(event.keyval) == "space" and \
               self.entry_types:
            self.set_next_entry_type()

    def set_next_entry_type(self):
        current = self.entry_switch.get_active()
        self.entry_switch.set_active((current + 1) % len(self.entry_types))

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
            text = self.entry.get_text()
            original = get()
            if text == original:
                return
            self.add_undo_action(undo.ActionSet(get,
                                                set,
                                                original,
                                                suppress_similar=True))
            set(text)

    def _entry_switch_changed(self, w):
        if self.entry_types and self.entry_switch.get_active_text():
            name, get, set = self.active_entry_type()
            self.entry.set_text(get())
            self.entry.select_region(0, -1)


class NetList(ObjectTree):

    def __init__(self, project, neteditor):
        defs = [("_", object), ("Network|markup", str)]
        ObjectTree.__init__(self, defs, has_context_menu=True)
        self.hide_headers()
        self.project = project
        self.neteditor = neteditor
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
            ("Remove net", self._remove),
            ("-", None),
            ("Tracing", [
                ("Trace everything", self._trace_everything),
                ("Trace nothing", self._trace_nothing) ]),
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
        if netname_dialog(net, self.neteditor.app.window):
            self.project.add_net(net)
            self.neteditor.switch_to_net(net)

    def _set_simulator_net(self, w):
         obj = self.selected_object()
         self.project.set_simulator_net(obj)

    def _remove(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        if obj.is_main():
            self.neteditor.app.show_info_dialog("Net 'Main' cannot be removed.")
        else:
            self.project.remove_net(obj)
            net = self.project.get_nets()[0]
            self.neteditor.switch_to_net(net)

    def _rename(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        if obj.is_main():
            self.neteditor.app.show_info_dialog("Net 'Main' cannot be renamed.")
        else:
            netname_dialog(obj, self.neteditor.app.window)
            self.update(obj)

    def _copy(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        net = obj.copy()
        net.name = obj.name + "_copy"
        self.project.add_net(net)

    def _trace_nothing(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        obj.trace_nothing()

    def _trace_everything(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        obj.trace_everything()

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
            self.neteditor.switch_to_net(obj, False)
        else:
            self.neteditor.switch_to_net(None, False)
