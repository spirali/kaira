#
#    Copyright (C) 2010-2013 Stanislav Bohm
#                  2012 Martin Kozubek
#                  2012 Lukas Tomaszek
#                  2014 Jan Homola
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
import gtkutils
import glib
import neteditcc
import undo
import codeedit
import objectlist
import tracing

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


    def __init__(self, name, initial_value, set_fn):
        self.name = name
        self.initial_value = initial_value
        self.set_fn = set_fn


class NetEditor(gtk.VBox):

    def __init__(self, app, project):
        gtk.VBox.__init__(self)
        self.project = project
        self.app = app
        self.net = None
        self.undo_manager = None
        self.set_size_request(500,400)
        self.canvas = None
        self.attribute_widgets = []
        self.attribute_item = None

        self.mode = "edit"

        self.pack_start(self._controls(), False)

        paned = gtk.HPaned()
        self.pack_start(paned)

        self.netlist = NetList(project, self)

        vpaned = gtk.VPaned()
        vpaned.pack1(self._attribute_box(), True, True)
        vpaned.pack2(self.netlist, False, False)
        paned.pack1(vpaned, False, False)
        self.canvas = Canvas(None)
        paned.pack2(self.canvas, True, True)
        paned.show_all()

        self._setup_no_attribute("Nothing selected")

        self.transition_edit_callback = None
        self.place_edit_callback = None
        self.set_tool("selection")
        self.switch_to_net(self.get_net(), False)
        self.on_undomanager_changed()

        self.project.set_callback("changed", self._project_changed)

    def get_net(self):
        return self.netlist.selected_object()

    def set_mode(self, value):
        self.mode = value
        self.canvas.config.configure()
        self.redraw()

    def switch_to_net(self, net, select_in_netlist = True):
        self.net = net
        if self.canvas: # Bootstrap problem
            self.canvas.config.set_net(net, None)
        if net is None:
            self.undo_manager = None
            return

        def changed(net, item):
            net.project.generator = None
            self.redraw()

        net.change_item_callback = changed
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

    def focus_edit_item(self, index=0):
        self.attribute_widgets[0].grab_focus()

    def set_viewport(self, viewport):
        self.canvas.set_viewport(viewport)

    def get_viewport(self):
        return self.canvas.get_viewport()

    def redraw(self):
        self.canvas.redraw()

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
        icon_simrun = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "simrun.svg"))
        icon_verif = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "verif.svg"))

        toolbar = gtk.Toolbar()

        button1 = gtk.RadioToolButton(None)
        button1.connect("toggled", lambda w: self.set_mode("edit"))
        button1.set_stock_id(gtk.STOCK_EDIT)
        toolbar.add(button1)

        button2 = gtk.RadioToolButton(button1, None)
        button2.connect("toggled", lambda w: self.set_mode("tracing"))
        button2.set_icon_widget(icon_trace)
        toolbar.add(button2)

        button2 = gtk.RadioToolButton(button1, None)
        button2.connect("toggled", lambda w: self.set_mode("simrun"))
        button2.set_icon_widget(icon_simrun)
        toolbar.add(button2)

        button2 = gtk.RadioToolButton(button1, None)
        button2.connect("toggled", lambda w: self.set_mode("verif"))
        button2.set_icon_widget(icon_verif)
        toolbar.add(button2)
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

    def _attribute_box(self):
        box = gtk.VBox(False)
        box.set_size_request(200, 0)
        self.attribute_box = box
        return box

    def _key_press(self, w, event):
        if gtk.gdk.keyval_name(event.keyval) == "Tab":
            if hasattr(w, "code_complete"):
                if w.code_complete.active_place_holder:
                    return False
            self.focus_next_attribute()
            return True

    def focus_next_attribute(self):
        for i, widget in enumerate(self.attribute_widgets):
            if widget.has_focus():
                self.attribute_widgets[(i + 1) % len(self.attribute_widgets)].grab_focus()
                return
        if self.attribute_widgets:
            self.attribute_widgets[0].grab_focus()

    def set_attributes(self, item):
        focus = self.attribute_item is not item
        self.attribute_item = item
        if not self.attribute_widgets and item is None:
            return

        for widget in self.attribute_box.get_children():
            self.attribute_box.remove(widget)
        self.attribute_widgets = []

        if item is not None:
            if self.mode == "edit":
                self._setup_attribute_widges_mode_edit(item)
            elif self.mode == "tracing":
                self._setup_attribute_widges_mode_tracing(item)
            elif self.mode == "simrun":
                self._setup_attribute_widges_mode_simrun(item)
            elif self.mode == "verif":
                self._setup_attribute_widges_mode_verif(item)

        if self.attribute_widgets:
            if focus:
                self.focus_edit_item()
        else:
            if item is not None:
                text = "Nothing to edit in this mode"
            else:
                text = "Nothing selected"
            self._setup_no_attribute(text)

        self.attribute_box.show_all()

    def _setup_no_attribute(self, text):
        editor = codeedit.CodeEditor(self.app, self.project.get_syntax_highlight_key())
        editor.set_size_request(0, 60)
        editor.set_sensitive(False)
        label = gtk.Label(text)
        label.set_sensitive(False)
        self.attribute_box.pack_start(label, False, False)
        self.attribute_box.pack_start(editor, False, False)
        self.attribute_box.show_all()

    def _setup_attribute_widges_mode_edit(self, item):
        if item.is_transition():
            def set_collective(value):
                if value and item.has_code():
                    self.app.console_write("This transition contains inner code.\n"
                                           "A code in a transition is ignored "
                                           "when the transition is collective.\n", "warn")
                item.set_collective(value)

            self._add_attribute_labelled_code_editor("Name", item.get_name, item.set_name)
            self._add_attribute_labelled_code_editor("Guard", item.get_guard, item.set_guard)
            self._add_attribute_labelled_code_editor("Priority",
                                                     item.get_priority,
                                                     item.set_priority)
            self._add_attribute_checkbox("Collective communication",
                                         item.is_collective(),
                                         set_collective)
            root_editor = self._add_attribute_labelled_code_editor("Root",
                                                                   item.get_root,
                                                                   item.set_root)
            root_editor.set_sensitive(item.is_collective())
            self._add_attribute_checkbox("Clock", item.has_clock(), item.set_clock)
        elif item.is_place():
            self._add_attribute_labelled_code_editor("Name", item.get_name, item.set_name)
            self._add_attribute_labelled_code_editor("Type",
                                                     item.get_place_type,
                                                     item.set_place_type)
            self._add_attribute_labelled_code_editor("Init",
                                                     item.get_init_string,
                                                     item.set_init_string)
            self._add_attribute_checkbox_entry(
                "Input", item.get_interface_in, item.set_interface_in)
            self._add_attribute_checkbox_entry(
                "Output", item.get_interface_out, item.set_interface_out)

        elif item.is_edge():
            self._add_attribute_labelled_code_editor("Inscription",
                                            item.get_inscription,
                                            item.set_inscription)
        elif item.is_area():
            self._add_attribute_labelled_code_editor("Init",
                                                     item.get_init_expr,
                                                     item.set_init_expr)

    def _setup_attribute_widges_mode_tracing(self, item):
        def trace_fire(value):
            item.trace_fire = value
        if item.is_transition():
            self._add_attribute_checkbox("Trace fire",
                                         item.trace_fire,
                                         set_fn=trace_fire)
        elif item.is_place():
            def refresh():
                objlist.refresh(item.trace_tokens_functions)
                self.canvas.config.configure()
            def trace_tokens_fn(w):
                item.trace_tokens = w.get_active()
                refresh()
            def add_fn():
                trace_fn = tracing.TraceFunction("", "int")
                result = tracing.tracefn_dialog(self.app.window, trace_fn)
                if result:
                    item.trace_tokens_functions.append(trace_fn)
                    refresh()
            def edit_fn(obj):
                tracing.tracefn_dialog(self.app.window, obj)
                refresh()
            def remove_fn(obj):
                item.trace_tokens_functions.remove(obj)
                refresh()
            def add_token_name():
                trace_fn = tracing.TraceFunction("ca::token_name", "std::string")
                item.trace_tokens_functions.append(trace_fn)
                refresh()

            objlist = objectlist.ObjectList([("_", object), ("Name", str), ("Type", str)])
            objlist.set_size_request(0, 150)
            objlist.object_as_row = lambda obj: [ obj, obj.name, obj.return_type ]
            objlist.fill(item.trace_tokens_functions)

            button = gtk.CheckButton("Trace tokens")
            button.set_active(item.trace_tokens)
            button.connect("toggled", trace_tokens_fn)
            self.attribute_box.pack_start(button, False, False)
            self.attribute_box.pack_start(gtk.HSeparator(), False, False, 5)

            widget = self._add_attribute_objlist("Trace functions",
                                                 objlist,
                                                 add_fn,
                                                 edit_fn,
                                                 remove_fn)
            button = gtk.Button("Trace ca::token_name")
            button.connect("clicked", lambda w: add_token_name())
            widget.pack_start(button, False, False)
            widget.set_sensitive(item.trace_tokens)
            self.attribute_box.pack_start(widget, False, False)

    def _setup_attribute_widges_mode_simrun(self, item):
        if item.is_transition():
            self._add_attribute_checkbox_code_editor(
                "Time substitution",
                item.get_time_substitution(),
                item.set_time_substitution,
                item.get_time_substitution_code,
                item.set_time_substitution_code)
            self._add_attribute_checkbox_code_editor(
                "Clock substitution",
                item.get_clock_substitution(),
                item.set_clock_substitution,
                item.get_clock_substitution_code,
                item.set_clock_substitution_code)
        elif item.is_edge():
            self._add_attribute_checkbox_code_editor(
                "Size substitution",
                item.get_size_substitution(),
                item.set_size_substitution,
                item.get_size_substitution_code,
                item.set_size_substitution_code)

    def _setup_attribute_widges_mode_verif(self, item):
        def set_calls_quit(value):
            item.calls_quit = value
        def set_compare_process(value):
            item.occurrence_analysis_compare_process = value
            self.canvas.config.configure()
        def set_compare_binding(value):
            item.occurrence_analysis_compare_binding = value
            self.canvas.config.configure()
        def set_occurrence_analysis(value):
            item.occurrence_analysis = value
            frame.set_sensitive(value)
            self.canvas.config.configure()
        if item.is_transition():
            self._add_attribute_checkbox("Transition calls quit",
                                         item.calls_quit,
                                         set_fn=set_calls_quit)
            self.attribute_box.pack_start(gtk.HSeparator(), False, False, 5)
            self._add_attribute_checkbox("Transition occurrence",
                                         item.occurrence_analysis,
                                         set_fn=set_occurrence_analysis)
            frame = gtk.Frame("Properties to compare")
            box = gtk.VBox()
            checkbox1 = gtk.CheckButton("process id", False)
            checkbox2 = gtk.CheckButton("binding", False)
            checkbox1.set_active(item.occurrence_analysis_compare_process)
            checkbox2.set_active(item.occurrence_analysis_compare_binding)
            checkbox1.connect("toggled", lambda w: set_compare_process(w.get_active()))
            checkbox2.connect("toggled", lambda w: set_compare_binding(w.get_active()))
            box.pack_start(checkbox1)
            box.pack_start(checkbox2)
            frame.add(box)
            frame.set_sensitive(item.occurrence_analysis)
            self.attribute_box.pack_start(frame, False, False)
        elif item.is_place():
            self._add_attribute_checkbox("Check in final markings",
                                         item.get_final_marking(),
                                         item.set_final_marking)

    def _add_attribute_objlist(self, text, objlist, add_fn, edit_fn, remove_fn):
        def remove(w):
            obj = objlist.selected_object()
            if (obj):
                remove_fn(obj)
        widget = gtk.VBox()
        label = gtk.Label(text)
        label.set_alignment(0.05, 0.5)
        widget.pack_start(label, False, False)
        box = gtk.HBox()
        widget.pack_start(box, False, False)
        button = gtk.Button("Add")
        button.connect("clicked", lambda w: add_fn())
        box.pack_start(button)
        button = gtk.Button("Edit")
        button.connect("clicked", lambda w: edit_fn(objlist.selected_object()))
        box.pack_start(button)
        button = gtk.Button("Remove")
        button.connect("clicked", remove)
        box.pack_start(button)

        widget.pack_start(objlist, False, False)
        self.attribute_widgets.append(objlist)
        return widget

    def _add_attribute_combobox(self,
                                values,
                                initial_value,
                                set_fn):
        def changed(w):
            set_fn(combobox.get_object())
            self.canvas.config.configure()
        combobox = gtkutils.SimpleComboBox(values)
        combobox.set_object(initial_value)
        combobox.connect("changed", changed)
        self.attribute_box.pack_start(combobox, False, False)
        self.attribute_widgets.append(combobox)

    def _add_attribute_checkbox(self,
                                text,
                                initial_value,
                                set_fn=None,
                                set_true=None,
                                set_false=None):
        def changed(w):
            if set_fn:
                set_fn(checkbox.get_active())
            if set_true and set_false:
                if checkbox.get_active():
                    set_true()
                else:
                    set_false()
            self.canvas.config.configure()
        checkbox = gtk.CheckButton(text, False)
        checkbox.set_active(initial_value)
        checkbox.connect("toggled", changed)
        self.attribute_box.pack_start(checkbox, False, False)
        self.attribute_widgets.append(checkbox)

    def _add_attribute_labelled_code_editor(self, name, get_fn, set_fn):
        label = gtk.Label(name)
        self.attribute_box.pack_start(label, False, False)
        editor = self._add_attribute_code_editor(get_fn, set_fn)
        if label.get_text() == "Type":
            import completion
            editor.view.code_complete = completion.Completion(editor)
            editor.view.set_show_line_numbers(False)
            editor.view.code_complete.clang.set_type("")
        else:
            return editor
        #return self._add_attribute_code_editor(get_fn, set_fn)

    def _add_attribute_checkbox_code_editor(
        self, name, bool_value, set_bool_fn, get_code_fn, set_code_fn):
        def changed(value):
            editor.set_sensitive(value)
            set_bool_fn(value)
        self._add_attribute_checkbox(name, bool_value, set_fn=changed)
        editor = self._add_attribute_code_editor(get_code_fn, set_code_fn)
        editor.set_sensitive(bool_value)


    def _add_attribute_checkbox_entry(self, name, get_fn, set_fn):
        def changed(value):
            entry.set_sensitive(value)
            if value:
                set_fn("")
            else:
                set_fn(None)
        self._add_attribute_checkbox(name, get_fn() is not None, set_fn=changed)
        entry = self._add_attribute_entry(get_fn, set_fn)
        entry.set_sensitive(get_fn() is not None)

    def _add_attribute_entry(self, get_fn, set_fn):
        def changed(w):
            set_fn(w.get_text())
        entry = gtk.Entry()
        value = get_fn()
        if value is None:
            entry.set_text("")
        else:
            entry.set_text(value)
        entry.connect("changed", changed) #lambda w: set_fn(entry.get_text()))
        self.attribute_box.pack_start(entry, False, False, 0)
        self.attribute_widgets.append(entry)
        return entry

    def _add_attribute_code_editor(self, get_fn, set_fn):
        def on_change():
            original = get_fn()
            text = editor.get_text()
            set_fn(text)
            self.add_undo_action(undo.ActionSet(get_fn,
                                                set_fn,
                                                original,
                                                suppress_similar=True))
        editor = codeedit.CodeEditor(self.app, self.project.get_syntax_highlight_key())

        # Brackets are highlighted even when textview has no focus, it is quite disturbing
        # So we turn this feature off
        editor.buffer.set_highlight_matching_brackets(False)

        editor.view.set_events(gtk.gdk.KEY_PRESS)
        editor.view.connect("key_press_event", self._key_press)
        editor.set_size_request(0, 60)
        editor.set_text(get_fn())
        editor.buffer_changed = on_change
        self.attribute_box.pack_start(editor, False, False, 0)
        self.attribute_widgets.append(editor)
        return editor

    def _project_changed(self, obj):
        if obj != self.net and obj != "error_messages":
            return
        self.canvas.config.configure()
        self.redraw()


class NetList(ObjectTree):

    def __init__(self, project, neteditor):
        defs = [("_", object), ("Nets|markup", str)]
        ObjectTree.__init__(self, defs, has_context_menu=True)
        self.set_size_request(0, 80)
        self.project = project
        self.neteditor = neteditor
        self.setup()
        self.select_first()
        project.set_callback("netlist_changed", self.setup)

    def get_context_menu(self):
        obj = self.selected_object()
        menu = [ ("Add net", lambda w: self._add_net()) ]
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
        menu.append(("Select for build", self._set_build_net))
        return menu

    def setup(self):
        self.refresh(self.project.nets)

    def _add_net(self):
        net = Net(self.project, "Net_{0}".format(self.project.new_id()))
        if netname_dialog(net, self.neteditor.app.window):
            self.project.add_net(net)
            self.neteditor.switch_to_net(net)

    def _set_build_net(self, w):
        obj = self.selected_object()
        self.project.set_build_net(obj)

    def _remove(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
        if len(self.project.nets) == 1:
            self.neteditor.app.show_error_dialog("Net cannot be removed.\n"
                                                 "Project has to have at least one net.")
        else:
            self.project.remove_net(obj)
            net = self.project.get_nets()[0]
            self.neteditor.switch_to_net(net)

    def _rename(self, w):
        obj = self.selected_object()
        if isinstance(obj, str):
            return
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
            if obj.is_build_net():
                return (obj, "<b>{0}</b>".format(name))
            else:
                return (obj, name)

    def cursor_changed(self, obj):
        if not isinstance(obj, str):
            self.neteditor.switch_to_net(obj, False)
        else:
            self.neteditor.switch_to_net(None, False)
