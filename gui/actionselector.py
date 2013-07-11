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

import re
import os
import sys
import imp

from events import EventSource
from stypes import repository as types_repo

KAIRA_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PLUGIN_DIR = os.path.join(KAIRA_DIR, "plugins")

plugins = {}

class Source:

    def __init__(self, name, type, data=None):
        """ A source of data.

        Arguments:
        name -- file name (source file on disk)
        type -- type of the data (stype.Type)
        data -- if data are in memory the source could be created on fly,
                default value is None

        """

        self.name = name
        self.type = type # Category of source
        self.data = data

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

class ViewSource(gtk.Alignment, EventSource):

    def __init__(self, source):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.set_padding(5, 0, 10, 10)

        self.source = source

        self.w_name = gtk.Entry() # note: 'w' as a widget
        self.w_name.set_size_request(40, -1)
        self.w_name.set_editable(False)

        self.lbl_type = gtk.Label()
        self.lbl_type.set_alignment(0, 0)
        self.w_name.set_text(source.get_name())
        self.lbl_type.set_markup(
            "<i>{0}</i>".format(source.type.get_name()))

        btn_attach = gtk.Button("Attach")
        btn_attach.connect(
            "clicked", lambda w: self.emit_event("attach-source", self.source))

        # source menu
        menu = gtk.Menu()
        menu_show = gtk.MenuItem("Show")
        menu.append(menu_show)
        menu.append(gtk.SeparatorMenuItem())

        menu_store = gtk.MenuItem("Store")
        menu.append(menu_store)
        menu_load = gtk.MenuItem("Load")
        menu_load.set_sensitive(False)
        menu.append(menu_load)
        menu.append(gtk.SeparatorMenuItem())

        menu_del = gtk.MenuItem("Delete")
        menu.append(menu_del)

        src_view_menu = gtk.MenuItem(">")
        src_view_menu.set_submenu(menu)

        menu_bar = gtk.MenuBar()
        menu_bar.set_child_pack_direction(gtk.PACK_DIRECTION_TTB)
        menu_bar.append(src_view_menu)

        # source component
        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        table = gtk.Table(2, 3, False)
        table.set_border_width(2)
        table.set_col_spacing(0, 10)
        table.set_col_spacing(1, 2)

        table.attach(self.w_name,   0, 1, 0, 1)
        table.attach(self.lbl_type, 0, 1, 1, 2)
        table.attach(btn_attach,    1, 2, 0, 2, xoptions=gtk.FILL)
        table.attach(menu_bar,      2, 3, 0, 2, xoptions=0)

        frame.add(table)
        self.add(frame)

    def get_source(self):
        return self.source

class Parameter(EventSource):

    def __init__(self, name, type, array, required):
        EventSource.__init__(self)

        self.name = name
        self.type = type
        self.array = array
        self.required = required

        self.real_attached = 0
        self.sources = [None] * self.required

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

    def is_array(self):
        return self.array

    def get_required(self):
        return self.required

    def get_sources_count(self):
        return self.real_attached

    def is_empty(self):
        return self.real_attached == 0

    def get_source(self, index=-1):
        """ Returns a chosen source, default returns the last attached source.

        Arguments:
        index -- index of chosen source

        """
        if not self.sources or index >= len(self.sources):
            return None
        else:
            return self.sources[index]

    def attach_source(self, source, index=None):
        if index is None: # attach
            attached = False
            for i in range(0, len(self.sources)):
                if self.sources[i] is None:
                    self.sources[i] = source
                    attached = True
                    break
            if not attached:
                self.sources.append(source)

            self.real_attached += 1
        elif index >= 0:
            if index < len(self.sources):
                if self.sources[index] is None:
                    self.real_attached += 1
                self.sources[index] = source
            else:
                self.sources.append(source)
                self.real_attached += 1
        else: # no change
            return
        self.emit_event("parameter-changed")

    def detach_source(self, index):
        if index >= 0 and index < len(self.sources):
            if len(self.sources) - self.required < 0:
                self.sources[index] = None
            else:
                self.sources.pop(index)
            self.real_attached -= 1
            self.emit_event("parameter-changed")

class ParameterView(gtk.Table, EventSource):

    def __init__(self, parameter):
        gtk.Table.__init__(self, 1, 4, False)
        EventSource.__init__(self)

        self.set_border_width(2)

        self.parameter = parameter
        self.parameter.set_callback(
            "parameter-changed", self._actualize)

        self._actualize()

    def _actualize(self):
        # remove
        for child in self.get_children():
            self.remove(child)

        # create new view
        rows = self.parameter.get_sources_count() + 1
        columns = 4
        self.resize(rows, columns)

        lbl_name = gtk.Label()
        lbl_name.set_alignment(0, 0.5)
        lbl_name.set_markup("<b>{0}</b>".format(self.parameter.get_name()))
        self.attach(lbl_name, 0, 1, 0, 1, xoptions=0)

        lbl_type = gtk.Label()
        lbl_type.set_alignment(0, 0.5)
        lbl_type.set_markup(
            " ({0})".format(self.parameter.get_type().get_short_name()))
        self.attach(lbl_type, 1, 2, 0, 1)

        to = 1
        if self.parameter.is_array():
            to = self.parameter.get_sources_count() + 1
            if self.parameter.get_required() > self.parameter.get_sources_count():
                to = self.parameter.get_required()

        for i in range(0, to):
            entry = gtk.Entry()
            entry.set_editable(False)
            entry.connect("focus-in-event", self.choose_parameter, i)
            attached_source = self.parameter.get_source(i)
            if attached_source is not None:
                entry.set_text(attached_source.get_name())
            self.attach(entry, 2, 3, i, i+1, xoptions=gtk.FILL)

            btn_detach = gtk.Button("Detach")
            btn_detach.set_sensitive(attached_source is not None)
            btn_detach.connect(
                "clicked",
                lambda w, index: self.parameter.detach_source(index), i)

            self.attach(btn_detach, 3, 4, i, i+1, xoptions=0)

        self.show_all()

    def choose_parameter(self, widget, event, index):
        self.emit_event("filter-sources", self.parameter.get_type().get_id())
        self.emit_event("select-parameter", self.parameter.get_name(), index)

class Action(EventSource):

    """ Action is the template for user actions. It should not be used directly.
    """

    def __init__(self, name, description):
        EventSource.__init__(self)
        self.name = name
        self.description = description
        self.parameters = []
        self.state = "incomplete"

    def get_name(self):
        return self.name

    def get_description(self):
        return self.description

    def get_state(self):
        return self.state

    def set_state(self, state):
        if state == "ready" or state == "incomplete" or state == "incorrect":
            self.state = state
        else:
            raise Exception("Incorrect '{0}' icon state".format(state))

    def get_required_sources(self):
        return [(param.get_name(), param) for param in self.parameters]

    def get_processed_data(self): # interface
        """ Returns Source type. """
        return None

    def attach_source(self, source, name=None, index=None):
        for parameter in self.parameters:
            if name is not None and parameter.get_name() == name:
                parameter.attach_source(source, index)
                return

        for parameter in self.parameters:
            if source.get_type().compare(parameter.get_type()) and \
                    (parameter.is_empty() or parameter.is_array()):
                # it must be in another loop, because it is necessary
                # check all parameters by the name at first
                parameter.attach_source(source)
                return

        # not attached source
        self.emit_event("no-free-slot", source)

    def detach_source(self, name):
        for parameter in self.parameters:
            if parameter.get_name() == name:
                parameter.detach_source()

    def _add_parameter(self, name, type, list=False, required=1):
        """ Adds a new argument.

        Argumens:
        name -- Name of argument.
        type -- Type of argument (type).
        list -- True if the argument is represented as a list,
                 otherwise false.
        required -- The default value is 1 (one argument is almost always
                    required), but if it is an array, it determines a minimum
                    length of the array.

        """
        if not list and required > 1:
            raise Exception(
                "The parameter '{0}' is not a list, 'required' must be equal "
                "to one, not '{1}'!".format(name, required))

        self.parameters.append(Parameter(name, type, list, required))

    def run(self): # interface
        pass

class StateIcon(gtk.DrawingArea):

    def __init__(self, state, width=30, height=30):
        """
            Initialize of StateIcon.
            -- state - possible values: "ready", "incomplete", "incorrect"
        """
        self.icon_state = state
        gtk.DrawingArea.__init__(self)
        self.set_size_request(width, height)
        self.connect("expose_event", self._expose)

    def _expose(self, widget, event):
        cr = widget.window.cairo_create()
        rect = self.get_allocation()
        self._draw(cr, rect.width, rect.height)

    def _draw(self, cr, width, height):
        # clear background
        cr.set_source_rgb(0.95,0.95,0.95)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        # draw
        x = width / 2
        y = height / 2
        radius = min(width / 2, height / 2) - 5

        cr.arc(x, y, radius, 0, 2 * math.pi)
        if self.icon_state == "ready":
            cr.set_source_rgb(0, 0.8, 0)
        elif self.icon_state == "incomplete":
            cr.set_source_rgb(1, 0.4, 0)
        elif self.icon_state == "incorrect":
            cr.set_source_rgb(1, 0, 0)
        else:
            raise Exception(
                "Incorrect '{0}' icon state.".format(self.icon_state))

        cr.fill()

        radial = cairo.RadialGradient(
            x, height, 0,
            x, height, height-0.2*height)
        radial.add_color_stop_rgba(0, 0, 0, 0, 0.4)
        radial.add_color_stop_rgba(1, 0, 0, 0, 0.0)
        cr.set_source(radial)
        cr.arc(x, y, radius, 0, 2 * math.pi)
        cr.fill()

        cr.set_line_width(1)
        cr.arc(x, y, radius, 0, 2 * math.pi)
        cr.set_source_rgb(0, 0, 0)
        cr.stroke()

class ActionViewShort(gtk.Alignment, EventSource):

    def __init__(self, action):
        self.action = action

        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.set_padding(5, 0, 5, 5)

        hbox = gtk.HBox(False)

        # ready | incomplete | incorrect
        icon = StateIcon(action.get_state())
        hbox.pack_start(icon, False, False)

        lbl_name = gtk.Label()
        lbl_name.set_alignment(0, 0.5)
        lbl_name.set_padding(2, 0)
        lbl_name.set_markup("<b>{0}</b>".format(action.get_name()))
        hbox.pack_start(lbl_name, True, True)

        btn_choose = gtk.Button("Select")
        btn_choose.connect("clicked", self._select_action)
        hbox.pack_start(btn_choose, False, False)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        frame.add(hbox)
        self.add(frame)

    def _select_action(self, widget):
        self.emit_event("action-selected", self.action)

class ActionViewFull(gtk.VBox, EventSource):

    def __init__(self):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.params_views = {}
        self.action = None
        self.selected_parameter = (None, None)

    def select_parameter(self, param_name, index):
        self.selected_parameter = (param_name, index)

    def get_selected_parameter(self):
        return self.selected_parameter

    def get_action(self):
        return self.action

    def set_action(self, action):

        self.action = action
#        self.action.set_callback("source-attached", self._attach_source)
        self.action.set_callback("no-free-slot", self._no_free_slot)

        # remove old
        for comp in self.get_children():
            self.remove(comp)

        # generate
        self.set_border_width(5)
        # line with name and run button
        hbox = gtk.HBox(False)

        icon = StateIcon(action.get_state(), 25, 25)
        hbox.pack_start(icon, False, False)

        lbl_name = gtk.Label()
        lbl_name.set_alignment(0, 1)
        lbl_name.set_markup("<b>{0}</b>".format(action.get_name()))
        halign = gtk.Alignment(0, 0.5, 0, 0)
        halign.set_padding(0, 0, 2, 0)
        halign.add(lbl_name)
        hbox.pack_start(halign, True, True)

        self.btn_run = gtk.Button("Run")
        self.btn_run.set_sensitive(False)
        self.btn_run.hide()
        hbox.pack_start(self.btn_run, False, False)

        self.pack_start(hbox, False, False)

        # description
        def cb_allocate(label, allocation ):
            label.set_size_request(allocation.width - 2, -1)

        align = gtk.Alignment(0, 0, 1, 1)
        align.set_padding(0, 5, 5, 5)

        self.dsc_frame = gtk.Frame()
        self.dsc_frame.set_label("Description")
        self.lbl_description = gtk.Label()
        self.lbl_description.set_alignment(0, 1)
        self.lbl_description.set_line_wrap(True)
        self.lbl_description.set_markup("<i>{0}</i>".format(action.description))
        self.lbl_description.connect( "size-allocate", cb_allocate)
        self.dsc_frame.add(self.lbl_description)
        self.dsc_frame.hide_all()
        align.add(self.dsc_frame)
        self.pack_start(align, False, False)

        w_required_sources = gtk.HBox()
        w_required_sources.hide()

        required_sources = action.get_required_sources()
        for name, parameter in required_sources:
            self.params_views[name] = ParameterView(parameter)
            self.params_views[name].set_callback(
                "filter-sources",
                lambda t: self.emit_event("filter-sources", t))
            self.params_views[name].set_callback(
                "select-parameter",
                lambda name, idx: self.emit_event("select-parameter", name, idx))

            self.pack_start(self.params_views[name], False, False)

    def _no_free_slot(self, source):
        message = gtk.MessageDialog(type=gtk.MESSAGE_INFO,
                                    buttons=gtk.BUTTONS_OK)

        message.set_markup("There is no free slot for source: '{0}'.".format(
            source.get_name()))
        message.run()
        message.destroy()

class SourceRepository(EventSource):

    def __init__(self):
        EventSource.__init__(self)

        self.repo = []

    def add(self, source):
        """ Adds a new source to the repository. Returns True if the
        source is added, otherwise False.

        Arguments:
        source -- object (actionselector.Source)

        """

        if source not in self.repo:
            self.repo.append(source)
            self.emit_event("add-source", source)
            return True
        return False

    def remove(self, source):
        """ Removes the source from the repository. Returns True if the source
        is removed, otherwise False.

        Arguments:
        source -- source which should be removed.

        """

        if source in self.repo:
            idx = self.repo.index(source)
            del self.repo[idx]
            return True
        return False

    def get_sources(self, filter=[]):
        """ Returns list of loaded sources. If the filter is not empty,
        the sources are filtered by the extension of type.

        Arguments:
        filter -- list of types extensions which will be excluded.

        """

        return [source for source in self.repo
                if source.get_type().get_id() not in filter]

class ViewSourceRepository(gtk.VBox, EventSource):

    def __init__(self, repository):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.filter = []
        self.repository = repository
        self.repository.set_callback("add-source", self.add_source)

        sources = repository.get_sources(self.filter)
        for source in sources:
            self.add_source(source)

    def set_selected_parameter(self, name):
        self.selected_parameter = name

    def set_filter(self, filter):
        self.filter = filter
        if not self.filter:
            for child in self.get_children():
                child.show_all()
            return

        for child in self.get_children():
            if isinstance(child, ViewSource):
                if child.get_source().get_type().get_id() in self.filter:
                    child.show_all()
                else:
                    child.hide_all()

    def get_filter(self):
        return self.filter

    def add_source(self, source):
        w_source = ViewSource(source)
        w_source.set_callback("attach-source",
                              lambda s: self.emit_event("attach-source", s))
        self.pack_start(w_source, False, False)
        w_source.show_all()

class TriColumnsWidget(gtk.VBox):

    def __init__(self):
        gtk.VBox.__init__(self)

        # repository of loaded sources
        self.sources_repository = SourceRepository()
        self.w_selected_action = ActionViewFull()

        # toolbar
        toolbar = gtk.HBox(False)
        toolbar.set_border_width(5)
        button = gtk.Button("Load")
        button.connect("clicked", lambda w: self._load_action())
        toolbar.pack_start(button, False, False)

        button = gtk.Button("Filter off")
        button.connect("clicked", lambda w: self._filter_off_action())
        toolbar.pack_start(button, False, False)
        self.pack_start(toolbar, False, False)

        # sources -------------------------------------------------------------
        paned = gtk.HPaned()
        vbox = gtk.VBox(False)

        title = gtk.Label("Sources:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        vbox.pack_start(haling, False, False)

        self.w_source_repository = ViewSourceRepository(self.sources_repository)
        self.w_source_repository.set_callback(
            "attach-source", self._attach_source_action)

        scw = gtk.ScrolledWindow()

        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scw.add_with_viewport(self.w_source_repository)

        vbox.pack_start(scw, True, True)

        paned.pack1(vbox, resize=True)

        # actions -------------------------------------------------------------
        # column 2
        paned2 = gtk.VPaned()

        column2 = gtk.VBox(False)
        title = gtk.Label("Actions:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        column2.pack_start(haling, False, False)

        # list of actions # TODO: there should be used call _load_actions ---->
#        action = TestAction()
#        action_view_short = ActionViewShort(action)
#        action_view_short.set_callback(
#            "action-selected", self._select_action)
#
#        actions = self._column([action_view_short])
        actions = self._load_modules()
        # --------------------------------------------------------------------<

        column2.pack_start(actions)
        paned2.pack1(column2, resize=True)

        # full action view
        # TODO: rename scw to more readable term
        self.scw = gtk.ScrolledWindow()
        self.scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.w_selected_action.set_callback("filter-sources",
                                            self._filter_sources_action)
        self.w_selected_action.set_callback("select-parameter",
                                            self._select_parameter)
        self.scw.add_with_viewport(self.w_selected_action)
        self.scw.set_size_request(-1, 30)
        paned2.pack2(self.scw)

        paned.pack2(paned2, resize=True)
        self.pack_start(paned)

        self.show_all()

    def _column(self, items=[]): # TODO: column should be object,
                                 # it will be contain "hide repository".
        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        column = gtk.VBox(False)
        for item in items:
            column.pack_start(item, False, False)
        scw.add_with_viewport(column)
        return scw

    def _filter_off_action(self):
        self.w_selected_action.select_parameter(None, None)
        self.w_source_repository.set_filter([])

    def _select_action(self, action):
        self.w_selected_action.set_action(action)
        self.w_selected_action.show_all()

    def _select_parameter(self, param_name, index):
        self.w_selected_action.select_parameter(param_name, index)

    def _filter_sources_action(self, type):
        self.w_source_repository.set_filter([type])

    def _load_action(self):
        """ It runs a loader for sources. For button "Load" in toolbar. """

        dialog = gtk.FileChooserDialog("Load",
                                       None,
                                       gtk.FILE_CHOOSER_ACTION_OPEN,
                                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                       gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)

        for type in types_repo.get_registered_types():
            filter = gtk.FileFilter()
            filter.set_name("{0} ({1})".format(
                type.get_name(),
                ", ".join(map(
                    lambda s: "*.{0}".format(s),
                    type.get_extensions()))))

            # TODO: should be there used also mime-type?
            for extension in type.get_extensions():
                filter.add_pattern("*.{0}".format(extension))
            dialog.add_filter(filter)

        response = dialog.run()
        if response == gtk.RESPONSE_OK:
            src = Source("/home/sur096/a.kth", types_repo.get_type("kth"))
            self.sources_repository.add(src)
            src = Source("/home/sur096/b.kcs", types_repo.get_type("kcs"))
            self.sources_repository.add(src)
            src = Source("/home/sur096/c.kth", types_repo.get_type("kth"))
            self.sources_repository.add(src)

        elif response == gtk.RESPONSE_CANCEL:
            print "Closed,no files selected"

        dialog.destroy()

    def _attach_source_action(self, source):
        if self.w_selected_action.get_action() is not None:
            selected_parameter = self.w_selected_action.get_selected_parameter()
            self.w_selected_action.get_action().attach_source(source, *selected_parameter)

            # after attach is canceled selected parameter, and turn filter off
            self.w_selected_action.select_parameter(None, None)
            self.w_source_repository.set_filter([])

    def _load_modules(self):
        """ Load modules (actions). It returns a column widget with all loaded
        actions."""

        actions_views = []
        for name, action in plugins.items():
            action_view_short = ActionViewShort(action)
            action_view_short.set_callback(
                "action-selected", self._select_action)
            actions_views.append(action_view_short)

        return self._column(actions_views)

#*******************************************************************************
# Modules methods

def add_plugin(plugin):
    plugins[plugin.name] = plugin

def load_plugins():
    sys.path.insert(0, PLUGIN_DIR)
    for filename in os.listdir(PLUGIN_DIR):
        basename = os.path.basename(filename)
        fullname = os.path.join(PLUGIN_DIR, filename)
        if re.match('.*\.py$', basename) and os.path.isfile(fullname):
            name = filename[:-3] # strip sufix
            # the file is *.py and it exists
            imp.load_source("plugin_" + name, fullname)

    sys.path.remove(PLUGIN_DIR)

load_plugins()
