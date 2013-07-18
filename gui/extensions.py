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
from datatypes import types_repository
from datatypes import NoLoaderExists
from mainwindow import Tab

KAIRA_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
EXTENSIONS_DIR = os.path.join(KAIRA_DIR, "extensions")

extensions = {}

# ******************************************************************************
# Sources

class Source:

    def __init__(self, name, type, data):
        """ A source of data.

        Arguments:
        name -- file name (source file on disk)
        type -- type of the data (stype.Type)
        data -- if data are in memory the source could be created on fly,
                default value is None

        """

        self.name = name
        self.type = type
        self.data = data

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

    def get_data(self):
        return self.data

    def set_data(self, data):
        self.data = data

class SourceView(gtk.Alignment, EventSource):

    def __init__(self, source, app):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.source = source
        self.app = app # reference to the main application
        self.tabview = None

        self.set_padding(5, 0, 10, 10)

        table = gtk.Table(2, 3, False)
        table.set_border_width(2)
        table.set_col_spacing(0, 10)
        table.set_col_spacing(1, 2)

        # name of source
        entry = gtk.Entry()
        entry.set_size_request(40, -1)
        entry.set_editable(False)
        entry.set_text(source.get_name())
        table.attach(entry, 0, 1, 0, 1)

        # name of data type
        label = gtk.Label()
        label.set_alignment(0, 0)
        label.set_markup("<i>{0}</i>".format(source.type.name))
        table.attach(label, 0, 1, 1, 2)

        # attach button
        button = gtk.Button("Attach")
        button.connect(
            "clicked", lambda w: self.emit_event("attach-source", self.source))
        table.attach(button, 1, 2, 0, 2, xoptions=gtk.FILL)

        # source menu
        menu = gtk.Menu()

        item = gtk.MenuItem("Show")
        item.connect("activate", lambda w: self._cb_show())
        menu.append(item)
        menu.append(gtk.SeparatorMenuItem())

        item = gtk.MenuItem("Store")
        menu.append(item)
        item = gtk.MenuItem("Load")
        item.set_sensitive(False)
        menu.append(item)
        menu.append(gtk.SeparatorMenuItem())

        item = gtk.MenuItem("Delete")
        item.connect("activate", lambda w: self._cb_delete())
        menu.append(item)

        source_menu = gtk.MenuItem(">")
        source_menu.set_submenu(menu)

        menu_bar = gtk.MenuBar()
        menu_bar.set_child_pack_direction(gtk.PACK_DIRECTION_TTB)
        menu_bar.append(source_menu)
        table.attach(menu_bar, 2, 3, 0, 2, xoptions=0)

        # source component
        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)
        frame.add(table)

        self.add(frame)

    def get_source(self):
        return self.source

    def _cb_show(self):
        if self.tabview is None:
            type = self.source.get_type()
            view = type.get_view(self.source.get_data(), self.app)
            self.tabview = Tab("Tracelog", view, mainmenu_groups=("tracelog",))
            self.app.window.add_tab(self.tabview)
        else:
            self.app.window.switch_to_tab(self.tabview)

    def _cb_delete(self):
        self.emit_event("delete-source", self.source)

class SourceRepository(EventSource):

    def __init__(self):
        EventSource.__init__(self)

        self.repo = {} # (name: source)
        self.filter = [] # contains id of datatypes.Type

    def add(self, source):
        if source.get_name() not in self.repo:
            self.repo[source.get_name()] = source
            self.emit_event("source-added", source)
            return True
        return False

    def remove(self, source):
        if source.get_name() in self.repo:
            del self.repo[source.get_name()]
            source.set_data(None) # free data
            self.emit_event("source-removed", source)
            return True
        return False

    def get_filter(self):
        return self.filter

    def set_filter(self, filter):
        self.filter = filter
        self.emit_event("filter-changed", filter)

    def get_sources(self):
        """ Returns list of loaded sources. If the filter is not empty,
        the sources are filtered by the extension of type.

        Arguments:
        filter -- list of types extensions which will be excluded.
        """

        return [source for source in self.repo
                if source.get_type() not in self.filter]

class SourceRepositoryView(gtk.VBox, EventSource):

    def __init__(self, repository, app):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.repository = repository
        self.repository.set_callback("filter-changed", self._cb_filter_changed)
        self.repository.set_callback("source-added", self._cb_source_added)
        self.repository.set_callback("source-removed", self._cb_source_removed)
        self.app = app

        sources = self.repository.get_sources()
        for source in sources:
            self._cb_source_added(source)

    def _cb_filter_changed(self, filter):
        if not filter:
            for child in self.get_children():
                child.show_all()
            return

        for child in self.get_children():
            if child.get_source().get_type() in filter:
                child.show_all()
            else:
                child.hide_all()

    def _cb_source_added(self, source):
        source_view = SourceView(source, self.app)
        source_view.set_callback("attach-source", self._cb_attach_source)
        source_view.set_callback("delete-source", self._cb_delete_source)
        self.pack_start(source_view, False, False)
        source_view.show_all()

    def _cb_source_removed(self, source):
        for child in self.get_children():
            if isinstance(child, SourceView) and \
                child.get_source().get_name() == source.get_name():

                child.remove_callback("attach-source", self._cb_attach_source)
                child.remove_callback("delete-source", self._cb_delete_source)
                self.remove(child)

    def _cb_attach_source(self, source):
        # redirect the event from repository
        self.emit_event("attach-source", source)

    def _cb_delete_source(self, source):
        self.repository.remove(source)

# ******************************************************************************
# Extension

class IncorrectStateException(Exception):

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "Incorrect state value '{0}'.".format(self.value)

class Extension(EventSource):

    """ Extension is the template for user's extensions. It should not be used
     directly. """

    def __init__(self, name, description):
        EventSource.__init__(self)

        self.name = name
        self.description = description
        self.parameters = []
        self.selected_parameter = (None, None)

        # adding of user's parameters
        self.init_parameters()
        self.state = "ready" if self.all_sources_filled() else "incomplete"

    # -------------------------------------------------------------------------
    # interface

    def init_parameters(self):
        """ There must be initialized all parameters because of right
         identification of the state of extension. """
        pass

    def run(self):
        """ This method is called for proceeding of the extension. The setting
        of some inner parameters of the extension is left to the user. """
        pass

    def get_processed_data(self): # interface
        """ Returns a list of extensions.Source type or None. """
        return None

    # -------------------------------------------------------------------------
    # implemented

    def get_name(self):
        return self.name

    def get_description(self):
        return self.description

    def get_state(self):
        return self.state

    def set_state(self, state):
        if state == "ready" or state == "incomplete" or state == "incorrect":
            self.state = state
            self.emit_event("state-changed", state)
        else:
            raise IncorrectStateException(state)

    def get_parameters(self):
        return self.parameters

    def get_selected_parameter(self):
        return self.selected_parameter

    def select_parameter(self, parameter, index=0):
        """ Select a specific parameter. The index is important if the selected
         parameter is a list. Then the index specify the position in the list.

        Arguments:
        parameter -- selected parameter
        index -- the specific position in a list, default is 0
        """

        self.selected_parameter = (parameter, index)

    def attach_source(self, source):
        parameter, index = self.selected_parameter
        if parameter is not None:
            parameter.attach_source(source, index)
            return

        for parameter in self.parameters:
            if source.get_type() == parameter.get_type() and \
                    (parameter.is_empty() or parameter.is_list()):
                parameter.attach_source(source)
                return

        # not attached source
        self.emit_event("no-free-slot", source)

    def all_sources_filled(self):
        for parameter in self.parameters:
            if parameter.get_source() is None:
                return False
            if parameter.is_list():
                for idx in range(parameter.get_required()):
                    if parameter.get_source(idx) is None:
                        return False
        return True

    def _cb_parameter_changed(self):
        if self.all_sources_filled():
            self.set_state("ready")
        else:
            self.set_state("incomplete")

    def _dev_add_parameter(self, name, type, list=False, required=1):
        """ Adds a new argument. This method should be called in the method
         'init_parameters'.

        Arguments:
        name -- Name of argument
        type -- Type of argument (type)
        list -- True if the argument is represented as a list,
                 otherwise false
        required -- The default value is 1 (one argument is almost always
                    required), but if it is an list, it determines a minimum
                    length of the list

        """
        if not list and required > 1:
            raise Exception(
                "The parameter '{0}' is not a list, 'required' must be equal "
                "to one, not '{1}'!".format(name, required))

        parameter = Parameter(name, type, list, required)
        parameter.set_callback("parameter-changed", self._cb_parameter_changed)
        self.parameters.append(parameter)

class ExtensionShortView(gtk.Alignment, EventSource):

    def __init__(self, extension):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.extension = extension

        self.set_padding(5, 0, 5, 5)
        hbox = gtk.HBox(False)

        icon = StateIcon(extension.get_state())
        self.extension.set_callback(
            "state-changed", lambda s: icon.set_state(s))
        hbox.pack_start(icon, False, False)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_padding(2, 0)
        label.set_markup("<b>{0}</b>".format(extension.get_name()))
        hbox.pack_start(label, True, True)

        button = gtk.Button("Select")
        button.connect(
            "clicked",
            lambda w : self.emit_event("extension-selected", self.extension))
        hbox.pack_start(button, False, False)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        frame.add(hbox)
        self.add(frame)

class ExtensionFullView(gtk.VBox, EventSource):

    def __init__(self, app):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.__registered_callbacks = []

        self.app = app
        self.extension = None

    def get_extension(self):
        return self.extension

    def set_extension(self, extension):

        # remove callbacks
        for callback in self.__registered_callbacks:
            callback.remove()
        self.__registered_callbacks = []

        # remove old components
        for comp in self.get_children():
            self.remove(comp)

        # create a new view
        self.extension = extension

        if self.extension is None:
            return

        self.set_border_width(5)
        hbox = gtk.HBox(False)

        icon = StateIcon(extension.get_state(), 25, 25)
        hbox.pack_start(icon, False, False)

        cb = self.extension.set_callback("no-free-slot", self._cb_no_free_slot)
        self.__registered_callbacks.append(cb)

        # name
        label = gtk.Label()
        label.set_alignment(0, 1)
        label.set_markup("<b>{0}</b>".format(extension.get_name()))
        halign = gtk.Alignment(0, 0.5, 0, 0)
        halign.set_padding(0, 0, 2, 0)
        halign.add(label)
        hbox.pack_start(halign, True, True)

        # button run
        button = gtk.Button("Run")
        button.set_sensitive(extension.get_state() == "ready")
        button.connect("clicked", lambda w: self.extension.run())
        hbox.pack_start(button, False, False)

        self.pack_start(hbox, False, False)

        cb = self.extension.set_callback(
            "state-changed",
            lambda state: self._cb_state_changed(state, icon, button))

        self.__registered_callbacks.append(cb)
        # description
        def cb_allocate(label, allocation ):
            label.set_size_request(allocation.width -2, -1)

        align = gtk.Alignment(0, 0, 1, 1)
        align.set_padding(0, 5, 5, 5)

        if "" != self.extension.get_description() != None:
            frame = gtk.Frame()
            frame.set_label("Description")
            label = gtk.Label()
            label.set_alignment(0, 1)
            label.set_line_wrap(True)
            label.set_markup(
                "<i>{0}</i>".format(self.extension.get_description()))
            label.connect( "size-allocate", cb_allocate)
            frame.add(label)
            align.add(frame)
            self.pack_start(align, False, False)

        # parameters
        parameters = extension.get_parameters()
        for parameter in parameters:
            param_view = ParameterView(parameter)
            cb = param_view.set_callback(
                "filter-sources",
                lambda f: self.emit_event("filter-sources", f))
            self.__registered_callbacks.append(cb)
            cb = param_view.set_callback(
                "select-parameter",
                lambda param, idx: self.emit_event(
                    "select-parameter", param, idx))
            self.__registered_callbacks.append(cb)

            self.pack_start(param_view, False, False)

        self.show_all()

    def _cb_state_changed(self, state, icon, btn_run):
        icon.set_state(state)
        if state == "ready":
            btn_run.set_sensitive(True)
        else:
            btn_run.set_sensitive(False)

    def _cb_no_free_slot(self, source):
        self.app.show_message_dialog(
            "There is no free slot for source: '{0}'.".format(
                source.get_name()),
            gtk.MESSAGE_INFO)

class Parameter(EventSource):

    def __init__(self, name, type, list, required):
        EventSource.__init__(self)

        self.name = name
        self.type = type
        self.list = list
        self.required = required

        self.real_attached = 0
        self.sources = [None] * self.required

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

    def is_list(self):
        return self.list

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

    def detach_source(self, index=0):
        if 0 <= index < len(self.sources):
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
            "parameter-changed", self._cb_parameter_changed)

        # initialize view
        self._cb_parameter_changed()

    def _cb_parameter_changed(self):
        # remove
        for child in self.get_children():
            self.remove(child)

        # create actualized view
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
            " ({0})".format(self.parameter.get_type().short_name))
        self.attach(lbl_type, 1, 2, 0, 1)

        until = 1
        if self.parameter.is_list():
            until = self.parameter.get_sources_count() + 1
            if self.parameter.get_required() > self.parameter.get_sources_count():
                until = self.parameter.get_required()

        for i in range(0, until):
            entry = gtk.Entry()
            entry.set_editable(False)
            entry.connect("focus-in-event", self._cb_choose_parameter, i)
            attached_source = self.parameter.get_source(i)
            if attached_source is not None:
                entry.set_text(attached_source.get_name())
            self.attach(entry, 2, 3, i, i+1, xoptions=gtk.FILL)

            button = gtk.Button("Detach")
            button.set_sensitive(attached_source is not None)
            button.connect(
                "clicked",
                lambda w, index: self.parameter.detach_source(index), i)

            self.attach(button, 3, 4, i, i+1, xoptions=0)

        self.show_all()

    def _cb_choose_parameter(self, widget, event, index):
        self.emit_event("filter-sources", [self.parameter.get_type()])
        self.emit_event("select-parameter", self.parameter, index)

# ******************************************************************************
# Extensions manager

class ExtensionManager(gtk.VBox):

    def __init__(self, app):
        gtk.VBox.__init__(self)

        self.app = app

        # repository of loaded sources
        self.sources_repository = SourceRepository()
        self.sources_repository.set_callback(
            "source-removed", self._cb_detach_source)

        # full view of selected extension
        self.full_view = ExtensionFullView(self.app)
        self.full_view.set_callback(
            "filter-sources", lambda f: self.sources_repository.set_filter(f))
        self.full_view.set_callback(
            "select-parameter", self._cb_select_parameter)

        # toolbar
        toolbar = gtk.HBox(False)
        toolbar.set_border_width(5)
        button = gtk.Button("Load")
        button.connect("clicked", lambda w: self._cb_load())
        toolbar.pack_start(button, False, False)

        button = gtk.Button("Filter off")
        button.connect("clicked", lambda w: self._cb_filter_off())
        toolbar.pack_start(button, False, False)
        self.pack_start(toolbar, False, False)

        # sources
        vbox = gtk.VBox(False)

        title = gtk.Label("Sources:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        vbox.pack_start(haling, False, False)

        sources_view = SourceRepositoryView(self.sources_repository, self.app)
        sources_view.set_callback(
            "attach-source", lambda s: self._cb_attach_source(s))

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scw.add_with_viewport(sources_view)

        vbox.pack_start(scw, True, True)

        paned1 = gtk.HPaned()
        paned1.pack1(vbox, resize=True)

        # list of extension's views
        vbox = gtk.VBox(False)
        title = gtk.Label("Extensions:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        vbox.pack_start(haling, False, False)
        extensions = self._load_extensions()
        vbox.pack_start(extensions)
        paned2 = gtk.VPaned()
        paned2.pack1(vbox, resize=True)

        # full action view (selected action)
        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scw.set_size_request(-1, 30)
        scw.add_with_viewport(self.full_view)
        paned2.pack2(scw)

        paned1.pack2(paned2, resize=True)
        self.pack_start(paned1)

        self.show_all()

    def _load_extensions(self):
        """ Load modules (extensions). It returns a column with all loaded
         extensions."""

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        column = gtk.VBox(False)
        for name, extension in extensions.items():
            short_view = ExtensionShortView(extension)
            short_view.set_callback(
                "extension-selected",
                lambda ext: self.full_view.set_extension(ext))
            column.pack_start(short_view, False, False)
        scw.add_with_viewport(column)
        return scw

    def _cb_load(self):
        """ It runs a loader for sources. For button "Load" in toolbar. """

        dialog = gtk.FileChooserDialog("Source load",
                                       self.app.window,
                                       gtk.FILE_CHOOSER_ACTION_OPEN,
                                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                       gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)

        for type in types_repository:
            filter = gtk.FileFilter()
            name = "{0} ({1})".format(
                type.short_name, ", ".join(map(
                    lambda s: "*.{0}".format(s),
                    type.files_extensions)))
            filter.set_name(name)
            filter.set_data(name, type)

            for file_extension in type.files_extensions:
                filter.add_pattern("*.{0}".format(file_extension))
            dialog.add_filter(filter)

        response = dialog.run()
        if response == gtk.RESPONSE_OK:
            filename = dialog.get_filename()

            filter = dialog.get_filter()
            type = filter.get_data(filter.get_name())

            try:
                src = type.load_source(filename)
                self.sources_repository.add(src)
            except NoLoaderExists as ex:
                self.app.show_message_dialog(str(ex), gtk.MESSAGE_WARNING)
            finally:
                dialog.destroy()

        dialog.destroy()

    def _cb_filter_off(self):
        self.full_view.get_extension().select_parameter(None, None)
        self.sources_repository.set_filter([])

    def _cb_select_parameter(self, param, index):
        extension = self.full_view.get_extension()
        if extension is not None:
            extension.select_parameter(param, index)

    def _cb_attach_source(self, source):
        extension = self.full_view.get_extension()
        if extension is not None:
            extension.attach_source(source)

            # after attach is canceled selected parameter, and turn filter off
            extension.select_parameter(None, None)
            self.sources_repository.set_filter([])
        else:
            self.app.show_message_dialog(
                "No extension is chosen.", gtk.MESSAGE_INFO)

    def _cb_detach_source(self, source):
        """ Detach source from all extension's parameters. """
        for name, extension in extensions.items():
            for param in extension.get_parameters():
                if param.is_list():
                    idx = 0
                    while idx < param.get_sources_count():
                        psource = param.get_source(idx)
                        if psource is not None and psource == source:
                            param.detach_source(idx)
                        else:
                            idx += 1
                else:
                    psource = param.get_source()
                    if psource is not None and psource == source:
                        param.detach_source()

# ******************************************************************************
# Temporary classes

class StateIcon(gtk.DrawingArea):

    def __init__(self, state, width=30, height=30):
        """ Initialize of StateIcon.

        Arguments:
        state -- possible values: "ready", "incomplete", "incorrect"
        """
        self.icon_state = state
        gtk.DrawingArea.__init__(self)
        self.set_size_request(width, height)
        self.connect("expose_event", self._expose)

    def set_state(self, state):
        if state == "ready" or state == "incomplete" or state == "incorrect":
            self.icon_state = state
            self.queue_draw()

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

# ******************************************************************************
# Modules methods

def add_extension(extension):
    extensions[extension.name] = extension

def load_extensions():
    sys.path.insert(0, EXTENSIONS_DIR)
    for filename in os.listdir(EXTENSIONS_DIR):
        basename = os.path.basename(filename)
        fullname = os.path.join(EXTENSIONS_DIR, filename)
        if re.match('.*\.py$', basename) and os.path.isfile(fullname):
            name = filename[:-3] # strip sufix
            # the file is *.py and it exists
            imp.load_source("extension_" + name, fullname)
    sys.path.remove(EXTENSIONS_DIR)

load_extensions()
