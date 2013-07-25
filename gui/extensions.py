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
import drawing
import paths

import os
import sys
import imp

from time import time, gmtime, strftime
from events import EventSource, EventCallbacksList
from datatypes import types_repository
from datatypes import NoLoaderExists
from mainwindow import Tab


operations = {} # the list of all loaded operations

class ExtensionException(Exception):
    pass

# *****************************************************************************
# Sources

class Source(object):

    def __init__(self, name, type, data):
        """Initialize a source of data.

        Arguments:
        name -- file name (source file on disk)
        type -- type of the data (stype.Type)
        data -- physical data

        """
        self.name = name
        self.type = type
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
        entry.set_text(self.source.name)
        table.attach(entry, 0, 1, 0, 1)

        # name of data type
        label = gtk.Label()
        label.set_alignment(0, 0)
        label.set_markup("<i>{0}</i>".format(self.source.type.name))
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
        item.set_sensitive(False)
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

    def _cb_show(self):
        if self.tabview is None:
            type = self.source.type
            view = type.get_view(self.source.data, self.app)
            self.tabview = Tab(self.source.type.short_name, view,)
            self.app.window.add_tab(self.tabview)
        else:
            self.app.window.switch_to_tab(self.tabview)

    def _cb_delete(self):
        self.emit_event("delete-source", self.source)


class SourcesRepository(object, EventSource):

    def __init__(self):
        EventSource.__init__(self)

        self._repo = {} # (name: source)

    def add(self, source):
        if source.name not in self._repo:
            self._repo[source.name] = source
            self.emit_event("source-added", source)
            return True
        return False

    def remove(self, source):
        if source.name in self._repo:
            del self._repo[source.name]
            source.data = None # free data
            self.emit_event("source-removed", source)
            return True
        return False

    def get_sources(self, filter=None):
        """Return a list of loaded sources. If the filter is not empty,
        the sources are filtered by the type.

        Keyword arguments:
        filter -- a list of types which will be included (default None);
                  if the filter is None than are include all of sources

        """
        return [source for name, source in self._repo.items()
                if filter is None or source.type in filter]


class SourcesRepositoryView(gtk.VBox, EventSource):

    def __init__(self, repository, app):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.repository = repository
        self.events = EventCallbacksList()
        self.events.set_callback(
            self.repository, "source-added", self._cb_source_added)
        self.events.set_callback(
            self.repository, "source-removed", self._cb_source_removed)
        self.app = app

        self.sources_views = {} # (source, source_view)

        sources = self.repository.get_sources()
        for source in sources:
            self._cb_source_added(source)

    def set_filter(self, filter):
        show_sources = self.repository.get_sources(filter)
        for source, source_view in self.sources_views.items():
            if source in show_sources:
                source_view.show_all()
            else:
                source_view.hide_all()

    def deregister_callbacks(self):
        for source in self.repository.get_sources(None):
            source_view = self.sources_views[source]
            source_view.remove_callback(
                "attach-source", self._cb_attach_source)
            source_view.remove_callback(
                "delete-source", self._cb_delete_source)
        self.events.remove_all()

    def _cb_source_added(self, source):
        source_view = SourceView(source, self.app)
        source_view.set_callback("attach-source", self._cb_attach_source)
        source_view.set_callback("delete-source", self._cb_delete_source)
        self.pack_start(source_view, False, False)
        source_view.show_all()
        self.sources_views[source] = source_view

    def _cb_source_removed(self, source):
        source_view = self.sources_views[source]
        source_view.remove_callback("attach-source", self._cb_attach_source)
        source_view.remove_callback("delete-source", self._cb_delete_source)
        self.remove(source_view)

    def _cb_attach_source(self, source):
        # redirect the event from repository
        self.emit_event("attach-source", source)

    def _cb_delete_source(self, source):
        self.repository.remove(source)


# *****************************************************************************
# Extension

class Argument(object):
    """This class describes the argument of operation. It serves as persistent
     structure.

    """

    def __init__(self, name, type, list=False, minimum=1):
        """Initialize of an argument.

        Arguments:
        name -- display name of argument
        type -- data type of argument (datatypes.Type)
        list -- True if the argument represents a list of arguments, otherwise
                False
        minimum -- minimal count of values in list

        """
        self.name = name
        self.type = type
        self.list = list
        self.minimum = minimum

class Parameter(object, EventSource):

    def __init__(self, argument):
        EventSource.__init__(self)

        self._argument = argument

        self.real_attached = 0
        self.sources = [None] * self._argument.minimum

    @property
    def name(self):
        return self._argument.name

    @property
    def type(self):
        return self._argument.type

    @property
    def minimum(self):
        return self._argument.minimum

    def is_list(self):
        return self._argument.list

    def is_empty(self):
        return self.real_attached == 0

    def sources_count(self):
        """Return a number of real attached sources, with no respect
        to minimum count.

        """
        return self.real_attached

    def get_source(self, index=-1):
        """Return a chosen source.

        Keyword arguments:
        index -- index of chosen source (default -1; last added)

        """
        if not self.sources or index >= len(self.sources):
            return None
        else:
            return self.sources[index]

    def attach_source(self, source, index=None):
        if index is None: # attach
            attached = False
            for i in xrange(len(self.sources)):
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
                    # increase only if the source is None, in the other case
                    # it is only exchange of attached object
                    self.real_attached += 1
                self.sources[index] = source
            else:
                self.sources.append(source)
                self.real_attached += 1
        else: # negative index
            raise ExtensionException(
                "You try attach source to negative index"
                "({0}) of parameter!".format(index))

        self.emit_event("parameter-changed")

    def detach_source(self, index=0):
        if 0 <= index < len(self.sources):
            if len(self.sources) - self.minimum <= 0:
                self.sources[index] = None
            else:
                self.sources.pop(index)
            self.real_attached -= 1
            self.emit_event("parameter-changed")

    def get_data(self):
        if self.is_list():
            return [self.sources[idx].data
                    for idx in xrange(self.real_attached)]
        else:
            return self.sources[0].data


class ParameterView(gtk.Table, EventSource):

    def __init__(self, parameter):
        gtk.Table.__init__(self, 1, 4, False)
        EventSource.__init__(self)

        self.set_border_width(2)

        self.parameter = parameter
        self.event = self.parameter.set_callback(
            "parameter-changed", self._cb_parameter_changed)

        # initialize view
        self._cb_parameter_changed()

    def deregister_callbacks(self):
        self.event.remove()

    def _cb_parameter_changed(self):
        # remove
        for child in self.get_children():
            self.remove(child)

        # create actualized view
        rows = self.parameter.sources_count() + 1
        columns = 4
        self.resize(rows, columns)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_markup("<b>{0}</b>".format(self.parameter.name))
        self.attach(label, 0, 1, 0, 1, xoptions=0)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_markup(
            " ({0})".format(self.parameter.type.short_name))
        self.attach(label, 1, 2, 0, 1)

        until = 1
        if self.parameter.is_list():
            until = self.parameter.sources_count() + 1
            if self.parameter.minimum > self.parameter.sources_count():
                until = self.parameter.minimum

        for i in xrange(until):
            entry = gtk.Entry()
            entry.set_editable(False)
            entry.connect("focus-in-event", self._cb_choose_parameter, i)
            attached_source = self.parameter.get_source(i)
            if attached_source is not None:
                entry.set_text(attached_source.name)
            self.attach(entry, 2, 3, i, i+1, xoptions=gtk.FILL)

            button = gtk.Button("Detach")
            button.set_sensitive(attached_source is not None)
            button.connect(
                "clicked",
                lambda w, index: self.parameter.detach_source(index), i)

            self.attach(button, 3, 4, i, i+1, xoptions=0)

        self.show_all()

    def _cb_choose_parameter(self, widget, event, index):
        self.emit_event("filter-sources", [self.parameter.type])
        self.emit_event("select-parameter", self.parameter, index)


class Operation(object, EventSource):

    def __init__(self):
        EventSource.__init__(self)

        self.events = EventCallbacksList()
        self.parameters = [Parameter(arg) for arg in self.arguments]
        for parameter in self.parameters:
            self.events.set_callback(
                parameter, "parameter-changed", self._cb_parameter_changed)

        self.selected_parameter = (None, None)
        self._state = "ready" if self.all_sources_filled() else "incomplete"

    @property
    def state(self):
        return self._state

    @state.setter
    def state(self, state):
        assert (state == "ready" or
                state == "incomplete" or
                state == "incorrect")
        self._state = state
        self.emit_event("state-changed", state)

    def select_parameter(self, parameter, index=0):
        """Select a specific parameter. The index is important if the selected
         parameter is a list. Then the index specify the position in the list.

        Arguments:
        parameter -- selected parameter

        Keyword arguments:
        index -- the specific position in a list (default 0)

        """
        self.selected_parameter = (parameter, index)

    def run(self, *args):
        """This method is called with attached arguments. Method must not
         any side effect and it must not modify argument.

        """
        return None

    def execute(self):
        args = [parameter.get_data() for parameter in self.parameters]
        return self.run(*args)

    def attach_source(self, source):
        parameter, index = self.selected_parameter
        if parameter is not None and parameter.type == source.type:
            parameter.attach_source(source, index)
            return

        for parameter in self.parameters:
            if source.type == parameter.type and \
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
                for idx in xrange(parameter.minimum):
                    if parameter.get_source(idx) is None:
                        return False
        return True

    def deregister_callbacks(self):
        self.events.remove_all()

    def _cb_parameter_changed(self):
        if self.all_sources_filled():
            self.state = "ready"
        else:
            self.state = "incomplete"


class OperationShortView(gtk.Alignment, EventSource):

    def __init__(self, operation):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.operation = operation

        self.set_padding(5, 0, 5, 5)
        hbox = gtk.HBox(False)

        icon = drawing.StateIcon(self.operation.state)
        self.event = self.operation.set_callback(
            "state-changed", lambda s: icon.set_state(s))
        hbox.pack_start(icon, False, False)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_padding(2, 0)
        label.set_markup("<b>{0}</b>".format(operation.name))
        hbox.pack_start(label, True, True)

        button = gtk.Button("Select")
        button.connect(
            "clicked",
            lambda w : self.emit_event("operation-selected", self.operation))
        hbox.pack_start(button, False, False)

        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)

        frame.add(hbox)
        self.add(frame)

    def deregister_callbacks(self):
        self.event.remove()


class OperationFullView(gtk.VBox, EventSource):

    def __init__(self, app):
        gtk.VBox.__init__(self)
        EventSource.__init__(self)

        self.events = EventCallbacksList()

        self.app = app
        self.operation = None

    def deregister_callbacks(self):
        self.events.remove_all()

    def set_operation(self, operation):

        # remove callbacks
        self.events.remove_all()
        self.events = EventCallbacksList() # TODO: Why must by initialized again ??

        # remove old components
        for comp in self.get_children():
            self.remove(comp)

        # create a new view
        self.operation = operation

        if self.operation is None:
            return

        self.set_border_width(5)
        hbox = gtk.HBox(False)

        icon = drawing.StateIcon(self.operation.state, 25, 25)
        hbox.pack_start(icon, False, False)

        self.events.set_callback(
            self.operation, "no-free-slot", self._cb_no_free_slot)

        # name
        label = gtk.Label()
        label.set_alignment(0, 1)
        label.set_markup("<b>{0}</b>".format(operation.name))
        halign = gtk.Alignment(0, 0.5, 0, 0)
        halign.set_padding(0, 0, 2, 0)
        halign.add(label)
        hbox.pack_start(halign, True, True)

        # button run
        button = gtk.Button("Run")
        button.set_sensitive(operation.state == "ready")
        button.connect("clicked", lambda w: self._cb_run())
        hbox.pack_start(button, False, False)

        self.pack_start(hbox, False, False)

        self.events.set_callback(
            self.operation, "state-changed",
            lambda state: self._cb_state_changed(state, icon, button))

        # description
        def cb_allocate(label, allocation ):
            label.set_size_request(allocation.width -2, -1)

        align = gtk.Alignment(0, 0, 1, 1)
        align.set_padding(0, 5, 5, 5)

        if "" != self.operation.description != None:
            frame = gtk.Frame()
            frame.set_label("Description")
            label = gtk.Label()
            label.set_alignment(0, 1)
            label.set_line_wrap(True)
            label.set_markup(
                "<i>{0}</i>".format(self.operation.description))
            label.connect( "size-allocate", cb_allocate)
            frame.add(label)
            align.add(frame)
            self.pack_start(align, False, False)

        # parameters
        for parameter in operation.parameters:
            param_view = ParameterView(parameter)
            self.events.set_callback(
                param_view, "filter-sources",
                lambda f: self.emit_event("filter-sources", f))
            self.events.set_callback(
                param_view,
                "select-parameter",
                lambda param, idx: self.emit_event(
                    "select-parameter", param, idx))

            self.pack_start(param_view, False, False)

        self.show_all()

    def _cb_run(self):
        data = self.operation.execute()
        self.emit_event("operation-finished", self.operation, data)

    def _cb_state_changed(self, state, icon, btn_run):
        icon.set_state(state)
        if state == "ready":
            btn_run.set_sensitive(True)
        else:
            btn_run.set_sensitive(False)

    def _cb_no_free_slot(self, source):
        self.app.show_message_dialog(
            "There is no free slot for source: '{0}'.".format(source.name),
            gtk.MESSAGE_INFO)


# *****************************************************************************
# Extensions manager

class ExtensionManager(gtk.VBox):

    def __init__(self, sources_repository, app):
        gtk.VBox.__init__(self)

        self.__objects_with_callbacks = []

        self.app = app
        self.loaded_operations = []
        self.events = EventCallbacksList()

        # repository of loaded sources
        self.sources_repository = sources_repository
        self.events.set_callback(
            self.sources_repository, "source-removed", self._cb_detach_source)

        # full view of selected operation
        self.full_view = OperationFullView(self.app)
        self.__objects_with_callbacks.append(self.full_view)
        self.events.set_callback(
            self.full_view, "select-parameter", self._cb_select_parameter)
        self.events.set_callback(
            self.full_view, "filter-sources",
            lambda f: self.sources_view.set_filter(f))
        self.events.set_callback(
            self.full_view, "operation-finished", self._cb_operation_finished)

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

        self.sources_view = SourcesRepositoryView(
            self.sources_repository, self.app)
        self.__objects_with_callbacks.append(self.sources_view)
        self.events.set_callback(
            self.sources_view, "attach-source",
            lambda s: self._cb_attach_source(s))

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scw.add_with_viewport(self.sources_view)

        vbox.pack_start(scw, True, True)

        paned1 = gtk.HPaned()
        paned1.pack1(vbox, resize=True)

        # list of operation's views
        vbox = gtk.VBox(False)
        title = gtk.Label("Operations:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(title)
        vbox.pack_start(haling, False, False)
        operations = self._load_operations()
        vbox.pack_start(operations)
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

    def close(self):
        for obj in self.__objects_with_callbacks:
            obj.deregister_callbacks()
        self.events.remove_all()

    def _load_operations(self):
        """Load modules (operations). It returns a column with all loaded
         operations.

        """
        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        column = gtk.VBox(False)
        for name, operation_cls in operations.items():
            operation = operation_cls()
            self.loaded_operations.append(operation)
            short_view = OperationShortView(operation)
            self.events.set_callback(
                short_view, "operation-selected",
                lambda op: self.full_view.set_operation(op))
            column.pack_start(short_view, False, False)

            self.__objects_with_callbacks.append(operation)
            self.__objects_with_callbacks.append(short_view)
        scw.add_with_viewport(column)
        return scw

    def _cb_load(self):
        """It runs a loader for sources. For button "Load" in toolbar."""

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
        if self.full_view.operation is not None:
            self.full_view.operation.select_parameter(None, None)
            self.sources_view.set_filter(None)

    def _cb_select_parameter(self, param, index):
        operation = self.full_view.operation
        if operation is not None:
            operation.select_parameter(param, index)

    def _cb_operation_finished(self, operation, sources):
        if sources is None:
            return

        try:
            sources = list(sources)
        except TypeError:
            sources = [sources]

        ts = time()
        tstring = strftime("%Y-%m-%d %H:%M:%S", gmtime(ts))
        # add milliseconds
        tstring = "%s.%03d" % (tstring, int(round(ts * 1e3)) - int(ts) * 1e3)
        for source in sources:
            source.name = "%s (%s)" % (source.name, tstring)
            self.sources_repository.add(source)
        # destroy filter and selected_parameter
        self.full_view.operation.select_parameter(None, None)
        self.sources_view.set_filter(None)

    def _cb_attach_source(self, source):
        operation = self.full_view.operation
        if operation is not None:
            operation.attach_source(source)

            param, idx = operation.selected_parameter
            if param is None:
                return

            if param.is_list(): # the filter will stay on,
                                # if a parameter is list type
                operation.select_parameter(param, idx + 1)
            else:
                operation.select_parameter(None, None)
                self.sources_view.set_filter(None)
        else:
            self.app.show_message_dialog(
                "No operation is chosen.", gtk.MESSAGE_INFO)

    def _cb_detach_source(self, source):
        """Detach source from all operation's parameters."""

        for operation in self.loaded_operations:
            for param in operation.parameters:
                if param.is_list():
                    idx = 0
                    while idx < param.minimum + param.sources_count():
                        psource = param.get_source(idx)
                        if psource is not None and psource == source:
                            param.detach_source(idx)
                        else:
                            idx += 1
                else:
                    psource = param.get_source()
                    if psource is not None and psource == source:
                        param.detach_source()


# *****************************************************************************
# Modules methods

def add_operation(operation):
    operations[operation.name] = operation

def load_extensions():
    sys.path.insert(0, paths.EXTENSIONS_DIR)
    for filename in os.listdir(paths.EXTENSIONS_DIR):
        basename = os.path.basename(filename)
        fullname = os.path.join(paths.EXTENSIONS_DIR, filename)
        if basename.endswith(".py") and os.path.isfile(fullname):
            name = basename.split(".", 1)[0]
            # the file is *.py and it exists
            imp.load_source("extension_" + name, fullname)
    sys.path.remove(paths.EXTENSIONS_DIR)

load_extensions()
