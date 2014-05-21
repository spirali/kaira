#
#    Copyright (C) 2013, 2014 Martin Surkovsky
#                  2013 Stanislav Bohm
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

from events import EventSource, EventCallbacksList
import datatypes
from mainwindow import Tab
import utils

operations = {} # the list of all loaded operations

# *****************************************************************************
# Sources

class Source(object, EventSource):

    def __init__(self, name, type, data, stored=False, settings=None):
        """Initialize a source of data.

        Arguments:
        name -- file name (source file on disk)
        type -- type of the data (stype.Type)
        data -- physical data

        Keyword:
        stored -- flag informs that source is stored in a file
        settings -- a settings that was used by creating the source

        """
        EventSource.__init__(self)

        if name is not None:
            self._name = name
        else:
            self._name = "{0} ({1})".format(utils.get_timestamp_string(),
                                            utils.get_unique_id())

        self.type = type
        self.data = data
        self.stored = stored
        self.settings = settings

    @property
    def name(self):
        return self._name

    @name.setter
    def name(self, name):
        old_name = self._name
        self._name = name
        self.emit_event("source-name-changed", old_name, name)

    def store(self, filename, app, settings=None):
        """Store the source into a file. It calls a function in the 'savers'
        dictionary by the suffix of a filename.

        Arguments:
        filename -- a name of a file (include a path where the data will be
        stored)
        app -- a reference to the main application

        Keywords:
        settings -- an optional argument where may be stored some users'
        setting information

        """
        suffix = utils.get_filename_suffix(filename)
        if suffix is None and self.type.default_saver is not None:
            suffix = self.type.default_saver
            filename += "." + suffix
        saver = self.type.savers.get(suffix)
        if saver is None:
            app.show_message_dialog(
                    "Cannot save '.{0}' file".format(suffix),
                    gtk.MESSAGE_WARNING)
            return

        correct, settings = saver(self.data, filename, app, settings)
        if correct:
            self.name = filename
            self.settings = settings
            self.stored = True

class SourceView(gtk.Alignment, EventSource):

    def __init__(self, source, app):
        gtk.Alignment.__init__(self, 0, 0, 1, 1)
        EventSource.__init__(self)

        self.source = source
        self.source.set_callback("source-name-changed",
                                 lambda old, new: self.entry_name.set_text(new))

        self.app = app # reference to the main application
        self.tabview = None

        self.set_padding(5, 0, 10, 10)

        table = gtk.Table(2, 3, False)
        table.set_border_width(2)
        table.set_col_spacing(0, 10)
        table.set_col_spacing(1, 2)

        # name of source
        self.entry_name = gtk.Entry()
        self.entry_name.set_size_request(40, -1)
        self.entry_name.set_editable(False)
        self.entry_name.set_text(self.source.name)
        table.attach(self.entry_name, 0, 1, 0, 1)

        # name of data type
        label = gtk.Label()
        label.set_alignment(0, 0)
        label.set_markup("<i>{0}</i>".format(self.source.type.name))
        table.attach(label, 0, 1, 1, 2)

        self.btns_group1 = []
        self.btns_group2 = []
        # attach button
        button = gtk.Button("Attach")
        button.connect(
            "clicked", lambda w: self.emit_event("attach-source", self.source))
        table.attach(button, 1, 2, 0, 2, xoptions=gtk.FILL)
        self.btns_group1.append(button)

        button = gtk.Button("Show")
        button.connect(
            "clicked", lambda w: self._cb_show())
        table.attach(button, 2, 3, 0, 2, xoptions=gtk.FILL)
        self.btns_group1.append(button)

        # source menu
        menu = gtk.Menu()

        item = gtk.MenuItem("Store")
        item.connect("activate", lambda w: self._cb_store())
        self.btns_group1.append(item)
        menu.append(item)
        self.item_reload = gtk.MenuItem("Reload")
        self.item_reload.connect("activate", lambda w: self._cb_load())
        self.item_reload.set_sensitive(self.source.stored)
        menu.append(self.item_reload)
        menu.append(gtk.SeparatorMenuItem())

        self.item_dispose = gtk.MenuItem("Dispose")
        self.item_dispose.connect("activate", lambda w: self._cb_dispose())
        self.item_dispose.set_sensitive(self.source.stored)
        self.btns_group1.append(self.item_dispose)
        menu.append(self.item_dispose)

        item = gtk.MenuItem("Delete")
        item.connect("activate", lambda w: self._cb_delete())
        menu.append(item)
        menu.show_all()

        menu_btn = gtk.Button(">");
        menu_btn.connect_object("event-after", self._menu_handler, menu)
        table.attach(menu_btn, 3, 4, 0, 2, xoptions=0)

        # source component
        frame = gtk.Frame()
        frame.set_shadow_type(gtk.SHADOW_OUT)
        frame.add(table)

        self.add(frame)

    def _menu_handler(self, widget, event):
        if event.type == gtk.gdk.BUTTON_PRESS:
            widget.popup(None, None, None, event.button, event.time)
            return True
        return False

    def _cb_show(self):
        if self.tabview is None:
            type = self.source.type
            view = type.get_view(self.source.data, self.app)
            if view is None:
                return
            tabname = "{0} ({1})".format(
                self.source.type.short_name, os.path.basename(self.source.name))
            self.tabview = Tab(tabname, view)

            # modify close method
            origin_close = self.tabview.close
            def new_close():
                origin_close()
                self.tabview = None
            self.tabview.close = new_close
            self.app.window.add_tab(self.tabview)
        else:
            self.app.window.switch_to_tab(self.tabview)

    def _lock_buttons(self):
        for btn in self.btns_group1:
            btn.set_sensitive(self.source.data is not None)
        for btn in self.btns_group2:
            btn.set_sensitive(self.source.data is None)

    def _cb_store(self):
        if len(self.source.type.savers) == 0:
            self.app.show_message_dialog(
                    "The type '{0}' cannot be saved.".format(
                        self.source.type.name),
                    gtk.MESSAGE_WARNING)
            return
        dialog = gtk.FileChooserDialog("Source store",
                                       self.app.window,
                                       gtk.FILE_CHOOSER_ACTION_SAVE,
                                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                       gtk.STOCK_SAVE, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)
        dialog.add_filter(datatypes.get_save_file_filter(self.source.type))

        try:
            response = dialog.run()
            filename = dialog.get_filename()
        finally:
            dialog.destroy()

        if response == gtk.RESPONSE_OK:
            self.source.store(filename, self.app)
            self.item_reload.set_sensitive(True)
            self.item_dispose.set_sensitive(True)

    def _cb_load(self):
        self.source.data = load_source(
            self.source.name, self.app, self.source.settings).data
        self._lock_buttons()
        self.emit_event("source-data-changed", self.source)

    def _cb_dispose(self):
        self.source.data = None
        self._lock_buttons()
        if self.tabview is not None:
            self.tabview.close()
        self.emit_event("source-data-changed", self.source)

    def _cb_delete(self):
        if self.tabview is not None:
            self.tabview.close()
        self.emit_event("delete-source", self.source)


class SourcesRepository(object, EventSource):

    def __init__(self):
        EventSource.__init__(self)
        self._sources = []

    def __len__(self):
        return len(self._sources)

    def __iter__(self):
        self._index = 0;
        return self

    def next(self):
        if self._index >= len(self._sources):
            raise StopIteration

        source = self._sources[self._index]
        self._index += 1

        return source

    def add(self, source):
        if source.stored:
            for s in self._sources:
                if s.stored and s.name == source.name:
                    print "Source {0} already exists.".format(source.name)
                    break
                    # throw an exception

        self._sources.append(source)
        self.emit_event("source-added", source)

    def remove(self, source): # index
        if  source in self._sources:
            self._sources.remove(source)
            self.emit_event("source-removed", source)
            return True
        return False

    def load_source(self, filename, app, settings=None):
        # calls the module's method (load_source)
        source = load_source(filename, app, settings)
        if source is not None:
            self.add(source)
            return source
        return None

    def get_sources(self, filter=None):
        """Return a list of loaded sources. If the filter is not empty,
        the sources are filtered by the type.

        Keyword arguments:
        filter -- a list of types which will be included (default None);
                  if the filter is None than are include all of sources

        """
        return [source for source in self._sources
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
        return (len(show_sources), len(self.repository))

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
        source_view.set_callback("source-data-changed", self._cb_data_changed)
        self.pack_start(source_view, False, False)
        source_view.show_all()
        self.sources_views[source] = source_view

    def _cb_source_removed(self, source):
        source_view = self.sources_views[source]
        source_view.remove_callback("attach-source", self._cb_attach_source)
        source_view.remove_callback("delete-source", self._cb_delete_source)
        source_view.remove_callback(
            "source-data-changed", self._cb_data_changed)
        self.remove(source_view)

    def _cb_attach_source(self, source):
        self.emit_event("attach-source", source)

    def _cb_data_changed(self, source):
        self.emit_event("source-data-changed", source)

    def _cb_delete_source(self, source):
        self.repository.remove(source)


class Parameter(object):
    """This class describes the parameter of an operation. It serves as a
    persistent structure.

    """

    def __init__(self, name, type, list=False, minimum=1):
        """Initialize of a parameter.

        Arguments:
        name -- display name of argument
        type -- data type of argument (datatypes.Type)
        list -- True if the argument represents a list of arguments, otherwise
                False

        Keywords:
        minimum -- minimal count of values in list (default: 1)

        """
        self.name = name
        self.type = type
        self.list = list
        self.minimum = minimum


class Argument(object, EventSource):

    def __init__(self, parameter):
        EventSource.__init__(self)

        self._parameter = parameter
        self._src_name_changed_events = EventCallbacksList()

        self._real_attached = 0
        self._sources = [None] * self._parameter.minimum

    @property
    def name(self):
        return self._parameter.name

    @property
    def type(self):
        return self._parameter.type

    @property
    def minimum(self):
        return self._parameter.minimum

    def is_list(self):
        return self._parameter.list

    def is_empty(self):
        return self._real_attached == 0

    def sources_count(self):
        """Return a number of real attached sources, without respect to a
        minimum count.

        """
        return self._real_attached

    def get_source(self, index=-1):
        """Return a chosen source.

        Keyword arguments:
        index -- index of chosen source (default -1; last added)

        """
        if not self._sources or index >= len(self._sources):
            return None
        else:
            return self._sources[index]

    def attach_source(self, source, index=None):
        old_real_attached = self._real_attached

        if index is None: # attach
            attached = False
            for i, s in enumerate(self._sources):
                if s is None: # fill required sources firstly
                    self._sources[i] = source
                    attached = True
                    break
            if not attached:
                self._sources.append(source)
            self._real_attached += 1
        else:
            assert(index >= 0)
            if index < len(self._sources):
                if self._sources[index] is None:
                    # increase only if the source is None, in the other case
                    # it is only exchange of attached object
                    self._real_attached += 1
                self._sources[index] = source
            else:
                self._sources.append(source)
                self._real_attached += 1

        if old_real_attached < self._real_attached:
            self.set_src_name_changed_callback(source, self._real_attached - 1)

        self.emit_event("argument-changed")

    def detach_source(self, index=0):
        if 0 <= index < len(self._sources):
            if len(self._sources) - self.minimum <= 0:
                # minimal count of arguments remain visible
                self._sources.append(None)
            self._sources.pop(index)
            self._real_attached -= 1
            self.emit_event("argument-changed")

    def get_data(self):
        if self.is_list():
            return [ source.data
                     for source in self._sources[:self._real_attached] ]
        else:
            return self._sources[0].data

    def set_src_name_changed_callback(self, source, index):
        self._src_name_changed_events.set_callback(
            source, "source-name-changed", self._cb_source_name_changed, index)

    def remove_src_name_changed_callbacks(self):
        self._src_name_changed_events.remove_all()

    def _cb_source_name_changed(self, idx, old_name, new_name):
        self.emit_event("source-name-changed", idx, old_name, new_name)


class ArgumentView(gtk.Table, EventSource):

    def __init__(self, argument):
        gtk.Table.__init__(self, 1, 4, False)
        EventSource.__init__(self)
        self.entries = []

        self.set_border_width(2)

        self.argument = argument
        self.events = EventCallbacksList()
        self.events.set_callback(
            self.argument, "argument-changed", self._cb_argument_changed)
        self.events.set_callback(
            self.argument, "source-name-changed", self._cb_source_name_changed)

        # initialize view
        self._cb_argument_changed()

    def deregister_callbacks(self):
        self.event.remove()

    def _cb_argument_changed(self):
        # remove
        self.argument.remove_src_name_changed_callbacks()
        for child in self.get_children():
            self.remove(child)
        self.entries = []

        # create actualized view
        rows = self.argument.sources_count() + 1
        columns = 4
        self.resize(rows, columns)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_markup("<b>{0}</b>".format(self.argument.name))
        self.attach(label, 0, 1, 0, 1, xoptions=0)

        label = gtk.Label()
        label.set_alignment(0, 0.5)
        label.set_markup(
            " ({0})".format(self.argument.type.short_name))
        self.attach(label, 1, 2, 0, 1)

        until = 1
        if self.argument.is_list():
            until = self.argument.sources_count() + 1
            if self.argument.minimum > self.argument.sources_count():
                until = self.argument.minimum

        for i in xrange(until):
            entry = gtk.Entry()
            entry.set_editable(False)
            entry.connect("focus-in-event", self._cb_choose_argument, i)
            attached_source = self.argument.get_source(i)
            if attached_source is not None:
                # reset callback, make right index
                self.argument.set_src_name_changed_callback(attached_source, i)
                entry.set_text(attached_source.name)
                entry.set_sensitive(attached_source.data is not None)
            self.attach(entry, 2, 3, i, i+1, xoptions=gtk.FILL)
            self.entries.append(entry)

            button = gtk.Button("Detach")
            button.set_sensitive(attached_source is not None)
            button.connect(
                "clicked",
                lambda w, index: self._cb_detach_source(index), i)

            self.attach(button, 3, 4, i, i+1, xoptions=0)

        self.show_all()

    def _cb_source_name_changed(self, idx, old_name, new_name):
        self.entries[idx].set_text(new_name)

    def _cb_detach_source(self, index):
        self.argument.detach_source(index)
        self.emit_event("detach-source", self.argument.get_source(index))

    def _cb_choose_argument(self, widget, event, index):
        self.emit_event("filter-sources", [self.argument.type])
        self.argument.emit_event("select-argument", index)


class Operation(object, EventSource):

    def __init__(self):
        EventSource.__init__(self)

        self.events = EventCallbacksList()
        self.arguments = [Argument(param) for param in self.parameters]
        for argument in self.arguments:
            self.events.set_callback(
                argument, "argument-changed", self._cb_argument_changed)
            self.events.set_callback(
                argument,
                "select-argument",
                lambda p, i: self.select_argument(p, i), argument)

        self.selected_argument = (None, None)
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

    def select_argument(self, argument, index=0):
        """Select a specific argument. The index is important if the selected
         argument is a list. Then the index specify the position in the list.

        Arguments:
        argument -- selected argument

        Keyword arguments:
        index -- the specific position in a list (default 0)

        """
        if argument is None:
            self.selected_argument = (None, None)
            return

        if argument.is_list():
            if index > argument.sources_count():
                index = argument.sources_count()
        else:
            index = 0
        self.selected_argument = (argument, index)

    def run(self, *args):
        """This method is called with attached arguments. Method must not
         have any side effect and it must not modifies its input arguments.

        """
        return None

    def execute(self, app, store_results=True):
        args = [ argument.get_data() for argument in self.arguments ]
        results = self.run(app, *args)
        if not store_results:
            return results
        if results is None:
            return
        try:
            sources = list(results)
        except TypeError:
            sources = [results]
        for source in sources:
            app.sources_repository.add(source)
        return results

    def attach_source(self, source):
        argument, index = self.selected_argument
        if argument is not None and argument.type == source.type:
            argument.attach_source(source, index)
            return
        for argument in self.arguments:
            if (source.type == argument.type and
                    (argument.is_empty() or argument.is_list())):
                argument.attach_source(source)
                return

        # not attached source
        self.emit_event("no-free-slot", source)

    def all_sources_filled(self):
        for argument in self.arguments:
            count = 0
            for idx in xrange(argument.sources_count()):
                src = argument.get_source(idx)
                if src is not None and src.data is not None:
                    count += 1
            if count < argument.minimum:
                return False
        return True

    def deregister_callbacks(self):
        self.events.remove_all()

    def _cb_argument_changed(self):
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

        self.operation = None
        self.app = app
        self.events = EventCallbacksList()

    def deregister_callbacks(self):
        self.events.remove_all()

    def set_operation(self, operation):

        # remove callbacks
        self.events.remove_all()

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
        button = gtk.Button("Run operation")
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

        # arguments
        for argument in operation.arguments:
            param_view = ArgumentView(argument)
            self.events.set_callback(
                param_view, "filter-sources",
                lambda f: self.emit_event("filter-sources", f))
            self.events.set_callback(
                param_view, "detach-source",
                lambda s: self.emit_event("detach-source", s))

            self.pack_start(param_view, False, False)

        self.show_all()

    def _cb_run(self):
        data = self.operation.execute(self.app)
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
# Operation manager

class OperationManager(gtk.VBox):

    def __init__(self, app):
        gtk.VBox.__init__(self)

        self.__objects_with_callbacks = []

        self.app = app
        self.loaded_operations = []
        self.events = EventCallbacksList()

        # repository of loaded sources
        self.events.set_callback(
            app.sources_repository, "source-removed",
            self._cb_detach_source_from_all_operations)

        # full view of selected operation
        self.full_view = OperationFullView(self.app)
        self.__objects_with_callbacks.append(self.full_view)
        self.events.set_callback(
            self.full_view, "filter-sources", self._cb_filter_sources)
        self.events.set_callback(
            self.full_view, "operation-finished", self._cb_operation_finished)
        self.events.set_callback(
            self.full_view, "detach-source", self._cb_detach_source)

        # toolbar
        toolbar = gtk.HBox(False)
        toolbar.set_border_width(5)
        button = gtk.Button("Load source")
        button.connect("clicked", lambda w: self._cb_load())
        toolbar.pack_start(button, False, False)

        button = gtk.Button("Disable filter")
        button.connect("clicked", lambda w: self._cb_filter_off())
        toolbar.pack_start(button, False, False)
        self.pack_start(toolbar, False, False)

        # sources
        vbox = gtk.VBox(False)
        vbox.set_size_request(80,-1)

        self.sources_title = gtk.Label()
        self.sources_title.set_markup("Sources:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(self.sources_title)
        vbox.pack_start(haling, False, False)

        self.sources_view = SourcesRepositoryView(
            app.sources_repository, self.app)
        self.__objects_with_callbacks.append(self.sources_view)
        self.events.set_callback(
            self.sources_view, "attach-source", self._cb_attach_source)
        self.events.set_callback(
            self.sources_view,
            "source-data-changed", self._cb_source_data_changed)

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        scw.add_with_viewport(self.sources_view)

        vbox.pack_start(scw, True, True)

        paned1 = gtk.HPaned()
        paned1.pack1(vbox, resize=True)

        # list of operation's views
        vbox = gtk.VBox(False)
        label = gtk.Label("Operations:")
        haling = gtk.Alignment(0, 0, 0, 0)
        haling.set_padding(0, 5, 2, 0)
        haling.add(label)
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

    def load_source(self, filename):
        return self.app.sources_repository.load_source(filename, self.app)

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
                short_view, "operation-selected", self._cb_operation_selected)
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

        for filter in datatypes.get_load_file_filters():
            dialog.add_filter(filter)

        try:
            response = dialog.run()
            if response == gtk.RESPONSE_OK:
                self._cb_filter_off()
                filename = dialog.get_filename()
                self.load_source(filename)
        finally:
            dialog.destroy()

    def _cb_operation_selected(self, operation):
        if self.full_view.operation == operation:
            return
        self._cb_filter_off()
        self.full_view.set_operation(operation)

    def _cb_filter_sources(self, type):
        visible_sources, all_sources = self.sources_view.set_filter(type)
        self.sources_title.set_markup(
            "Sources (<b>visible {0} from {1}</b>):".format(
                visible_sources, all_sources))

    def _cb_filter_off(self):
        self.sources_title.set_markup("Sources:")
        self.sources_view.set_filter(None)
        if self.full_view.operation is not None:
            self.full_view.operation.select_argument(None, None)

    def _cb_operation_finished(self, operation, sources):
        # destroy filter and selected_argument
        self.full_view.operation.select_argument(None, None)
        self.sources_view.set_filter(None)

    def _cb_attach_source(self, source):
        operation = self.full_view.operation
        if operation is not None:
            operation.attach_source(source)

            param, idx = operation.selected_argument
            if param is None:
                return

            if param.is_list(): # the filter will stay on,
                                # if a argument is list type
                operation.select_argument(param, param.sources_count() + 1)
            else:
                operation.select_argument(None, None)
                self.sources_view.set_filter(None)
        else:
            self.app.show_message_dialog(
                "No operation is chosen.", gtk.MESSAGE_INFO)

    def _cb_detach_source(self, source):
        operation = self.full_view.operation
        if operation is not None:
            param, idx = operation.selected_argument
            if param is not None:
                operation.select_argument(param, param.sources_count())

    def _cb_detach_source_from_all_operations(self, source):
        """Detach source from all operation's arguments."""

        for operation in self.loaded_operations:
            for param in operation.arguments:
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

    def _cb_source_data_changed(self, source):
        for operation in self.loaded_operations:
            for argument in operation.arguments:
                for i in xrange(argument.sources_count()):
                    arg_source = argument.get_source(i)
                    if arg_source == source:
                        argument.emit_event("argument-changed")


# *****************************************************************************
# Modules methods

def load_source(filename, app, settings=None):
    """Load the source from a file. It calls a function in the 'loaders'
    dictionary by the suffix of a filename.

    Arguments:
    filename -- a name of a file where are data stored
    app -- a reference to the main application

    Keywords:
    settings -- an optional argument where may be stored some users' setting
    information

    """

    # TODO: Catch IOError
    suffix = utils.get_filename_suffix(filename)
    loader = datatypes.get_loader_by_suffix(suffix)
    if loader is None:
        return None

    data, settings = loader(filename, app, settings)
    if data is None:
        return None
    return Source(
        filename, datatypes.get_type_by_suffix(suffix), data, True, settings)

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
