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
import gobject

class SettingsWidget(gtk.Table):
    """General widget for visualize and change settings parameters.
    The parameters are stored in form of dictionary (key: value). It means
    that every parameter must have a unique key. The widget also provides with
    two specific event signals: value-status-changed and select-key.

    Signals:
    value-status-changed -- it is emit if a value is not accepted by a
    validator or if a value is correct. Value-status is both True or False.
    The signal is emitted when the status is changed from True to False
    or vice versa
    select-key -- it is emit when the view of specific parameter get a focus.

    """
    warning_color = gtk.gdk.color_parse("#f66")

    def __init__(self):
        gtk.Table.__init__(self, 1, 2, False)
        self.set_row_spacings(5)
        self.set_col_spacing(0, 10)
        self.settings = dict() # key: value
        self.value_status = dict() # key: (true|false, message)
        self.widgets = dict() # key: widget
        self.labels = dict()  # key: label
        self.keys_order = []
        self.row = 0 # index of current row

    def set(self, key, value):
        self.settings[key] = value

    def get(self, key):
        assert(key in self.settings)
        return self.settings[key]

    def set_value_status(self, key, status, message=None):
        """Set a status to specific value. If the status is True, the message
        should be empty string (nothing wrong), otherwise the message informs
        about what is wrong.

        Arguments:
        key -- the unique key
        status -- True if the status is right, otherwise False
        message -- The message informs about what is wrong.

        """
        old_status, old_message = self.value_status[key]
        if old_status != status or old_message != message:
            self.value_status[key] = (status, message)
            self.emit("value-status-changed", key)

    def is_value_correct(self, key):
        return self.value_status[key][0]

    def are_values_correct(self):
        return all([status for key, (status,msg) in self.value_status.items()])

    def get_value_status_message(self, key):
        return self.value_status[key][1]

    # -------------------------------------------------------------------------
    # add general widget

    def add_widget(self,
                   key,
                   label,
                   widget,
                   validator=lambda x: None):

        """Adds a general widget which is responsible for correct setting of
        its value.

        Arguments:
        key -- the unique key
        label -- label than will be presented in a widget
        default_value -- default value of a setting's argument
        widget -- a widget which cares about correct setting of a value

        Keywords:
        validator -- a function which checks a value. If the value is correct
        validator returns None, otherwise it returns a message containing what
        is wrong (default: a function returns None)

        """
        self.keys_order.append(key)
        self.value_status[key] = (True, None)
        self.widgets[key] = widget
        self.labels[key] = label

        lbl = gtk.Label(label)
        lbl.set_alignment(0.0, 0.0)
        self.attach(lbl,
                    0, 1,
                    self.row, self.row+1,
                    xoptions=gtk.FILL, yoptions=gtk.FILL,
                    xpadding=5)

        widget.connect("focus-in-event", self._cb_focus_in, key)
        widget.connect("focus-out-event", self._cb_focus_out, key)
        yoptions = (gtk.EXPAND|gtk.FILL
                    if isinstance(widget, gtk.ScrolledWindow) else gtk.FILL)
        self.attach(widget,
                    1, 2,
                    self.row, self.row+1,
                    xoptions=gtk.EXPAND|gtk.FILL, yoptions=yoptions,
                    xpadding=5)

        self.row += 1
        self.resize(self.row, 2)

    def add_separator(self):
        self.attach(
            gtk.HSeparator(),
            0, 2,
            self.row, self.row+1,
            xoptions=gtk.EXPAND|gtk.FILL, yoptions=0,
            xpadding=5, ypadding=5)

        self.row += 1
        self.resize(self.row+1, 2)

    def _cb_focus_in(self, widget, event, key):
        self.emit("select-key", key)

    def _cb_focus_out(self, widget, event, key):
        self.emit("value-committed", key)

    # -------------------------------------------------------------------------
    # add specific components

    def add_entry(self,
                  key,
                  label,
                  default,
                  validator=lambda x: None,
                  strToValue=lambda x: x):

        """ Adds an entry component for editing parameters than can be easily
        represented as a string (string, numbers, etc.).

        Arguments:
        key -- the unique key
        label -- label than will be presented in a widget
        default_value -- default value of a setting's argument

        Keywords:
        validator -- a function than checks values (default: a function
        returns None)
        strToValue -- a function converts a string representation to required
        type, e.g. from string to int, etc. If the value cannot be convert
        than it throws a ValueError. (default: function returns given value)

        """
        self.settings[key] = default

        def callback(editable, key, std_color):
            try:
                value = strToValue(editable.get_text())
                message = validator(value)
                if message is not None:
                    editable.modify_text(gtk.STATE_NORMAL, self.warning_color)
                    self.set_value_status(key, False, message)
                else:
                    editable.modify_text(gtk.STATE_NORMAL, std_color)
                    self.settings[key] = value
                    self.set_value_status(key, True)

            except ValueError:
                editable.modify_text(gtk.STATE_NORMAL, self.warning_color)
                self.set_value_status(
                    key, False,
                    "The string cannot be convert to the desired type.");

        entry = gtk.Entry()
        self.add_widget(key, label, entry, validator)

        std_color = entry.get_style().text[gtk.STATE_NORMAL]
        entry.connect("changed", callback, key, std_color)
        entry.set_text(str(default))

    def add_combobox(self, key, label, items, default=0):

        """Adds to a setting widget a combo-box where it can be selected one
        of the items. If the list of items is empty then is add parameter to
        settings.

        Arguments:
        key -- unique key
        label -- the showed label in setting widget
        items -- couple: (label, object)
        default -- index of default item

        """
        if len(items) == 0:
            return
        assert(default < len(items))

        def callback(combo, key, items):
            self.settings[key] = items[combo.get_active()][1]

        store = gtk.ListStore(str, object)
        combo = gtk.ComboBox(store)
        for item in items:
            store.append(item)
        cell = gtk.CellRendererText()
        combo.pack_start(cell, True)
        combo.add_attribute(cell, 'text', 0)
        combo.connect("changed", callback, key, items)
        combo.set_active(default) # also set default value
        self.add_widget(key, label, combo)

    def add_radiobuttons(self, key, label, items, default=0, ncols=1):

        """Adds a list of radio-buttons where one them can be selected.

        Arguments:
        key -- unique key
        label -- the showed label in settings widget
        items -- couples: (label, object)

        Keywords:
        default -- the index of default value (default: first item)
        ncols -- number of radio-buttons which will be put in one line.
        (default: 1)

        """
        if len(items) == 0:
            return
        assert(default < len(items))

        def callback(button, key, value):
            self.settings[key] = value

        buttons = []
        vbox, hbox = gtk.VBox(), gtk.HBox()
        idx, button = 0, None
        for vlabel, value in items:
            button = gtk.RadioButton(button, vlabel)
            button.connect("toggled", callback, key, value)
            buttons.append(button)
            hbox.pack_start(button, False, False, 3)
            idx += 1
            if idx % ncols == 0:
                vbox.pack_start(hbox, False, False)
                hbox = gtk.HBox()
        if idx % ncols != 0: # add last unprocessed row
            vbox.pack_start(hbox, False, False)
        buttons[default].set_active(True)
        vbox.show_all()
        self.add_widget(key, label, vbox)

    def add_checkbutton(self, key, label, default_value=True, button_label=""):

        self.settings[key] = default_value

        def callback(button, key):
            self.settings[key] = not self.settings[key]

        button = gtk.CheckButton(button_label)
        button.set_active(default_value) # also set default value
        button.connect("toggled", callback, key)
        self.add_widget(key, label, button)

    def add_checkbuttons(self, key, label, items, defaults=[0], ncols=1):
        """Add to setting widget a list of check-box buttons and in the
        settings it is represented by a list of selected values.

        Arguments:
        key -- unique key
        label -- the showed label in setting widget
        items -- couples: (label, object)

        Keywords:
        defaults -- a list of indexes in items (default: first items)
        ncols -- a number of check-boxes which should be in one line
        (default: 1)

        """
        if len(items) == 0:
            return
        assert(all([index < len(items) for index in defaults]))

        self.settings[key] = []
        def callback(button, key, value):
            if value in self.settings[key]:
                self.settings[key].remove(value)
            else:
                self.settings[key].append(value)

        buttons = []
        vbox, hbox = gtk.VBox(), gtk.HBox()
        idx = 0
        for vlabel, value in items:
            button = gtk.CheckButton(vlabel)
            button.connect("toggled", callback, key, value)
            if idx in defaults:
                buttons.append(button)
            hbox.pack_start(button, False, False, 3)
            idx += 1
            if idx % ncols == 0:
                vbox.pack_start(hbox, False, False)
                hbox = gtk.HBox()
        if idx % ncols != 0: # add last unprocessed row
            vbox.pack_start(hbox, False, False)

        map(lambda btn: btn.set_active(True), buttons) # also set default vals.
        vbox.show_all()

        self.add_widget(key, label, vbox)

    def add_checkbuttons_list(self, key, label, items, defaults=[]):

        if len(items) == 0:
            return
        assert(all([index< len(items) for index in defaults]))

        self.settings[key] = []
        def callback(crtoggle, path, store):
            siter = store.get_iter(path)
            value = store.get_value(siter, 1)
            select = store.get_value(siter, 2)
            if select:
                self.settings[key].remove(value)
            else:
                self.settings[key].append(value)
            store.set_value(siter, 2, not select)

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

        store = gtk.ListStore(gobject.TYPE_STRING,
                              gobject.TYPE_PYOBJECT,
                              gobject.TYPE_BOOLEAN)

        data = [(vlabel, value, idx in defaults)
                for idx, (vlabel, value) in enumerate(items)]
        map(store.append, data) # fill store

        tree_view = gtk.TreeView(store)
        scw.add_with_viewport(tree_view)

        # column with labels of values
        text_renderer = gtk.CellRendererText()
        column = gtk.TreeViewColumn("Value", text_renderer, text=0)
        column.set_sort_column_id(0)
        tree_view.append_column(column)

        # column for select values
        bool_renderer = gtk.CellRendererToggle()
        bool_renderer.set_property("activatable", True)
        bool_renderer.connect("toggled", callback, store)
        column = gtk.TreeViewColumn("Select", bool_renderer, active=2)
        column.set_sort_column_id(2)
        tree_view.append_column(column)

        self.add_widget(key, label, scw)

    # -------------------------------------------------------------------------
    # add specific data types

    def add_int(self, key, label, default_value):
        self.add_entry(key, label, default_value, strToValue=int)

    def add_positive_int(self, key, label, default_value):
        def validator(value):
            if value <= 0:
                return "The number must be greater than zero."
            return None

        self.add_entry(key, label, default_value, validator, int)


# register new signals to setting widget
gobject.type_register(SettingsWidget)
gobject.signal_new("value-status-changed",
                   SettingsWidget,
                   gobject.SIGNAL_RUN_FIRST,
                   gobject.TYPE_NONE,
                   (gobject.TYPE_PYOBJECT,))
gobject.signal_new("select-key",
                   SettingsWidget,
                   gobject.SIGNAL_RUN_FIRST,
                   gobject.TYPE_NONE,
                   (gobject.TYPE_PYOBJECT,))
gobject.signal_new("value-committed",
                   SettingsWidget,
                   gobject.SIGNAL_RUN_FIRST,
                   gobject.TYPE_NONE,
                   (gobject.TYPE_PYOBJECT,))

# *****************************************************************************
# Setting dialog with status-bar

class SettingPage(gtk.VBox):

    def __init__(self, setting_widget=None):
        gtk.VBox.__init__(self, False)
        self.setting_widget = setting_widget
        self.wrong_keys = []

        scw = gtk.ScrolledWindow()
        scw.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

        self.sw_vbox = gtk.VBox(False)
        self.sw_vbox.set_border_width(5)
        # due to scrolled window sw_vbox must not be empty
        # the label will be removed
        self.sw_vbox.pack_start(gtk.Label())
        scw.add_with_viewport(self.sw_vbox)
        self.sw_vbox.show()
        self.pack_start(scw, True, True)
        scw.show()

        self.label_msg = gtk.Label()
        self.infobar = gtk.InfoBar()
        self.infobar.set_message_type(gtk.MESSAGE_WARNING)
        self.infobar.get_content_area().add(self.label_msg)
        self.pack_start(self.infobar, False, False)

        self.set_setting_widget(self.setting_widget)

    def set_setting_widget(self, sw):
        if sw is None:
            self.remove_settig_widget()
            return

        for child in self.sw_vbox.get_children():
            self.sw_vbox.remove(child)
        self.setting_widget = sw
        self.setting_widget.connect("value-committed",self._cb_check_value)
        self.setting_widget.connect("value-status-changed",
                                    self._cb_value_status_changed)
        self.sw_vbox.pack_start(self.setting_widget, True, True)
        self.setting_widget.show_all()
        self.set_infobar()
        self.show()

    def remove_settig_widget(self):
        if self.setting_widget is not None:
            self.sw_vbox.remove(self.setting_widget)
            self.setting_widget = None

    def get_setting(self):
        if self.setting_widget is None:
            return None
        return self.setting_widget.settings

    def are_values_correct(self):
        if self.setting_widget is None:
            return False
        return self.setting_widget.are_values_correct()

    def set_infobar(self):
        if self.are_values_correct():
            self.set_wrong_message()
            return

        for key in self.setting_widget.settings:
            msg = self.setting_widget.get_value_status_message(key)
            if msg is not None:
                self.set_wrong_message(key, msg)
                return

    def set_wrong_message(self, key=None, message=None):
        if message is None:
            if key in self.wrong_keys:
                self.wrong_keys.remove(key)
            if self.wrong_keys: # check if is it there other unsolved key
                old_key = self.wrong_keys[-1]
                msg = self.setting_widget.get_value_status_message(old_key)
                self.set_wrong_message(old_key, msg)
                return
            self.label_msg.set_text("")
            self.infobar.hide_all()
        else:
            if key not in self.wrong_keys:
                self.wrong_keys.append(key)
            self.label_msg.set_markup("<b>{0}:</b> {1}".format(
                self.setting_widget.labels[key], message))
            self.infobar.show_all()

    def _cb_value_status_changed(self, sw, key):
        msg = sw.get_value_status_message(key)
        if msg is None: # correct status immediately
            self.set_wrong_message()

    def _cb_check_value(self, sw, key):
        msg = sw.get_value_status_message(key)
        self.set_wrong_message(key, msg)


class BasicSettingDialog(gtk.Dialog):

    """Default setting dialog containing a status-bar informs about messages
    got from validators. Without buttons; they must be added manually.

    """
    def __init__(self, setting_widget, title, window=None):
        gtk.Dialog.__init__(self,
                            title=title,
                            parent=window,
                            flags=gtk.DIALOG_MODAL,
                            buttons=None)
        self.setting_widget = setting_widget
        self.setting_widget.connect(
            "value-status-changed", self._cb_value_status_changed)
        self.protected_buttons = []

        self.vbox.pack_start(SettingPage(self.setting_widget), True, True)
        self.vbox.show()

    def add_button(self, button_text, response_id, protected=False):
        """ Overridden version of method for adding buttons. This one has the
        same behavior as the super class method. But moreover it is possible
        to specify whether the button is protected or not.

        Arguments:
        button_text -- label of button
        response_id -- response after click on it (default gtk responses)

        Keywords:
        protected -- True if the button is against of wrong setting values;
                     the button is locked when the setting widget is not
                     correct (default: False).

        """
        button = gtk.Dialog.add_button(self, button_text, response_id)
        if protected:
            self.protected_buttons.append(button)
        return button

    def _cb_value_status_changed(self, setting_widget, key):
        status = setting_widget.are_values_correct()
        for button in self.protected_buttons:
            button.set_sensitive(status)


class BasicSettingAssistant(gtk.Assistant):

    def __init__(self, pages_count, title, parent=None):
        gtk.Assistant.__init__(self)
        self._response = None
        self._last_page = 0

        self.set_title(title)
        if parent is not None:
            self.set_transient_for(parent)
            self.set_position(gtk.WIN_POS_CENTER_ON_PARENT)
        self.set_modal(True)
        self.set_forward_page_func(self.__forward_page)

        self.pages_count = pages_count
        self.pages = []
        self.create_functions = []

        assert(self.pages_count > 0)
        if self.pages_count == 1:
            self.__add_empty_setting_page(gtk.ASSISTANT_PAGE_INTRO)
            self.__add_empty_summary_page()
        else:
            self.__add_empty_setting_page(gtk.ASSISTANT_PAGE_INTRO)
            for i in xrange(1, self.pages_count):
                self.__add_empty_setting_page(gtk.ASSISTANT_PAGE_CONTENT)
            self.__add_empty_summary_page()

        self.connect("apply", self._cb_apply)
        self.connect("cancel", self._cb_cancel)
        self.connect("close", self._cb_close)
        self.connect("prepare", self._cb_prepare)

    def append_setting_widget(self, title, fn_create_setting_widget):
        sp = self.pages[len(self.create_functions)]
        self.set_page_title(sp, title)
        self.create_functions.append(fn_create_setting_widget)

    def create_setting_widget(self, page_num, setting=None):
        sw = self.create_functions[page_num](setting)
        sw.connect("value-status-changed", self._cb_check_setting)
        sp = self.get_nth_page(page_num)
        sp.set_setting_widget(sw)
        self.set_page_complete(sp, sp.are_values_correct())

    def fill_summary_widget(self, smp):
        smp.reset_summary_page()

        for n in xrange(self.pages_count):
            sp = self.get_nth_page(n)
            sw = sp.setting_widget
            smp.add_page_title(self.get_page_title(sp))
            for key in sw.keys_order:
                smp.add_setting_value(sw.labels[key], sw.settings[key])
        smp.show_all()
        self.set_page_complete(smp, True)

    def reset_pages_from(self, page_num):
        for n in xrange(page_num, self.pages_count):
            sp = self.get_nth_page(n)
            sp.remove_settig_widget()

    def run(self):
        self.show_all()
        gtk.main()
        return self._response

    def _cb_apply(self, bsa):
        self.collected_setting = []
        for n in xrange(bsa.pages_count):
            sp = bsa.get_nth_page(n)
            self.collected_setting.append(sp.get_setting())
        self._response = gtk.RESPONSE_APPLY

    def _cb_cancel(self, bsa):
        bsa.destroy()
        gtk.main_quit()
        self._response = gtk.RESPONSE_CANCEL

    def _cb_close(self, bsa):
        self.destroy()
        gtk.main_quit()
        if self._response == gtk.RESPONSE_APPLY:
            self._response = gtk.RESPONSE_OK
            return
        self._response = gtk.RESPONSE_CLOSE

    def _cb_prepare(self, bsa, sp):
        csp_num = bsa.get_current_page()
        page_type = bsa.get_page_type(sp)
        if page_type == gtk.ASSISTANT_PAGE_CONFIRM:
            self.fill_summary_widget(sp)
            return

        if sp.setting_widget is not None:
            return

        if page_type == gtk.ASSISTANT_PAGE_INTRO:
            bsa.create_setting_widget(csp_num)
        elif page_type == gtk.ASSISTANT_PAGE_CONTENT:
            previous_sp = bsa.get_nth_page(csp_num - 1)
            bsa.create_setting_widget(csp_num, previous_sp.get_setting())

    def _cb_check_setting(self, sw, key):
        csp_num = self.get_current_page()
        self.set_page_complete(self.get_nth_page(csp_num),
                               sw.are_values_correct())

    def __forward_page(self, current_page):
        if self._last_page > current_page:
            '''after back step/s, it is supposed that something has changed,
               then next pages are reseted.'''
            self.reset_pages_from(current_page+1)

        self._last_page = current_page
        page = self.get_nth_page(current_page)
        if self.get_page_type(page) == gtk.ASSISTANT_PAGE_CONFIRM:
            return -1
        return current_page + 1

    def __add_empty_setting_page(self, page_type):
        sp = SettingPage()
        self.pages.append(sp)
        self.append_page(sp)
        self.set_page_type(sp, page_type)

    def __add_empty_summary_page(self):
        smp = self.SummaryPage()
        self.append_page(smp)
        self.set_page_type(smp, gtk.ASSISTANT_PAGE_CONFIRM)
        self.set_page_title(smp, "Configuration summary")


    class SummaryPage(gtk.ScrolledWindow):

        def __init__(self):
            gtk.ScrolledWindow.__init__(self)
            self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

            self.table = gtk.Table(1, 2, False)
            # due to scrolled window table must not be empty
            # the label will be removed
            self.table.attach(gtk.Label(), 0, 1, 0,1)

            self.add_with_viewport(self.table)
            self.row = 0

        def add_page_title(self, title):
            label = gtk.Label()
            label.set_markup("<b>{0}</b>".format(title))
            label.set_alignment(0, 1)
            self.table.attach(label,
                              0, 2,
                              self.row, self.row+1,
                              xoptions=gtk.FILL, yoptions=0,
                              xpadding=10, ypadding=10)

            self.row += 1
            self.table.resize(self.row, 2)

        def add_setting_value(self, vlabel, value):
            label = gtk.Label()
            label.set_markup("<i>{0}:</i>".format(vlabel))
            label.set_alignment(1, 1)
            self.table.attach(label,
                              0, 1,
                              self.row, self.row+1,
                              xoptions=gtk.FILL, yoptions=0,
                              xpadding=10, ypadding=3)

            label = gtk.Label(repr(value))
            label.set_alignment(0, 1)
            self.table.attach(label,
                              1, 2,
                              self.row, self.row+1,
                              xoptions=gtk.FILL, yoptions=0,
                              xpadding=0, ypadding=3)

            self.row += 1
            self.table.resize(self.row, 2)

        def reset_summary_page(self):
            for child in self.table.get_children():
                self.table.remove(child)
            self.table.resize(1, 2)
