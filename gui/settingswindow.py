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
        self.set_col_spacing(0, 10)
        self.settings = dict() # key: value
        self.value_status = dict() # key: (true|false, message)
        self.widgets = dict() # key: widget
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
                   default_value,
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
        self.settings[key] = default_value
        self.value_status[key] = (True, None)

        lbl = gtk.Label(label)
        lbl.set_alignment(0.0, 0.5)
        self.attach(
            lbl,
            0, 1,
            self.row, self.row+1,
            xoptions=gtk.FILL, yoptions=0,
            xpadding=5)

        widget.connect("focus-in-event", self._cb_focus, key)
        self.attach(
            widget,
            1, 2,
            self.row, self.row+1,
            xoptions=gtk.EXPAND|gtk.FILL, yoptions=0,
            xpadding=5)

        self.row += 1
        self.resize(self.row+1, 2)
        self.widgets[key] = widget

    def add_separator(self):
        self.attach(
            gtk.HSeparator(),
            0, 2,
            self.row, self.row+1,
            xoptions=gtk.EXPAND|gtk.FILL, yoptions=0,
            xpadding=5, ypadding=5)

        self.row += 1
        self.resize(self.row+1, 2)

    def _cb_focus(self, widget, event, key):
        self.emit("select-key", key)

    # -------------------------------------------------------------------------
    # add specific components

    def add_entry(self,
                  key,
                  label,
                  default_value,
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
                    key, False, "The string cannot be convert to the value.");

        entry = gtk.Entry()
        entry.set_text(str(default_value))
        std_color = entry.get_style().text[gtk.STATE_NORMAL]
        entry.connect("changed", callback, key, std_color)
        self.add_widget(key, label, default_value, entry, validator)

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

        assert(default < len(items))
        combo.set_active(default)
        default_value = items[default][1]
        self.add_widget(key, label, default_value, combo)

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

        def callback(button, key, value):
            self.settings[key] = value

        vbox, hbox = gtk.VBox(), gtk.HBox()
        idx, button = 0, None
        for vlabel, value in items:
            button = gtk.RadioButton(button, vlabel)
            button.connect("toggled", callback, key, value)
            if idx == default:
                button.set_active(True)
            hbox.pack_start(button, False, False, 3)
            idx += 1
            if idx % ncols == 0:
                vbox.pack_start(hbox, False, False)
                hbox = gtk.HBox()
        if idx % ncols != 0: # add last unprocessed row
            vbox.pack_start(hbox, False, False)
        vbox.show_all()

        assert(default < len(items))
        default_value = items[default][1]
        self.add_widget(key, label, default_value, vbox)


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

        self.settings[key] = []
        def callback(button, key, value):
            if value in self.settings[key]:
                self.settings[key].remove(value)
            else:
                self.settings[key].append(value)

        vbox, hbox = gtk.VBox(), gtk.HBox()
        idx = 0
        for vlabel, value in items:
            button = gtk.CheckButton(vlabel)
            button.connect("toggled", callback, key, value)
            if idx in defaults:
                button.set_active(True)
            hbox.pack_start(button, False, False, 3)
            idx += 1
            if idx % ncols == 0:
                vbox.pack_start(hbox, False, False)
                hbox = gtk.HBox()
        if idx % ncols != 0: # add last unprocessed row
            vbox.pack_start(hbox, False, False)

        assert(all([index < len(items) for index in defaults]))
        defaults_values = [items[default][1] for default in defaults]
        self.add_widget(key, label, defaults_values, vbox)

    # -------------------------------------------------------------------------
    # add specific data types

    def add_int(self, key, label, default_value):
        self.add_entry(key, label, default_value, strToValue=int)

    def add_positive_int(self, key, label, default_value):
        def validator(value):
            if value <= 0:
                return "The number is not greater than zero."
            return None

        self.add_entry(key, label, default_value, validator, int)


# register new signals to setting widget
gobject.type_register(SettingsWidget)
gobject.signal_new("value-status-changed",
                   SettingsWidget,
                   gobject.SIGNAL_RUN_FIRST,
                   gobject.TYPE_NONE,
                   (gobject.TYPE_STRING,))
gobject.signal_new("select-key",
                   SettingsWidget,
                   gobject.SIGNAL_RUN_FIRST,
                   gobject.TYPE_NONE,
                   (gobject.TYPE_STRING,))

# *****************************************************************************
# Setting dialog with status-bar

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
        self.setting = setting_widget
        self.setting.connect(
            "value-status-changed", self._cb_value_status_changed)
        self.protected_buttons = []

        self.vbox.pack_start(self.setting, False, False)
        self.vbox.pack_start(gtk.HSeparator(), False, False, 3)
        self.vbox.show_all()

    def add_protected_button(self, button):
        """Protected buttons are locked if some of values is not correct.

        Arguments:
        button -- a button which will be protected

        """
        self.protected_buttons.append(button)

    def remove_protected_button(self, button):
        self.protected_buttons.remove(button)

    def _cb_value_status_changed(self, setting, key):
        status = setting.are_values_correct()
        for button in self.protected_buttons:
            button.set_sensitive(status)
        widget = setting.widgets[key]
        status_message = setting.get_value_status_message(key)
        if widget.get_has_tooltip() and status_message == None:
            widget.set_has_tooltip(False)
        else:
            widget.set_tooltip_text(status_message)
