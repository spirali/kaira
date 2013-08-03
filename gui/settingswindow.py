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

    warning_color = gtk.gdk.color_parse("#f66")

    def __init__(self):
        gtk.Table.__init__(self, 1, 2, False)
        self.set_col_spacing(0, 10)
        self.settings = dict() # key: value
        self.value_status = dict() # key: (true|false, message)
        self.row = 0 # count of parameters

    def set(self, key, value):
        self.settings[key] = value

    def get(self, key):
        return self.settings[key]

    def set_value_status(self, key, status, message=""):
        old_status, old_message = self.value_status[key]
        if old_status != status or old_message != message:
            self.value_status[key] = (status, message)
            self.emit("value-status-changed", key)

    def is_value_correct(self, key):
        return self.value_status[key][0]

    def are_values_correct(self):
        return all([status
                    for key, (status, msg) in self.value_status.items()])

    def get_value_status_message(self, key):
        return self.value_status[key][1]


# *****************************************************************************
# Level 0

    def add_widget(self,
                   key,
                   label,
                   default_value,
                   widget,
                   validator=lambda x: None):

        assert(key not in settings)
        self.settings[key] = default_value
        self.value_status[key] = (True, "")

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

# *****************************************************************************
# Level 1

    def add_entry(self,
                  key,
                  label,
                  default_value,
                  validator=lambda x: None,
                  strToValue=lambda x: x):

        def callback(editable, key, std_color):
            try:
                value = strToValue(editable.get_text())
                message = validator(value)
                if message is not None:
                    editable.modify_text(
                        gtk.STATE_NORMAL, self.warning_color)
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
        default_value = None
        if items and default < len(items):
            combo.set_active(default)
            default_value = items[default][1]

        combo.connect("changed", callback, key, items, default=0)
        self.add_widget(key, label, default_value, combo)

    def add_radiobuttons(self, key, label, items, default=0, ncols=1):
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
        if idx % ncols != 0:
            vbox.pack_start(hbox, False, False)
        vbox.show_all()

        if default < len(items):
            default_value = items[default][1]
        else:
            default_value = items[0][1]

        self.add_widget(key, label, default_value, vbox)


    def add_checkbuttons(self, key, label, items, defaults=[0], ncols=1):
        """ Add to setting widget a list of check-box buttons and in the
        settings it is represented by a list of selected values.

        Arguments:
        key -- unique key
        label -- the showed label in setting widget
        items -- couple: (label, object)
        defaults -- a list of indexes in items
        ncols -- a number of check-boxes which should be in one line

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
        if idx % ncols != 0:
            vbox.pack_start(hbox, False, False)

        defaults_values = [items[default][1] for default in defaults]
        self.add_widget(key, label, defaults_values, vbox)


# *****************************************************************************
# Level 1

    def add_int(self, key, label, default_value):
        self.add_entry(key, label, default_value, strToValue=int)

    def add_positive_int(self, key, label, default_value):
        def validator(value):
            if value <= 0:
                return "The number is not greater than zero."
            return None

        self.add_entry(key, label, default_value, validator, int)


# register new signal
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


class SettingDialog(gtk.Dialog):

    def __init__(self, setting_widget, title, window=None):
        gtk.Dialog.__init__(self,
                            title=title,
                            parent=window,
                            flags=gtk.DIALOG_MODAL,
                            buttons=None)
        self.setting = setting_widget
        self.setting.connect(
            "value-status-changed", self._cb_value_status_changed)
        self.setting.connect(
            "select-key", self._cb_select_value)

        self.protected_buttons = []

        self.statusbar = gtk.Statusbar()

        self.vbox.pack_start(self.setting, False, False)
        self.vbox.pack_start(self.statusbar, False, False, 5)
        self.vbox.pack_start(gtk.HSeparator())
        self.vbox.show_all()
        for child in self.get_content_area().get_children():
            print child

    def add_protected_button(self, button):
        self.protected_buttons.append(button)

    def remove_protected_button(self, button):
        self.protected_buttons.remove(button)

    def _cb_value_status_changed(self, setting, key):
        status = setting.are_values_correct()
        for button in self.protected_buttons:
            button.set_sensitive(status)
        self.statusbar.push(hash(key), setting.get_value_status_message(key))

    def _cb_select_value(self, setting, key):
        self.statusbar.push(hash(key), settings.get_value_status_message(key))

# *****************************************************************************
# Test
# *****************************************************************************
def cb(widget, key, button):
    button.set_sensitive(widget.are_values_correct())

settings = SettingsWidget()
settings.add_int("k", "K", 3)
settings.add_separator()
settings.add_positive_int("b", "Positive int", 3)
settings.add_entry("str", "String", "")
settings.add_checkbuttons("item", "Items", [('one', 1), ('two', 2), ('three', 3), ('four', 4), ('five', 5)], [0,2], 2)


dialog = SettingDialog(settings, "Setting")
button = dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
dialog.add_protected_button(button)
response = dialog.run()
if response == gtk.RESPONSE_OK:
    for key in settings.settings:
        print key, " = ", settings.settings[key]
    dialog.destroy()
