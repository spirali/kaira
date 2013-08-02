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

    def __init__(self, name=None):
        gtk.Table.__init__(self, 1, 2, False)
        print len(dir(self))
        self.set_col_spacing(0, 10)
        self.widget_name = name
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
        self.value_status[key] = (True, None)

        lbl = gtk.Label(label)
        lbl.set_alignment(0.0, 0.5)
        self.attach(
            lbl,
            0, 1,
            self.row, self.row+1,
            xoptions=gtk.FILL, yoptions=0,
            xpadding=5)

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

    def add_combobox(self, key, label, items):
        """Adds to a setting widget a combo-box where it can be selected one
        of the items.

        Arguments:
        key -- unique key
        label -- the showed label in setting widget
        items -- couple: (label, object)

        """
        pass

# *****************************************************************************
# Level 1

    def add_int(self, key, label, default_value):
        self.add_entry(key, label, default_value, strToValue=int)

    def add_positive_int(self, key, label, default_value):
        def validator(value):
            if value < 0:
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


# *****************************************************************************
# Test
# *****************************************************************************

def cb(widget, key, button):
    print "MSG: ", widget.get_value_status_message(key)
    button.set_sensitive(widget.are_values_correct())

settings = SettingsWidget("Tests settings")
settings.add_int("k", "K", 3)
settings.add_separator()
settings.add_positive_int("b", "Positive int", 3)
settings.add_entry("str", "String", "")


dialog = gtk.Dialog(title=settings.widget_name,
                    parent=None,
                    flags=gtk.DIALOG_MODAL,
                    buttons=None)
button_ok = dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK)
dialog.vbox.pack_start(settings, True, True)
dialog.vbox.show_all()

settings.connect("value-status-changed", cb, button_ok)
response = dialog.run()
if response == gtk.RESPONSE_OK:
    print "OK"
    dialog.destroy()
