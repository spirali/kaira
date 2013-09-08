#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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
import os
import paths

def escape_menu_name(name):
    return name.replace("_", " ")

def build_menu(description, use_underline=True):
    menu = gtk.Menu()
    for name, action in description:
        if name == "-":
            menu.append(gtk.SeparatorMenuItem())
            continue
        if isinstance(action, tuple):
            check, action = action
            item = gtk.CheckMenuItem(name)
            item.set_active(check)
        else:
            item = gtk.MenuItem(name, use_underline)
        if isinstance(action, list):
            item.set_submenu(build_menu(action, use_underline))
        else:
            item.connect("activate", action)
        menu.append(item)
    return menu

def show_context_menu(menu_actions, event):
    menu = build_menu(menu_actions, False)
    menu.show_all()
    menu.popup(None, None, None, event.button, event.get_time())

def load_ui(filename):
    builder = gtk.Builder()
    builder.add_from_file(os.path.join(paths.UI_DIR, filename + ".glade"))
    return builder

def radio_buttons(items, select_key, box, callback):
    """ Items: [(key, label)]
        callback is called with key if button is pressed"""
    def toggled(w):
        if not w.get_active():
            return
        for key, label in items:
            if label == w.get_label():
                callback(key)
                break

    buttons = []
    for key, label in items:
        button = gtk.RadioButton(label = label)
        buttons.append(button)
        box.pack_start(button, False, False)
        button.connect("toggled", toggled)

    for button in buttons[1:]:
        button.set_group(buttons[0])

    for (key, label), button in zip(items, buttons):
        if key == select_key:
            button.set_active(True)

class SimpleComboBox(gtk.ComboBox):

    def __init__(self, values):
        store = gtk.ListStore(object, str)
        gtk.ComboBox.__init__(self, store)
        self.values = values
        for v in values:
            store.append(v)
        cell = gtk.CellRendererText()
        self.pack_start(cell, True)
        self.add_attribute(cell, 'text', 1)
        if values:
            self.set_active(0)

    def get_object(self):
        return self.values[self.get_active()][0]

    def set_object(self, obj):
        for i, (o, name) in enumerate(self.values):
            if o == obj:
                self.set_active(i)
                return

class SimpleListBase(gtk.ScrolledWindow):

    def __init__(self, columns, store_class):
        """ Columns list of tuples: (name, type) """
        gtk.ScrolledWindow.__init__(self)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        self.set_shadow_type(gtk.SHADOW_ETCHED_OUT)

        self.store = store_class(*[ c[1] for c in columns ])
        self.view = gtk.TreeView(self.store)
        self.add(self.view)

        for i, (cname, ctype) in enumerate(columns):
            if cname != "_":
                tokens = cname.split("|")
                args = {}
                markup = False
                for j, parameter in enumerate(tokens[1:]):
                    if "markup" == parameter:
                        markup = True
                    else:
                        args[parameter] = i + j + 1

                if ctype is bool:
                    renderer = gtk.CellRendererToggle()
                    args["active"] = i
                else:
                    renderer = gtk.CellRendererText()
                    if markup:
                        args["markup"] = i
                    else:
                        args["text"] = i
                column = gtk.TreeViewColumn(tokens[0], renderer, **args)
                self.view.append_column(column)

        self.view.show()

    def connect_view(self, signal_name, callback):
        self.view.connect(signal_name, callback)

    def clear(self):
        self.store.clear()

    def unselect_all(self):
        self.view.get_selection().unselect_all()

    def get_selection_path(self):
        selection = self.view.get_selection()
        model, i = selection.get_selected()
        if i is None:
            return None
        return model.get_path(i)

    def get_selection(self, column):
        selection = self.view.get_selection()
        model, i = selection.get_selected()
        if i is not None:
            return model.get_value(i, column)
        else:
            return None

    def set_selection_all(self, data):
        model, i = self.view.get_selection().get_selected()
        if i is not None:
            self.set_all(data, i)

    def set_all(self, data, i):
        for x, d in enumerate(data):
            self.store.set_value(i, x, d)

    def remove_selection(self):
        model, i = self.view.get_selection().get_selected()
        if i is not None:
            model.remove(i)

    def get_and_remove_selection(self, column):
        model, i = self.view.get_selection().get_selected()
        if i is not None:
            v = model.get_value(i, column)
            model.remove(i)
            return v
        return None

    def get_column(self, column):
        return [ self.store.get_value(row.iter, column) for row in self.store ]

    def select_iter(self, iter):
        self.view.get_selection().select_iter(iter)

    def select_first(self):
        i = self.store.get_iter_first()
        if i is not None:
            self.select_iter(i)
            return True
        else:
            return False

    def fill(self, rows):
        for row in rows:
            self.append(row)

    def hide_headers(self):
        self.view.set_headers_visible(False)


class SimpleList(SimpleListBase):

    def __init__(self, columns):
        """ Columns list of tuples: (name, type) """
        SimpleListBase.__init__(self, columns, gtk.ListStore)

    def append(self, data):
        return self.store.append(data)

    def find(self, obj, column):
        i = self.store.get_iter_first()
        while i is not None:
            if self.store.get_value(i, column) == obj:
                return i
            i = self.store.iter_next(i)
        return None


class SimpleTree(SimpleListBase):

    def __init__(self, columns):
        SimpleListBase.__init__(self, columns, gtk.TreeStore)

    def append(self, parent, data):
        return self.store.append(parent, data)

    def find(self, obj, column):
        def scan(i):
            while i is not None:
                j = scan(self.store.iter_children(i))
                if j is not None:
                    return j
                if self.store.get_value(i, column) == obj:
                    return i
                i = self.store.iter_next(i)
        return scan(self.store.get_iter_first())
