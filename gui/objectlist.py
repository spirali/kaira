#
#    Copyright (C) 2010 Stanislav Bohm
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

import gtkutils
import gtk

class ObjectContainer(gtk.VBox):

    def __init__(self, container, buttons=None, has_context_menu=False):
        gtk.VBox.__init__(self)

        if buttons:
            box = gtk.HButtonBox()
            box.set_layout(gtk.BUTTONBOX_START)

            for (name, stock, callback) in buttons:
                if name is None:
                    button = gtk.Button(stock=stock)
                else:
                    button = gtk.Button(name)
                button.connect("clicked", self._callback(callback))
                box.add(button)

            self.pack_start(box, False, False)


        self.container = container
        self.container.connect_view("cursor-changed",
            lambda w: self.cursor_changed(self.container.get_selection(0)))
        self.container.connect_view("row-activated",
            lambda w, i, p: self.row_activated(self.container.get_selection(0)))

        self.has_context_menu = has_context_menu
        if has_context_menu:
            self.container.view.set_events(gtk.gdk.BUTTON_PRESS_MASK)
            self.container.view.connect("button_press_event", self._button_down)

        self.pack_start(self.container)
        self.show_all()

    def hide_headers(self):
        self.container.hide_headers()

    def _callback(self, callback):
        return lambda w: callback(self.container.get_selection(0))

    def selected_object(self):
        return self.container.get_selection(0)

    def select_object(self, obj):
        i = self.container.find(obj, 0)
        if i is not None:
            self.container.select_iter(i)
        else:
            self.select_first()

    def select_first(self):
        self.container.select_first()

    def get_and_remove_selected(self):
        return self.container.get_and_remove_selection(0)

    def update_selected(self, obj):
        self.container.set_selection_all(self.object_as_row(obj))

    def update(self, obj):
        i = self.container.find(obj, 0)
        if i is not None:
            self.container.set_all(self.object_as_row(obj), i)

    def refresh(self, data):
        obj = self.selected_object()
        self.clear()
        self.fill(data)
        self.container.view.expand_all()
        if obj:
            self.select_object(obj)
        else:
            self.select_first()
        self.cursor_changed(self.container.get_selection(0))

    def clear(self):
        self.container.clear()

    def cursor_changed(self, obj):
        pass

    def row_activated(self, obj):
        pass

    def _button_down(self, w, event):
        def call(f):
            return lambda w: f(self.selected_object())
        if event.button == 3 and self.has_context_menu:
            x = int(event.x)
            y = int(event.y)
            pathinfo = self.container.view.get_path_at_pos(x, y)
            if pathinfo is not None:
                path, col, cellx, celly = pathinfo
                self.container.view.grab_focus()
                self.container.view.set_cursor(path, col, 0)
            menu = self.get_context_menu()
            if menu is not None:
                gtkutils.show_context_menu(menu, event)
                return True

class ObjectList(ObjectContainer):

    def __init__(self, list_definition, *args, **kw):
        container = gtkutils.SimpleList(list_definition)
        ObjectContainer.__init__(self, container, *args, **kw)

    def fill(self, data):
        for obj in data:
            self.add_object(obj)

    def add_object(self, obj):
        return self.container.append(self.object_as_row(obj))

class ObjectTree(ObjectContainer):

    def __init__(self, list_definition, *args, **kw):
        container = gtkutils.SimpleTree(list_definition)
        ObjectContainer.__init__(self, container, *args, **kw)

    def fill(self, data, parent = None):
        for item in data:
            if isinstance(item, tuple):
                obj, content = item
                parent = self.add_object(parent, obj)
                self.fill(content, parent)
            else:
                self.add_object(parent, item)

    def add_object(self, parent, obj):
        return self.container.append(parent, self.object_as_row(obj))
