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

class ObjectList(gtk.VBox):

	def __init__(self, list_definition, buttons, rpanel = None):
		gtk.VBox.__init__(self)

		box = gtk.HButtonBox() 
		box.set_layout(gtk.BUTTONBOX_START)
	
		for (name, stock, callback) in buttons:
			if name is None:
				button = gtk.Button(stock = stock)
			else:
				button = gtk.Button(name)
			button.connect("clicked", self._callback(callback))
			box.add(button)

		self.pack_start(box, False, False)

		self.list = gtkutils.SimpleList(list_definition)
		self.list.connect_view("cursor-changed", lambda w: self.cursor_changed(self.list.get_selection(0)))
		if rpanel is None:
			self.pack_start(self.list)
		else:
			hbox = gtk.HBox()
			hbox.pack_start(self.list)
			hbox.pack_start(rpanel, False, False)
			self.pack_start(hbox)
		self.show_all()

	def _callback(self, callback):
		return lambda w: callback(self.list.get_selection(0))

	def selected_object(self):
		return self.list.get_selection(0)

	def add_object(self, obj):
		self.list.append(self.object_as_row(obj))

	def get_and_remove_selected(self):
		return self.list.get_and_remove_selection(0)

	def update_selected(self, obj):
		self.list.set_selection_all(self.object_as_row(obj))

	def update(self, obj):
		i = self.list.find(obj, 0)
		if i is not None:
			self.list.set_all(self.object_as_row(obj), i)

	def fill(self, obj_list):
		for obj in obj_list:
			self.add_object(obj)

	def cursor_changed(self, obj):
		pass
