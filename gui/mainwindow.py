#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                  2011       Ondrej Garncarz
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
import pango

class MainWindow(gtk.Window):

	def __init__(self, app):
		gtk.Window.__init__(self)
		self.app = app
		self.set_title("Kaira")
		self.connect("destroy", gtk.main_quit)
		self.tablist = []

		vbox = gtk.VBox()
		self.add(vbox)
		vbox.pack_start(self._create_main_menu(), False, False)

		paned = gtk.VPaned()
		vbox.pack_start(paned)
		self.notebook = gtk.Notebook()
		self.notebook.set_scrollable(True)
		paned.pack1(self.notebook, True)

		self.console = Console()
		paned.pack2(self.console, False)

		vbox.show_all()

	def project_is_active(self, value):
		for w in self.project_sensitives:
			w.set_sensitive(value)

	def add_tab(self, tab, switch = True):
		if tab.has_close_button():
			button = gtk.Button()
			button.set_relief(gtk.RELIEF_NONE)
			button.set_focus_on_click(False)
			icon = gtk.image_new_from_stock(gtk.STOCK_CLOSE, gtk.ICON_SIZE_MENU)

			style = gtk.RcStyle()
			style.xthickness = 0
			style.ythickness = 0
			button.modify_style(style)
			button.add(icon)
			button.connect("clicked", lambda w: tab.close())
			w = gtk.HBox(False, 0)
			w.pack_start(gtk.Label(tab.get_name()))
			w.pack_start(button, False, False)
			w.show_all()
		else:
			w = gtk.Label(tab.get_name())
		self.notebook.append_page(tab.get_widget(), w)
		self.notebook.set_tab_reorderable(tab.get_widget(), True)
		tab.get_widget().show()
		tab.window = self
		self.tablist.append(tab)
		if switch:
			self.switch_to_tab(tab)

	def foreach_tab(self, fn):
		for tab in self.tablist[:]:
			fn(tab)

	def switch_to_tab(self, tab):
		num = self.notebook.page_num(tab.get_widget())
		self.notebook.set_current_page(num)

	def switch_to_tab_by_key(self, key, fn = None):
		for tab in self.tablist:
			if tab.get_key() == key:
				self.switch_to_tab(tab)
				if fn:
					fn(tab)
				return True
		return False

	def close_tab(self, tab):
		num = self.notebook.page_num(tab.get_widget())
		self.notebook.remove_page(num)
		self.tablist.remove(tab)

	def close_all_tabs(self):
		for tab in self.tablist[:]:
			tab.close()

	def current_tab(self):
		widget = self.notebook.get_nth_page(self.notebook.get_current_page())
		for tab in self.tablist:
			if tab.get_widget() == widget:
				return tab

	def _create_main_menu(self):
		ag = gtk.AccelGroup()
		self.add_accel_group(ag)

		self.project_sensitives = []
		file_menu = gtk.Menu()

		item = gtk.MenuItem("_New project")
		item.connect("activate", lambda w: self.app.new_project())
		file_menu.append(item)

		item = gtk.MenuItem("_Open project")
		item.connect("activate", lambda w: self.app.load_project())
		file_menu.append(item)

		item = gtk.MenuItem("_Save project")
		item.connect("activate", lambda w: self.app.save_project())
		self.project_sensitives.append(item)
		file_menu.append(item)

		item = gtk.MenuItem("Save project _as")
		item.connect("activate", lambda w: self.app.save_project_as())
		self.project_sensitives.append(item)
		file_menu.append(item)

		file_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("_Connect to application")
		item.connect("activate", lambda w: self.app.connect_to_application())
		file_menu.append(item)

		item = gtk.MenuItem("Open lo_g")
		item.connect("activate", lambda w: self.app.load_log())
		file_menu.append(item)

		file_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("_Quit")
		item.connect("activate", gtk.main_quit)
		file_menu.append(item)

		build_menu = gtk.Menu()

		item = gtk.MenuItem("B_uild project")
		item.connect("activate", lambda w: self.app.build_project())
		build_menu.append(item)

		item = gtk.MenuItem("Run _simulation")
		item.connect("activate", lambda w: self.app.simulation_start(False))
		item.add_accelerator("activate", ag, gtk.gdk.keyval_from_name("F7"), 0, gtk.ACCEL_VISIBLE)
		build_menu.append(item)

		item = gtk.MenuItem("R_e-run simulation")
		item.connect("activate", lambda w: self.app.simulation_start(True))
		item.add_accelerator("activate", ag, gtk.gdk.keyval_from_name("F8"), 0, gtk.ACCEL_VISIBLE)
		build_menu.append(item)

		build_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("Run _simulation in Valgrind")
		item.connect("activate", lambda w: self.app.simulation_start(False, valgrind = True))
		build_menu.append(item)

		view_menu = gtk.Menu()

		item = gtk.RadioMenuItem(None, "No grid")
		item.connect("activate", lambda w: self.app.set_grid_size(1))
		item.set_active(True)
		view_menu.append(item)

		item = gtk.RadioMenuItem(item, "Small grid (5x5)")
		item.connect("activate", lambda w: self.app.set_grid_size(5))
		view_menu.append(item)

		item = gtk.RadioMenuItem(item, "Big grid (25x25)")
		item.connect("activate", lambda w: self.app.set_grid_size(25))
		view_menu.append(item)

		view_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("Hide error messages")
		item.connect("activate", lambda w: self.app.hide_error_messages())
		view_menu.append(item)

		view_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("Close tab")
		item.connect("activate", lambda w: self.app.close_current_tab())
		item.add_accelerator("activate", ag, gtk.gdk.keyval_from_name("W"), gtk.gdk.CONTROL_MASK, gtk.ACCEL_VISIBLE)
		view_menu.append(item)

		edit_menu = gtk.Menu()

		item = gtk.MenuItem("Edit _project details")
		item.connect("activate", lambda w: self.app.project_config())
		edit_menu.append(item)

		item = gtk.MenuItem("Edit _head file")
		item.connect("activate", lambda w: self.app.edit_headfile())
		edit_menu.append(item)

		edit_menu.append(gtk.SeparatorMenuItem())

		item = gtk.MenuItem("Edit _settings")
		item.connect("activate", lambda w: self.app.edit_settings())
		edit_menu.append(item)

		main_menu = gtk.MenuBar()
		item = gtk.MenuItem("_Project")
		item.set_submenu(file_menu)
		main_menu.append(item)
		item = gtk.MenuItem("_View")
		item.set_submenu(view_menu)
		self.project_sensitives.append(item)
		main_menu.append(item)
		item = gtk.MenuItem("_Edit")
		item.set_submenu(edit_menu)
		self.project_sensitives.append(item)
		main_menu.append(item)
		item = gtk.MenuItem("_Run")
		item.set_submenu(build_menu)
		self.project_sensitives.append(item)
		main_menu.append(item)
		return main_menu


class Console(gtk.ScrolledWindow):

	def __init__(self):
		gtk.ScrolledWindow.__init__(self)
		self.id_counter = 0
		self.link_callbacks = {}
		self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
		self.set_shadow_type(gtk.SHADOW_IN)
		self.textview = gtk.TextView()
		self.textview.set_editable(False)
		self.textview.connect("button-press-event", self._button_down)
		self.textview.connect("motion_notify_event", self._mouse_move)
		font_desc = pango.FontDescription('monospace')
		if font_desc:
			self.textview.modify_font(font_desc)
		self.buffer = self.textview.get_buffer()
		self.buffer.create_tag("normal")
		self.buffer.create_tag("output", foreground="blue")
		self.buffer.create_tag("success", foreground="darkgreen")
		self.buffer.create_tag("error", foreground="red")

		# Do not use tag "link" directly in method write, use always method "write_link"
		self.link_tag =	self.buffer.create_tag("link", underline=True)
		self.link_hidden_tag = self.buffer.create_tag("link_hidden", invisible = True)
		self.add(self.textview)

	def reset(self):
		self.buffer.set_text("")
		self.id_counter = 0
		self.link_callbacks = {}

	def write(self, text, tag_name="normal"):
		self.buffer.insert_with_tags_by_name(self.buffer.get_end_iter(), text, tag_name)
		self.textview.scroll_to_iter(self.buffer.get_end_iter(),0.0)

	def write_link(self, text, callback):
		new_id = str(self.id_counter)
		self.link_callbacks[new_id] = callback
		self.id_counter += 1
		self.write(text, "link")
		self.write(new_id, "link_hidden")

	def _iter_at_position(self, px, py):
		px, py = self.textview.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET, int(px), int(py))
		return self.textview.get_iter_at_location(px, py)

	def _button_down(self, w, event):
		i = self._iter_at_position(event.x, event.y)
		if i.has_tag(self.link_tag):
			i.forward_to_tag_toggle(self.link_tag)
			j = i.copy()
			j.forward_to_tag_toggle(self.link_hidden_tag)
			self.link_callbacks[self.buffer.get_text(i, j, True)]()
			return True
		else:
			return False

	def _mouse_move(self, w, event):
		i = self._iter_at_position(event.x, event.y)
		if i.has_tag(self.link_tag):
			cursor = gtk.gdk.Cursor(gtk.gdk.FLEUR)
		else:
			cursor = None
		w = self.textview.get_window(gtk.TEXT_WINDOW_TEXT)
		w.set_cursor(cursor)

class Tab:

	window = None

	def __init__(self, name, widget, key = None, has_close_button = True):
		self.name = name
		self.widget = widget
		self.key = key
		self.close_button = has_close_button

	def get_widget(self):
		return self.widget

	def get_key(self):
		return self.key

	def get_name(self):
		return self.name

	def has_close_button(self):
		return self.close_button

	def close(self):
		self.window.close_tab(self)

	def project_save(self):
		pass

	def project_export(self):
		pass
