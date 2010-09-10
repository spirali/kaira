import gtk
import pango

class MainWindow(gtk.Window):

	def __init__(self, app):
		gtk.Window.__init__(self)
		self.app = app
		self.set_title("Kaira")
		self.connect("destroy", gtk.main_quit)

		vbox = gtk.VBox()
		self.add(vbox)
		vbox.pack_start(self._create_main_menu(), False, False)

		paned = gtk.VPaned()
		vbox.pack_start(paned)
		self.notebook = gtk.Notebook()
		paned.pack1(self.notebook, True)

		self.console = Console()
		paned.pack2(self.console, False)

		vbox.show_all()


	def add_tab(self, name, widget, close_callback = None):
		if close_callback:
			button = gtk.Button()
			button.set_relief(gtk.RELIEF_NONE)
			button.set_focus_on_click(False)
			icon = gtk.image_new_from_stock(gtk.STOCK_CLOSE, gtk.ICON_SIZE_MENU)

			style = gtk.RcStyle()
			style.xthickness = 0
			style.ythickness = 0
			button.modify_style(style)
			button.add(icon)
			button.connect("clicked", close_callback)
			w = gtk.HBox(False, 0)
			w.pack_start(gtk.Label(name))
			w.pack_start(button, False, False)
			w.show_all()
		else:
			w = gtk.Label(name)
		self.notebook.append_page(widget, w)
		widget.show()

	def switch_to_tab(self, widget):
		num = self.notebook.page_num(widget)
		self.notebook.set_current_page(num)

	def close_tab(self, widget):
		num = self.notebook.page_num(widget)
		self.notebook.remove_page(num)

	def _create_main_menu(self):
		file_menu = gtk.Menu()
		
		item = gtk.MenuItem("_New project")
		item.connect("activate", lambda w: self.app.new_project())
		file_menu.append(item)

		item = gtk.MenuItem("_Open project")
		item.connect("activate", lambda w: self.app.load_project())
		file_menu.append(item)

		item = gtk.MenuItem("_Save project")
		item.connect("activate", lambda w: self.app.save_project_as())
		file_menu.append(item)

		item = gtk.MenuItem("Save project _as")
		item.connect("activate", lambda w: self.app.save_project_as())
		file_menu.append(item)

		item = gtk.MenuItem("_Quit")
		item.connect("activate", gtk.main_quit)
		file_menu.append(item)

		build_menu = gtk.Menu()

		item = gtk.MenuItem("B_uild project")
		item.connect("activate", lambda w: self.app.build_project())
		build_menu.append(item)

		item = gtk.MenuItem("Run _simulation")
		item.connect("activate", lambda w: self.app.simulation_start())
		build_menu.append(item)

		edit_menu = gtk.Menu()

		item = gtk.MenuItem("Edit _parameters")
		item.connect("activate", lambda w: self.app.parameters_edit())
		edit_menu.append(item)


		tool_menu = gtk.Menu()
		item = gtk.MenuItem("Export network to SVG")
		tool_menu.append(item)

		main_menu = gtk.MenuBar()
		item = gtk.MenuItem("_Project")
		item.set_submenu(file_menu)
		main_menu.append(item)
		item = gtk.MenuItem("_Edit")
		item.set_submenu(edit_menu)
		main_menu.append(item)
		item = gtk.MenuItem("_Run")
		item.set_submenu(build_menu)
		main_menu.append(item)
		item = gtk.MenuItem("_Tools")
		item.set_submenu(tool_menu)
		main_menu.append(item)
		return main_menu

	def _dummy(self):
		pass


class Console(gtk.ScrolledWindow):

	def __init__(self):
		gtk.ScrolledWindow.__init__(self)
		self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
		self.set_shadow_type(gtk.SHADOW_IN)
		self.textview = gtk.TextView()
		self.textview.set_editable(False)
		font_desc = pango.FontDescription('monospace')
		if font_desc:
			self.textview.modify_font(font_desc)
		self.buffer = self.textview.get_buffer()
		self.add(self.textview)

	def write(self, text):
		self.buffer.insert(self.buffer.get_end_iter(), text)
		self.textview.scroll_to_iter(self.buffer.get_end_iter(),0.0)
