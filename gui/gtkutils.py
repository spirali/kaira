import gtk

def build_menu(description):
	menu = gtk.Menu()
	for name, action in description:
		item = gtk.MenuItem(name)
		item.connect("activate", action)
		menu.append(item)
	return menu


class SimpleList(gtk.ScrolledWindow):
	
	def __init__(self, columns):
		""" Columns list of tuples: (name, type) """
		gtk.ScrolledWindow.__init__(self)
		self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

		self.liststore = gtk.ListStore(*[ c[1] for c in columns ])
		self.listview = gtk.TreeView(self.liststore)
		self.add(self.listview)

		for i, (cname, ctype) in enumerate(columns):
			if cname != "_":
				renderer = gtk.CellRendererText()
				column = gtk.TreeViewColumn(cname, renderer, text=i)
				self.listview.append_column(column)

		self.listview.show()

	def append(self, data, focus = False):
		self.liststore.append(data)

	def get_selection(self, column):
		model, i = self.listview.get_selection().get_selected()
		if i is not None:
			return model.get_value(i, column)
		else:
			return None

	def set_selection_all(self, data):
		model, i = self.listview.get_selection().get_selected()
		if i is not None:
			for x, d in enumerate(data):
				model.set_value(i, x, d)

	def remove_selection(self, column):
		model, i = self.listview.get_selection().get_selected()
		if i is not None:
			model.remove(i)

	def get_and_remove_selection(self, column):
		model, i = self.listview.get_selection().get_selected()
		if i is not None:
			v = model.get_value(i, column)
			model.remove(i)
			return v
		return None
