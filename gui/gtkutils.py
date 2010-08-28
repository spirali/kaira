import gtk

def build_menu(description):
	menu = gtk.Menu()
	for name, action in description:
		item = gtk.MenuItem(name)
		item.connect("activate", action)
		menu.append(item)
	return menu
