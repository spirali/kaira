
import gtkutils
import gtk

class ObjectList(gtk.VBox):

	def __init__(self, list_definition, buttons):
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
		self.pack_start(self.list)
		self.show_all()

	def _callback(self, callback):
		return lambda w: callback(self.list.get_selection(0))

	def add_object(self, obj):
		self.list.append(self.object_as_row(obj))

	def get_and_remove_selected(self):
		return self.list.get_and_remove_selection(0)

	def update_selected(self, obj):
		self.list.set_selection_all(self.object_as_row(obj))
