
import gtk

class BuildOptionsWidget(gtk.VBox):

	def __init__(self, project):
		gtk.VBox.__init__(self)
		self.project = project

		hbox = gtk.HBox()
		button = gtk.Button("Write makefile")
		button.connect("clicked", self._write_makefile)
		hbox.pack_start(button, False, False)
		self.pack_start(hbox, False, False)
		
		self.table = gtk.Table()		
		self.table.set_border_width(5)
		self.table.set_row_spacings(5)
		self.table.set_col_spacings(5)
		self.add_line(0, "CC")
		self.add_line(1, "CFLAGS")
		self.add_line(2, "LIBS")

		self.pack_start(self.table, False, False)
	
	def add_line(self, line, text):
		label = gtk.Label(text)
		label.set_alignment(1.0,0.5)
		entry = gtk.Entry()
		entry.set_text(self.project.get_build_option(text))
		entry.connect("changed", lambda w: self.project.set_build_option(text, w.get_text()))
		self.table.attach(label, 0, 1, line, line + 1, gtk.SHRINK | gtk.FILL)	
		self.table.attach(entry, 1, 2, line, line + 1)	

	def _write_makefile(self, w):
		self.project.write_makefile()
