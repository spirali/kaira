
import gtk

class SettingsWidget(gtk.Notebook):

	def __init__(self, app):
		gtk.Notebook.__init__(self)
		self.app = app
		self.set_tab_pos(gtk.POS_LEFT)
		self.append_page(self._generic_settings(), gtk.Label("Generic"))
		self.show_all()

	def _generic_settings(self):
		vbox = gtk.VBox()
		
		button = gtk.CheckButton("Save project before building")
		button.set_active(self.app.get_settings("save-before-build"))
		button.connect("toggled", lambda w: self.app.set_settings("save-before-build", w.get_active()))
		vbox.pack_start(button, False, False)
		return vbox
