from parameters import ParametersWidget
from externtypes import ExternTypesWidget, ExternTypeEditor
from functions import EventsWidget

import gtk

class ProjectConfig(gtk.Notebook):

	def __init__(self, app):
		gtk.Notebook.__init__(self)
		self.set_tab_pos(gtk.POS_LEFT)
		
		w = ParametersWidget(app.project, app.window)
		self.append_page(w, gtk.Label("Parameters"))

		w = ExternTypesWidget(app.project, app)
		self.append_page(w, gtk.Label("Extern types"))

		w = EventsWidget(app.project, app)
		self.append_page(w, gtk.Label("Events"))

		self.show_all()
