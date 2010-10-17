import gtk
from objectlist import ObjectList
from codeedit import CodeEditor

class EventsWidget(ObjectList):
	
	def __init__(self, project, app):
		defs = [("_", object), ("Name", str) ]
		buttons = [
			("Edit code", None, self._edit_code) ]

		ObjectList.__init__(self, defs, buttons)
		self.project = project
		self.app = app

		for obj in project.get_events():
			self.add_object(obj)

	def object_as_row(self, obj):
		return [obj, obj.get_name()]

	def _edit_code(self, obj):
		if obj is not None:
			self.app.event_edit(obj)

class EventEditor(CodeEditor):

	def __init__(self, event):
		self.event = event
		declaration = event.get_function_declaration()
		code = event.get_function_code()
		CodeEditor.__init__(self, declaration + "\n{\n", code, "}\n", (2, 1))

	def buffer_changed(self, buffer):
		self.event.set_function_code(self.get_text())
