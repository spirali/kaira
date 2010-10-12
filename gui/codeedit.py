
import gtksourceview2 as gtksourceview
import gtk
import pango
import os

class CodeEditor(gtk.VBox):
	
	def __init__(self, start_string, middle_string, end_string, start_pos):
		gtk.VBox.__init__(self)

		toolbar = self._toolbar()
		if toolbar is not None:
			self.pack_start(toolbar, False, False)
		
		sw = gtk.ScrolledWindow()
		self.pack_start(sw)
		sw.set_shadow_type(gtk.SHADOW_IN)

		buffer = self._create_buffer(start_string, middle_string, end_string, start_pos)
		view = self._create_view(buffer)
		sw.add(view)

		self.show_all()
	
	def _create_buffer(self, start_string, middle_string, end_string, start_pos):
		manager = gtksourceview.LanguageManager()
		lan = manager.get_language("cpp")
		buffer = gtksourceview.Buffer()
		start_line, start_char = start_pos

		buffer.create_tag("fixed", editable=False, background="lightgray")
		buffer.create_tag("normal")

		buffer.begin_not_undoable_action()
		buffer.insert_with_tags_by_name(buffer.get_end_iter(), start_string, "fixed")
		buffer.create_mark("start", buffer.get_end_iter(), True)
		buffer.insert_with_tags_by_name(buffer.get_end_iter(), middle_string, "normal")
		buffer.create_mark("tmp_end", buffer.get_end_iter(), True)
		buffer.insert_with_tags_by_name(buffer.get_end_iter(), end_string, "fixed")
		mark = buffer.get_mark("tmp_end")
		buffer.create_mark("end", buffer.get_iter_at_mark(mark), False)
		buffer.delete_mark(mark)
		buffer.end_not_undoable_action()

		buffer.place_cursor(buffer.get_iter_at_line_offset(start_line, start_char))
		buffer.set_language(lan)
		buffer.set_highlight_syntax(True)
		buffer.set_highlight_matching_brackets(True)
		buffer.connect("changed", self.buffer_changed)
		self.buffer = buffer
		return buffer

	def _create_view(self, buffer):
		view = gtksourceview.View(buffer)
		view.set_auto_indent(True)
		font_desc = pango.FontDescription('monospace')
		if font_desc:
			view.modify_font(font_desc)
		return view

	def buffer_changed(self, w):
		pass

	def get_text(self):
		start_mark = self.buffer.get_mark("start")
		end_mark = self.buffer.get_mark("end")
		start_iter = self.buffer.get_iter_at_mark(start_mark)
		end_iter = self.buffer.get_iter_at_mark(end_mark)
		return self.buffer.get_text(start_iter, end_iter)

	def _toolbar(self):
		return None

class CodeFileEditor(CodeEditor):

	def __init__(self, filename):
		self.filename = filename
		if os.path.isfile(filename):
			with open(filename, "r") as f:
				content = f.read()
		else:
			content = ""
		CodeEditor.__init__(self, "", content, "", (0,0))

	def _toolbar(self):
		button1 = gtk.Button("Save")
		button1.connect("clicked", self._save)
		toolbar = gtk.Toolbar()
		toolbar.add(button1)
		toolbar.show_all()
		return toolbar

	def _save(self, w):
		#FIXME: catch io erros
		with open(self.filename, "w") as f:
			f.write(self.get_text())


class TransitionCodeEditor(CodeEditor):
	
	def __init__(self, transition):
		self.transition = transition
		if transition.get_code() == "":
			code = "\t\n"
		else:
			code = transition.get_code()
		CodeEditor.__init__(self, "void transition_function(CaContext *ctx, VARS & var)\n{\n", code, "}\n", (2,1))


	def buffer_changed(self, buffer):
		self.transition.set_code(self.get_text())

class PlaceCodeEditor(CodeEditor):
	
	def __init__(self, place):
		self.place = place
		if place.get_code() == "":
			code = "\t\n"
		else:
			code = place.get_code()
		CodeEditor.__init__(self, "void init_place(CaContext *ctx, PLACE * place)\n{\n", code, "}\n", (2,1))


	def buffer_changed(self, buffer):
		self.place.set_code(self.get_text())
