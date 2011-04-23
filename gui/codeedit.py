#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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


import gtksourceview2 as gtksourceview
import gtk
import pango
import os
import mainwindow

class CodeEditor(gtk.VBox):
	
	def __init__(self, start_string, middle_string, end_string, start_pos, head_paragraph = None):
		gtk.VBox.__init__(self)

		toolbar = self._toolbar()
		if toolbar is not None:
			self.pack_start(toolbar, False, False)
		
		sw = gtk.ScrolledWindow()
		self.pack_start(sw)
		sw.set_shadow_type(gtk.SHADOW_IN)

		buffer = self._create_buffer(start_string, middle_string, end_string, start_pos, head_paragraph)
		self.view = self._create_view(buffer)
		sw.add(self.view)

		self.show_all()
	
	def _create_buffer(self, start_string, middle_string, end_string, start_pos, head_paragraph):
		manager = gtksourceview.LanguageManager()
		lan = manager.get_language("cpp")
		buffer = gtksourceview.Buffer()
		start_line, start_char = start_pos

		buffer.create_tag("fixed", editable=False, background="lightgray")
		buffer.create_tag("normal")

		buffer.begin_not_undoable_action()

		if head_paragraph:
			buffer.create_tag("fixed-paragraph", editable=False, paragraph_background="lightgray")
			buffer.insert_with_tags_by_name(buffer.get_end_iter(), head_paragraph, "fixed-paragraph")

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
		self.view.set_show_line_numbers(True)

	def _toolbar(self):
		button1 = gtk.Button("Save")
		button1.connect("clicked", self.save)
		toolbar = gtk.Toolbar()
		toolbar.add(button1)
		toolbar.show_all()
		return toolbar

	def save(self, w = None):
		#FIXME: catch io erros
		with open(self.filename, "w") as f:
			f.write(self.get_text())


class TransitionCodeEditor(CodeEditor):
	
	def __init__(self, transition, variables):
		self.transition = transition
		if transition.get_code() == "":
			code = "\t\n"
		else:
			code = transition.get_code()
		head = "struct Vars {\n" + "".join(["\t" + v.strip() + ";\n" for v in variables ]) + "};\n\n"
		line = 5 + len(variables)
		CodeEditor.__init__(self, "void transition_function(CaContext *ctx, Vars & var)\n{\n", code, "}\n", (line,0), head)


	def buffer_changed(self, buffer):
		self.transition.set_code(self.get_text())

class PlaceCodeEditor(CodeEditor):
	
	def __init__(self, place, place_type):
		self.place = place
		if place.get_code() == "":
			code = "\t\n"
		else:
			code = place.get_code()
		begin = "void init_place(CaContext *ctx, {0} * place)\n{{\n".format(place_type)
		CodeEditor.__init__(self, begin, code, "}\n", (2,0))


	def buffer_changed(self, buffer):
		self.place.set_code(self.get_text())

class TabCodeFileEditor(mainwindow.Tab):

	def __init__(self, filename, key):
		name = os.path.basename(filename)
		mainwindow.Tab.__init__(self, name, CodeFileEditor(filename), key)

	def project_save(self):
		self.widget.save()

	def project_export(self):
		self.widget.save()
