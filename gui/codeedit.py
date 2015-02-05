#
#    Copyright (C) 2010-2013  Stanislav Bohm
#                  2011       Ondrej Garncarz
#                  2014       Jan Homola
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
import sourceview
import textbuffer
import completion

class CodeEditor(gtk.ScrolledWindow):

    """
        sections: list of (name, start_string, middle_string, end_string)
    """
    def __init__(self, app, language, sections=None, start_pos=None, head_paragraph=None):
        gtk.ScrolledWindow.__init__(self)
        if sections is None:
            sections = [ ("", "", "", "") ]
        if start_pos is None:
            start_pos = (sections[0][0], 1, 1)
        self.sections = sections
        self.app = app
        self.set_shadow_type(gtk.SHADOW_IN)
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)

        buffer = self._create_buffer(language, sections, head_paragraph)
        self.view = self._create_view(buffer)

        self.add(self.view)

        self.show_all()
        self.jump_to_position(start_pos)

    def _create_buffer(self, key, sections, head_paragraph):
        manager = gtksourceview.LanguageManager()
        lan = manager.get_language(key)
        
        buffer = textbuffer.Buffer()
        buffer.create_tag("fixed", editable=False, background="lightgray")
        buffer.create_tag("normal")

        buffer.begin_not_undoable_action()

        if head_paragraph:
            buffer.create_tag("fixed-paragraph",
                              editable=False,
                              paragraph_background="lightgray")

            buffer.insert_with_tags_by_name(buffer.get_end_iter(),
                                            head_paragraph,
                                            "fixed-paragraph")

        for name, start_string, middle_string, end_string in sections:
            self._create_section(buffer, name, start_string, middle_string, end_string)

        buffer.end_not_undoable_action()
        buffer.set_language(lan)
        buffer.set_highlight_syntax(True)
        buffer.set_highlight_matching_brackets(True)
        buffer.connect("changed", lambda w: self.buffer_changed())
        self.buffer = buffer
        return buffer

    def _create_section(self, buffer, name, start_string, middle_string, end_string):
        buffer.insert_with_tags_by_name(buffer.get_end_iter(), start_string, "fixed")
        buffer.create_mark(name + "-start", buffer.get_end_iter(), True)
        buffer.insert_with_tags_by_name(buffer.get_end_iter(), middle_string, "normal")
        buffer.create_mark("tmp_end", buffer.get_end_iter(), True)
        buffer.insert_with_tags_by_name(buffer.get_end_iter(), end_string, "fixed")
        mark = buffer.get_mark("tmp_end")
        buffer.create_mark(name + "-end", buffer.get_iter_at_mark(mark), False)
        buffer.delete_mark(mark)

    def _create_view(self, buffer):
        view = sourceview.View(buffer)
        view.set_auto_indent(True)
        font_desc = pango.FontDescription("monospace 10")
        if font_desc:
            view.modify_font(font_desc)
        return view

    def buffer_changed(self):
        pass

    def get_section_iters(self, section_name):
        start_mark = self.buffer.get_mark(section_name + "-start")
        end_mark = self.buffer.get_mark(section_name + "-end")
        start_iter = self.buffer.get_iter_at_mark(start_mark)
        end_iter = self.buffer.get_iter_at_mark(end_mark)
        return start_iter, end_iter

    def get_text(self, section_name=""):
        start_iter, end_iter = self.get_section_iters(section_name)
        return self.buffer.get_text(start_iter, end_iter)

    def set_text(self, text, section_name=""):
        if text is None:
            text = ""
        start_iter, end_iter = self.get_section_iters(section_name)
        mark = self.buffer.create_mark("tmp_end", end_iter, True)
        self.buffer.insert_with_tags_by_name(end_iter, text, "normal")
        start_iter, end_iter = self.get_section_iters(section_name)
        self.buffer.delete(start_iter, self.buffer.get_iter_at_mark(mark))
        self.buffer.delete_mark(mark)

    def grab_focus(self):
        self.view.grab_focus()

    def has_focus(self):
        return self.view.has_focus()

    def jump_to_position(self, position):
        """ Take pair (section_name, line_number) and move cursor on this position
            or by triplet (section_name, line_number, column_number) """
        if position is None:
            return
        if len(position) == 3:
            section_name, line_number, column = position
        else:
            section_name, line_number = position
            column = 0

        start_mark = self.buffer.get_mark(section_name + "-start")
        text_iter = self.buffer.get_iter_at_mark(start_mark)
        text_iter.forward_lines(line_number - 1)
        text_iter.forward_chars(column)
        self.buffer.place_cursor(text_iter)
        self.view.scroll_to_iter(text_iter, 0.1)
        self.view.grab_focus()


class CodeFileEditor(CodeEditor):

    def __init__(self, app, language, filename=None):
        CodeEditor.__init__(self, app, language)
        self.view.set_show_line_numbers(True)
        self.load(filename)

    def load(self, filename):
        self.filename = filename
        if filename is None:
            self.set_text("")
        else:
            with open(self.filename, "r") as f:
                self.set_text(f.read())

    def save(self):
        if self.filename is not None:
            with open(self.filename, "w") as f:
                f.write(self.get_text())


class TransitionCodeEditor(CodeEditor):

    def __init__(self, app, project, transition, header):
        self.transition = transition
        if transition.get_code() == "":
            code = "\t\n"
        else:
            code = transition.get_code()
        section = [ "", "{\n", code, "}\n" ]
        CodeEditor.__init__(self,
                            app,
                            project.get_syntax_highlight_key(),
                            [ section ],
                            ("", 1, 1),
                            header)

        self.view.code_complete = completion.Completion(self)
        self.view.code_complete.clang.set_type(header, transition)
        self.view.code_complete.set_info_box(app.settings.getboolean("code_completion", "enable_info_box"))
        self.view.code_complete.set_refactoring(True)
        self.view.code_complete.parse_source_code()

    def buffer_changed(self):
        self.transition.set_code(self.get_text())


class PlaceCodeEditor(CodeEditor):

    def __init__(self, app, project, place, header):
        self.place = place
        if place.get_code() == "":
            code = "\t\n"
        else:
            code = place.get_code()

        section = [ "", "{\n", code, "}\n" ]
        CodeEditor.__init__(self,
                            app,
                            project.get_syntax_highlight_key(),
                            [ section ],
                            ("", 1, 1),
                            header)

        self.view.code_complete = completion.Completion(self)
        self.view.code_complete.clang.set_type(header, place)
        self.view.code_complete.set_info_box(app.settings.getboolean("code_completion", "enable_info_box"))
        self.view.code_complete.set_refactoring(True)
        self.view.code_complete.parse_source_code()

    def buffer_changed(self):
        self.place.set_code(self.get_text())


class HeadCodeEditor(CodeEditor):

    def __init__(self, app, project):
        self.project = project
        header = project.get_head_comment()
        section = [ "", "", project.get_head_code(), "" ]
        CodeEditor.__init__(self,
                            app,
                            project.get_syntax_highlight_key(),
                            [ section ],
                            ("", 1, 0),
                            header)

        self.view.code_complete = completion.Completion(self)
        self.view.code_complete.clang.set_type(header)
        self.view.code_complete.set_info_box(app.settings.getboolean("code_completion", "enable_info_box"))
        self.view.code_complete.set_refactoring(True)
        self.view.code_complete.parse_source_code()

    def save(self):
        self.project.set_head_code(self.get_text())


class TabCodeEditor(mainwindow.SaveTab):
    pass


class TabCodeFileEditor(mainwindow.SaveTab):

    def __init__(self, filename, key, language):
        name = os.path.basename(filename)
        mainwindow.Tab.__init__(self, name, CodeFileEditor(filename, language), key)
