#
#    Copyright (C) 2014 Jan Homola
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


import gtk
import paths
import os
import cProfile
import gobject
import clang.cindex as clanglib
from clangparser import ClangParser
from completionprovider import CompletionProvider
from proposalitem import ProposalItem


class ResultKindFilter():

    def __init__(self, default_map):
        self.default_map = default_map
        self.filtered_map = dict(default_map)

    def filter_by_name(self, names):
        for k in self.filtered_map.keys():
            if self.filtered_map[k][1] not in names:
                del self.filtered_map[k]

    def reset_filter(self):
        self.filtered_map = dict(self.default_map)

    def get_map(self):
        return self.filtered_map

result_kind_map = dict()


def init_kind_map(result_kind_map):
    maps = {
         0 : ([6,9,10,20,23,27,28,29],-100,"var"),
         1 : ([2,3,4,31,32],-85,"class"),
         2 : ([8,26,30],-90,"func"),
         3 : ([21],-92,"func"),
         4 : ([22,33],-80,"namespace"),
         5 : ([5],-78,"enum"),
         6 : ([7],-75,"enumerator"),
         #7 : ([500,501,502,503],-70,"macro"),
         8 : ([72],-40,"snippet")
    }

    for v in maps.values():
        icon_name = v[2]
        priority = v[1]
        for value in v[0]:
            result_kind_map[value] = (priority, icon_name)

init_kind_map(result_kind_map)


class KeyPressedMap():

    def __init__(self):
        self.pressed_keys = {}

    def count(self):
        return len(self.pressed_keys)

    def key_pressed(self, key):
        self.pressed_keys[key.keyval] = key

    def key_released(self, key):
        if self.pressed_keys.has_key(key.keyval):
            del self.pressed_keys[key.keyval]

    def remove_all(self):
        self.pressed_keys.clear()

    def has_pressed_keys(self, keys):
        for key in keys:
            if not self.pressed_keys.has_key(key):
                return False
        return True

    def has_pressed(self, key):
        return self.pressed_keys.has_key(key)

class PlaceHolderObject():
    items = []
    delimiters = ['(', ')', '{', '}', '<', '>', '[', ']', ', ', ' ', '\n','::', ';', '=', '']

    def __init__(self, iter, proposal, completion):
        self.proposal = proposal
        self.iter = iter
        self.completion = completion
        self.view = completion.view
        self.first_mark = completion.view.buffer.create_mark(None, iter, True)
        self.place_holder_marks = []
        self.current = 0
        self.loaded = False
        self.event_box = self._init_box()

    def is_loaded(self):
        return self.loaded

    def show(self):
        self.loaded = True
        self._set_marks()
        buffer = self.view.buffer
        iter = buffer.get_iter_at_mark(self.first_mark)
        rect = self.view.get_iter_location(iter)
        rect.x, rect.y = self.view.buffer_to_window_coords(gtk.TEXT_WINDOW_TEXT, rect.x, rect.y)
        self.view.add_child_in_window(self.event_box, gtk.TEXT_WINDOW_TEXT, rect.x, rect.y)
        self.select_index(self.current)
        self.event_box.show_all()
        PlaceHolderObject.items.append(self)

    def _init_box(self):
        event_box = gtk.EventBox()
        event_box.modify_bg(gtk.STATE_NORMAL, gtk.gdk.Color("black"))
        self.label = gtk.Label("")
        event_box.add(self.label)
        return event_box

    def _set_marks(self):
        buffer = self.view.buffer
        first = buffer.get_iter_at_mark(self.first_mark)
        places = self.proposal.get_placeholders()
        place_holder_count = places[-1]
        typed_text_len = len(places[0])
        first.forward_chars(typed_text_len)

        for index in range(1, len(places) - 1):
            w = places[index]
            l = len(w)
            if w in PlaceHolderObject.delimiters:
                first.forward_chars(l)
            else:
                w = w.replace("&","&amp;").replace(">","&gt;").replace("<","&lt;")
                place_holder_count -= 1
                first.backward_char()
                start_mark = buffer.create_mark(None, first)
                first.forward_chars(l + 1)
                end_mark = buffer.create_mark(None, first)
                self.place_holder_marks.append((start_mark, end_mark, w))

        if place_holder_count < 0:
            while place_holder_count != 0:
                del self.place_holder_marks[0]
                place_holder_count += 1

        last_mark = buffer.create_mark(None, first)
        self.place_holder_marks.append((last_mark, last_mark, "End"))

    def dismiss(self):
        buffer = self.completion.view.buffer
        buffer.delete_mark(self.first_mark)

        for index in range(len(self.place_holder_marks) - 1):
            sm, em , w = self.place_holder_marks[index]
            buffer.delete_mark(sm)
            buffer.delete_mark(em)

        buffer.delete_mark(self.place_holder_marks[-1][0])
        self.view.remove(self.event_box)
        self.view.grab_focus()

        if len(PlaceHolderObject.items) > 0:
            del PlaceHolderObject.items[-1]
        if len(PlaceHolderObject.items) > 0:
            place_holder_obj = PlaceHolderObject.items[-1]
            place_holder_obj.event_box.show_all()
            self.completion.active_place_holder = place_holder_obj
        else:
            self.completion.active_place_holder = None

    def hide(self):
        self.event_box.hide_all()

    def select_index(self, index):
        if index < 0 or index > len(self.place_holder_marks):
            return

        buffer = self.view.buffer
        start_mark, end_mark, word = self.place_holder_marks[index]
        start_iter = buffer.get_iter_at_mark(start_mark)
        start_iter.forward_char()
        end_iter = buffer.get_iter_at_mark(end_mark)
        rect = self.view.get_iter_location(start_iter)
        info = word
        if index == len(self.place_holder_marks) - 1:
            buffer.place_cursor(end_iter)
            rect = self.view.get_iter_location(end_iter)
        else:
            buffer.select_range(start_iter, end_iter)

        rect.x, rect.y = self.view.buffer_to_window_coords(gtk.TEXT_WINDOW_TEXT, rect.x, rect.y)

        if end_iter.get_line() == 0:
            rect.y += 20
        else:
            rect.y -= 20

        text = '<span color="white" size="medium">' + info +  '</span>'
        self.label.set_markup(text)
        self.view.move_child(self.event_box, rect.x, rect.y)

    def next(self):
        if self.current + 1 < len(self.place_holder_marks):
            self.current += 1
        else:
            self.current = 0
        self.select_index(self.current)

    def is_on_end(self):
        place_holders_count = self.proposal.placeholders[-1]
        return place_holders_count == self.current

    def contain(self, iter):
        offset = iter.get_offset()
        first_iter_offset = self.view.buffer.get_iter_at_mark(self.first_mark).get_offset()
        last_mark = self.place_holder_marks[-1][0]
        last_iter_offset = self.view.buffer.get_iter_at_mark(last_mark).get_offset()
        return (offset >= first_iter_offset and offset <= last_iter_offset)


class InfoBox(gtk.EventBox):

    def __init__(self, completion):
        gtk.EventBox.__init__(self)
        self.completion = completion
        self.label = gtk.Label("")
        self.add(self.label)
        self.completion.view.add_child_in_window(self, gtk.TEXT_WINDOW_TEXT, 0, 0)
        self.completion.view.connect("motion_notify_event", self.mouse_move)
        self.completion.app.window.connect("leave_notify_event", self.hide_box)
        self.delay = 0
        self.offset_x = -40
        self.offset_y = 20
        self.last_cursor = None
        self.show_box = True
        self.show_request = False
        self.mouse_x = 0
        self.mouse_y = 0
        self.modify_bg(gtk.STATE_NORMAL, gtk.gdk.color_parse("black"))
        self.label.modify_fg(gtk.STATE_NORMAL, gtk.gdk.color_parse("white"))

    def hide_box(self, w, e):
        self.show_box = False
        self.hide_all()

    def change_text(self, text):
        self.label.set_text(text)

    def set_show_delay(self, delay):
        self.delay = int(delay)

    def _set_window_pos(self, x, y):
        window_x = int(x)
        window_y = int(y)
        box_w, box_h = self.label.size_request()
        window_x -= box_w / 2
        window_y -= box_h + 15

        if window_x  < 0:
            window_x = 0
        if window_y < 0:
            window_y += box_h + 25
        self.completion.view.move_child(self, window_x, window_y)

    def mouse_move(self, view, e):
        bx, by = self.completion.view.window_to_buffer_coords(gtk.TEXT_WINDOW_TEXT, int(e.x), int(e.y))
        iter = self.completion.view.get_iter_at_location(int(bx), int(by))
        line = iter.get_line() + 1
        col = iter.get_line_offset()
        cursor = self.completion.get_cursor(line, col)
        self.mouse_x = e.x
        self.mouse_y = e.y

        def is_valid_cursor():
            if cursor and not (cursor.kind.is_invalid() or 
                               cursor.kind.is_unexposed() or
                               cursor.kind.is_statement()):
                return True
            else:
                return False

        def _fill_box(cursor, message = None):
            if not message:
                message = self._info_from_cursor(cursor)
            self.change_text(message)

        def _prepare_box(message = None):
            if self.show_box:
                _fill_box(self.last_cursor, message)
                self._set_window_pos(self.mouse_x, self.mouse_y)
                self.show_all()
            else:
                self.hide_all()
            self.show_request = False
            return False

        def request_show_box(message = None):
            self.show_box = True
            self.hide_all()
            if not self.show_request:
                self.show_request = True
                gobject.timeout_add(self.delay, _prepare_box, message)

        if not self.last_cursor or self.last_cursor != cursor:
            self.last_cursor = cursor
            if iter.get_chars_in_line() - 1 > col and is_valid_cursor():
                request_show_box()
            else:
                self.show_box = False
                self.hide_all()
        elif not is_valid_cursor():
                self.show_box = False
                self.hide_all()

        if self.completion.clang.type == "head":
            line -= self.completion.clang.get_header_line_offset()

        if self.completion.code_error_map.has_key(line - 1):
                error_code_info = self.completion.code_error_map[line - 1]
                if col >= error_code_info[0] and col <= error_code_info[1]:
                    message = "Error: " + error_code_info[2]
                    request_show_box(message)

    def _info_from_cursor(self, cursor):
        if not cursor:
            return "INVALID CURSOR"
        type = cursor.type.kind.spelling
        result_type = cursor.result_type.kind.spelling
        name = cursor.displayname
        definition = cursor.kind.name
        info_text = ""

        if name:
            info_text += "Name: " + name + "\n"
        if type and type != "Unexposed" and type != "Invalid":
            info_text += "Type: " + type + "\n"
        if result_type and result_type != "Invalid" and result_type != "Unexposed":
            info_text += "Result Type: " + result_type + "\n"
        if definition:
            info_text += "Definition: " + definition
        return info_text

def load_proposals_icons():
    theme = gtk.IconTheme()
    theme.set_search_path([paths.ICONS_COMPLETION_DIR])
    data = theme.list_icons()
    icons = {}

    for icon in data:
        icons[icon] = theme.load_icon(icon, 16, 0)
    return icons

icons = load_proposals_icons()

own_snippets = []
class_snippet = ProposalItem("class snippet", "class name\n{\n\n};", "class name\n{\n\n};", icons["snippet"])
class_snippet.set_placeholders(["class", " ", "name", "\n", "{", "\n", "\n", "}",";", 1])

class Completion(gobject.GObject):
    prefix_chars = [";", " ", "(", ".", ">", ":", "<", "[", "]", ")", "#", "-", "{", "}", "=", "\n", "\t"]

    def __init__(self, code_editor):
        gobject.GObject.__init__(self)
        self.code_editor = code_editor
        self.app = code_editor.app
        self.project = self.app.project
        self.view = code_editor.view
        self.completion = self.view.get_completion()

        self.completion.set_property("remember-info-visibility", True)
        self.completion.set_property("auto-complete-delay", 0)
        self.completion.set_property("accelerators", 0)
        self.completion.set_property("select-on-show", True)

        self.result_kind_filter = ResultKindFilter(result_kind_map)
        self.key_map = KeyPressedMap()
        self.clang = ClangParser(self)
        self.provider = CompletionProvider(self)
        self.completion.add_provider(self.provider)
        self.info_box = None

        self.active_place_holder = None
        self.kind_map = self.result_kind_filter.get_map()
        self.icons = icons
        self._load_settings()

        self.view.connect("key_press_event", self._key_pressed)
        self.view.connect("key_release_event", self._key_released)
        self.view.connect_after("key_release_event", self.parse)
        self.view.connect("button_press_event", self.mouse_click)

        self.completion.connect("show", self.window_showed)
        self.completion.connect("hide", self.window_hidden)
        self.view.connect("populate-popup", self.populate_context_menu)

        self.view.buffer.connect_after("changed", self.text_changed)
        self.view.buffer.connect_after("insert-text", self.text_insert_after)
        self.view.connect("paste-clipboard", self.paste_from_clip_board)
        self.view.connect_after("move-cursor", lambda v, s, c, e: self.set_prefix())
        self.view.connect_after("backspace",lambda w: self.set_prefix())
        self.view.connect("button_release_event", lambda w, e: self.set_prefix())
        self.gutt = self.view.get_gutter(gtk.TEXT_WINDOW_LEFT)
        self.gutt.connect("cell-activated", self.cell_activated)

        self.tu = None
        self.prefix = ""
        self.window_showed = False
        self.last_cursor_position = (1,1) #(line, column) when last completion occur
        self.code_changed = False
        self.results = []
        self.code_error_map = {}
        self.enabled_refactoring = False
        self.code_was_pasted = False
        self.code_change_completion = False

    def cell_activated(self, gutter, renderer, iter, event):
        pass

    def paste_from_clip_board(self, w):
        self.code_was_pasted = True

    def parse(self, w, key):
        if self.code_changed and self.key_map.count() == 0:
            self.parse_source_code()
            self.code_changed = False
            self.code_change_completion = True

    def _load_settings(self):
        self.view.set_highlight_current_line (self.app.settings.getboolean("code_completion",
                                                                            "enable_highlight_current_line"))
        self.view.set_show_line_numbers (self.app.settings.getboolean("code_completion",
                                                                       "enable_show_line_numbers"))
        self.view.set_tab_width(int(self.app.settings.getfloat("code_completion",
                                                                "tab_width")))

    def set_info_box(self, enable):
        if enable:
            self.info_box = InfoBox(self)
            self.info_box.set_show_delay(self.app.settings.getfloat("code_completion","delay_info_box"))
        else:
            self.info_box = None

    def set_refactoring(self, val):
        self.enabled_refactoring = val

    def refactor_code(self):
        cursor = self.get_cursor_under_mouse()
        referenced = None

        if cursor:
            referenced = cursor.referenced
        if not referenced:#or not(cursor.kind.is_expression() or cursor.kind.is_declaration()):
            return

        old_text = referenced.spelling

        dialog = gtk.Dialog("Insert new name", None, gtk.DIALOG_MODAL,
                            (gtk.STOCK_OK, gtk.RESPONSE_ACCEPT, gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL))
        dialog.set_position(gtk.WIN_POS_CENTER_ON_PARENT)
        dialog.set_size_request(300, 120)
        dialog.set_default_response(gtk.RESPONSE_ACCEPT)
        entry = gtk.Entry()
        entry.set_property("activates-default", True)
        entry.set_text(old_text)
        info_label = gtk.Label()
        dialog.vbox.pack_start(entry)
        dialog.vbox.pack_start(info_label)
        dialog.show_all()

        def response_handler(dialog, response):
            if response == gtk.RESPONSE_ACCEPT:
                new_text = entry.get_text()
                if not new_text:
                    info_label.set_text("Invalid variable name")
                elif new_text == old_text:
                    dialog.destroy()
                elif self.is_valid_refactor(referenced, new_text, info_label):
                    info_label.set_text("Refactoring in progress...")
                    dialog.show_all()
                    gtk.main_iteration_do()
                    loc = referenced.location
                    if self.clang.type == "header" or (loc.line < self.clang.get_header_line_offset() and (self.clang.type == "node")):
                        #Parse all project
                        self.rename_code_in_nodes(referenced, old_text, new_text)
                    else:
                        self.rename_code_in_node(self.code_editor, cursor, old_text, new_text)
                    self.parse_source_code()
                    dialog.destroy()
            else:
                dialog.destroy()

        dialog.connect("response", response_handler)

    def populate_context_menu(self, view, menu):
        goto_declaration = gtk.MenuItem("Go to declaration")
        goto_declaration.connect("activate", lambda w: self.goto_declaration())
        menu.append(goto_declaration)

        if self.enabled_refactoring:
            item = gtk.MenuItem("Refactor")
            refactoring_menu = gtk.Menu()
            rename_menu = gtk.MenuItem("Rename")
            rename_menu.connect("activate", lambda w: self.refactor_code())
            refactoring_menu.add(rename_menu)
            item.set_submenu(refactoring_menu)
            menu.append(item)

        menu.show_all()

    def window_hidden(self, w):
        if self.window_showed:
            self.window_showed = False

    def window_showed(self, w):
        self.window_showed = True

    def rename_code_by_location(self, buffer, where, new_text, old_text, line_offset):
        for location in where[:: - 1]:
            line = location.line - 1 - line_offset
            column = location.column - 1
            iter = buffer.get_iter_at_line(line)
            iter.set_line_offset(column)
            mark = buffer.create_mark(None, iter)
            start_iter = buffer.get_iter_at_mark(mark)
            end_iter = start_iter.copy()
            end_iter.forward_chars(len(old_text))
            buffer.delete(start_iter, end_iter)
            buffer.insert(start_iter, new_text)
            buffer.delete_mark(mark)

    #TODO:jen pro debug..pak smazat
    def set(self, cursor):
            buff = self.view.buffer
            buff.remove_tag_by_name("warning",buff.get_start_iter(),buff.get_end_iter())
            sr = cursor.extent
            start = sr.start
            end = sr.end
            sl = start.line - 1 - self.clang.line_offset
            sc = start.column - 1
            el = end.line - 1 - self.clang.line_offset
            ec = end.column - 1
            startiter = self.view.buffer.get_iter_at_line(sl)
            startiter.forward_chars(sc)
            enditer = self.view.buffer.get_iter_at_line(el)
            enditer.forward_chars(ec)
            self.view.buffer.apply_tag_by_name("warning",startiter,enditer)

    def is_valid_refactor(self, reference_cursor, new_token_name, info_label):
        self.closest = None
        self.contain = False
        self.error_message = None
        semantic_parent = reference_cursor.semantic_parent

        def find_closest_comp(cursor, ref_cursor):
            source_range = ref_cursor.extent
            line = source_range.start.line - 1
            column = source_range.start.column - 1
            line_end = source_range.end.line - 1

            def _find_closest_comp(cursor):
                if cursor.displayname == self.clang.tu.spelling:
                    self.closest = cursor
                    return
                if cursor.kind.from_param() in [2, 3, 4]:
                    if self.clang.type == "node":
                        s, e = self.code_editor.get_section_iters("")
                        ref_loc = cursor.location
                        ref_line = ref_loc.line - self.clang.get_line_offset() - s.get_line() - 1
                        if ref_line < s.get_line() and ref_line >=0: #test if var for rafactoring is in Vars struct in node
                            self.closest = None
                            self.error_message = "Variable \"" + new_token_name + "\" is inside Vars struct in node"
                            return

                    self.closest = cursor
                    return

                for c in cursor.get_children():
                    kind_value = c.kind.from_param()
                    if (kind_value >= 201 and kind_value <= 209) or kind_value in [2, 3, 4]:
                        source_range = c.extent
                        curr_start_line = source_range.start.line - 1
                        curr_start_column = source_range.start.column - 1
                        curr_end_line = source_range.end.line - 1

                        if line >= curr_start_line and column >= curr_start_column and line_end <= curr_end_line:
                            self.closest = c
                            _find_closest_comp(c)
            _find_closest_comp(cursor)

        def is_same_token_inside(cursor, token_name):

            def find_same_name(cursor, token_name):
                if not self.contain: 
                    for c in cursor.get_children():
                        if c.displayname == token_name or c.spelling == token_name:
                            self.contain = True
                            break
                        elif c.kind.is_statement():
                            find_same_name(c, token_name)
            find_same_name(cursor, token_name);
            return self.contain

        find_closest_comp(semantic_parent, reference_cursor)
        valid = False
        if not self.error_message and self.closest:
            is_same_token_inside(self.closest, new_token_name)
        else:
            if self.error_message:
                info_label.set_text(self.error_message)
            else:
                info_label.set_text("Error")
            del self.closest
            del self.contain
            del self.error_message
            return valid

        valid = self.closest and (not self.contain)
        if self.contain:
            self.error_message = "Variable \"" + new_token_name + "\" already exists"
            info_label.set_text(self.error_message)

        del self.closest
        del self.contain
        del self.error_message
        return valid

    def rename_code_in_node(self, node, referenced_cursor, old_name, new_name):
        places = []
        #TODO: improve recursive searching for referenced cursors
        self.find_cursor_uses(self.tu, self.tu.cursor, referenced_cursor, places)
        temp_buffer = gtk.TextBuffer()
        temp_buffer.set_text(node.buffer.get_all_text())
        start_iter, end_iter = node.get_section_iters("")
        start_line = start_iter.get_line()
        end_line = end_iter.get_line()
        self.rename_code_by_location(temp_buffer, places, new_name, old_name, self.clang.get_header_line_offset());
        new_code = temp_buffer.get_text(temp_buffer.get_iter_at_line(start_line), temp_buffer.get_iter_at_line(end_line))
        self.code_editor.set_text(new_code)

    def rename_code_in_nodes(self, referenced_cursor, old_name, new_name):
        head_code = self.project.get_head_comment() + self.clang.get_invisible_string()
        if self.clang.type == "header":
            head_code += self.code_editor.get_text("")
        else:
            head_code += self.project.get_head_code()
        line_offset = head_code.count("\n")

        try:
            gen = self.project.get_generator()
        except Exception, e:
            self.app.window.console.write(e.message)
            return

        temp_tu = clanglib.TranslationUnit.from_source("c.cpp", None, [("c.cpp", head_code)])
        temp_buffer = gtk.TextBuffer()

        for net in self.project.nets:
            location = referenced_cursor.location
            places = net.places()
            transitions = net.transitions()

            items = [] #contain code from all places and transitions
            items.append(head_code)
            node_info = [] #contain start_line, end_line, and place or transition where new_code should be written
            index_line = line_offset

            for place in places:
                place_code = "".join(["\n", gen.get_place_user_fn_header(place.get_id(), True), "{\n", place.code, "}\n"])
                end_lines = place_code.count("\n")
                items.append(place_code)
                node_info.append((index_line + 3, index_line + end_lines - 1, place))
                index_line += end_lines
 
            for transition in transitions:
                transition_headcode = gen.get_transition_user_fn_header(transition.get_id(), True)
                transition_code = "".join([transition_headcode, "{\n", transition.code, "}\n"])
                end_lines = transition_code.count("\n")
                items.append(transition_code)
                node_info.append((index_line + transition_headcode.count("\n") + 1, index_line + end_lines - 1, transition))
                index_line += end_lines

            all_code = "".join(items) #contain all code for parse and find all places where code should be rewritten
            temp_buffer.set_text(all_code)
            temp_tu.reparse([("c.cpp", all_code)])
            ref = clanglib.Cursor.from_location(temp_tu, location)
            where = [] #SourceLocation where is place for new code
            self.find_cursor_uses(temp_tu, temp_tu.cursor, ref, where)

            def location_sort(a, b):
                if a.line < b.line:
                    return -1
                if a.line > b.line:
                    return 1
                return cmp(a.column, b.column)

            where.sort(location_sort)
            self.rename_code_by_location(temp_buffer, where, new_name, old_name, 0)

            for info in node_info:
                start_line = info[0]
                end_line = info[1]
                place = info[2]
                start_iter_line = temp_buffer.get_iter_at_line(start_line)
                end_iter_line = temp_buffer.get_iter_at_line(end_line)
                new_code = temp_buffer.get_text(start_iter_line, end_iter_line)
                place.set_code(new_code)

        start_iter_head = temp_buffer.get_iter_at_line(self.clang.get_invisible_code_line_count() + 5)
        end_iter_head = temp_buffer.get_iter_at_line(line_offset)
        if end_iter_head.get_char() != "\n":
            end_iter_head.forward_to_line_end()

        new_head_code = temp_buffer.get_text(start_iter_head, end_iter_head)
        window = self.app.window
        reload_head = True

        if self.clang.type == "header":
            self.code_editor.set_text(new_head_code)
            reload_head = False
        else:
            self.project.set_head_code(new_head_code)

        def reload_tab(tab):
            from codeedit import CodeEditor

            if(issubclass(tab.widget.__class__, CodeEditor)):
                if(isinstance(tab.key, str)):
                    if reload_head:
                        tab.widget.set_text(self.project.get_head_code())
                else:
                    new_code = tab.key.get_code()
                    tab.widget.set_text(new_code)

        window.foreach_tab(reload_tab)

    def find_cursor_uses(self, tu, root_cursor, cursor, places):
        referenced_cursor = cursor.referenced
        if not referenced_cursor:
            return
        current_file = tu.spelling
        if cursor.semantic_parent and cursor.lexical_parent and cursor.semantic_parent != cursor.lexical_parent:
            referenced_cursor = cursor.canonical

        name = referenced_cursor.spelling

        def _find_cursors_locations(root_cursor, referenced_cursor, places):
            for c in root_cursor.get_children():
                if not c.location.file:
                    continue
                if c.location.file.name == current_file:
                    ref = c.referenced
                    valid = False
                    sem_par = c.semantic_parent

                    if (ref and ref == referenced_cursor) or (sem_par and sem_par == referenced_cursor):
                        valid = True
                    elif ref and not referenced_cursor.is_definition():
                        defin = referenced_cursor.get_definition()
                        if defin and defin == ref:
                            valid = True

                    if (c.spelling == name or (ref and ref.spelling == name)) and c.kind.from_param() != 103:
                        if valid:
                            loc = c.location
                            if loc not in places:
                                places.append(c.location)

                    _find_cursors_locations(c, referenced_cursor, places)

        _find_cursors_locations(root_cursor, referenced_cursor, places)

    def text_insert_after(self, buffer, iter, text, length):
        if self.code_was_pasted:
            self.code_was_pasted = False
            return True

        if self.active_place_holder and not self.active_place_holder.is_loaded():
            self.active_place_holder.show()

        if text == "." and not self.window_showed:
            self.parse_source_code()
            self.view.emit("show-completion")
        elif text == ">" and not self.window_showed:
            pos = buffer.get_cursor_position()
            iter = buffer.get_iter_at_offset(pos - 2)
            char = iter.get_char()
            if char == "-":
                self.parse_source_code()
                self.view.emit("show-completion")
        elif text == ":" and not self.window_showed:
            pos = buffer.get_cursor_position()
            iter = buffer.get_iter_at_offset(pos - 2)
            char = iter.get_char()
            if char == ":":
                self.parse_source_code()
                self.view.emit("show-completion")
        elif text == "(":
            buffer.insert_interactive(iter, ")", self.view.get_editable())
            iter.backward_char()
            buffer.place_cursor(iter)
        elif text == "{":
            buffer.insert_interactive(iter, "}", self.view.get_editable())
            iter.backward_char()
            buffer.place_cursor(iter)

    def text_changed(self, buffer):
        self.code_changed = True
        self.set_prefix()

    def mouse_click(self, w, e):
        if e.button == 3:
            buffer_x, buffer_y = self.view.window_to_buffer_coords(gtk.TEXT_WINDOW_TEXT, int(e.x), int(e.y))
            iter = self.view.get_iter_at_location(buffer_x, buffer_y)
            self.view.buffer.place_cursor(iter)

        if self.key_map.has_pressed(gtk.keysyms.Control_L) and e.button == 1:
            self.goto_declaration()
            self.key_map.remove_all()
            return True

        if self.active_place_holder:
            buffer_x, buffer_y = self.view.window_to_buffer_coords(gtk.TEXT_WINDOW_TEXT, int(e.x), int(e.y))
            iter = self.view.get_iter_at_location(buffer_x, buffer_y)
            if not self.active_place_holder.contain(iter):
                self.active_place_holder.dismiss()
        #DEBUG
#         if e.button == 3:
#             cursor = self.get_cursor_under_mouse()
#             where = []
#             self.find_cursor_uses(self.tu, self.tu.cursor, cursor, where)
#             
#             for w in where:
#                 l = w.line - self.clang.line_offset
#                 c = w.column
#                 print "line:",l, "col:", c
#             print "--------------"

#     def dump_cursor(self, cursor):
#         sem_par = cursor.semantic_parent
#         can = cursor.canonical
#         print "CANONICAL: ",can.spelling, can.displayname, can.kind
#         print "SELECTED: ",cursor.spelling, cursor.displayname, cursor.kind
#         if sem_par:
#             print "SEMANTIC_PARENT: ",sem_par.spelling, sem_par.displayname, sem_par.kind
#         
#         print "Childrens"
#         for cu in self.walk_preorder(cursor):
#             c = cu
#             if not c:
#                 continue
#             kind = c.kind
#             if kind.is_declaration():
#                 print "Declaration: ",
#             elif kind.is_statement():
#                 print "Statement: ",
#             elif kind.is_attribute():
#                 print "Attribute: ",
#             elif kind.is_unexposed():
#                 print "Unexposed: ",
#                 
#             print c.spelling, c.displayname, c.kind
# 
#     def walk_preorder(self,cursor): 
#         """Depth-first preorder walk over the cursor and its descendants. 
#         Yields cursors. 
#         """ 
#         yield cursor
#         for child in cursor.get_children(): 
#             for descendant in self.walk_preorder(child): 
#                 yield descendant 

    def goto_declaration(self):
            cursor = self.get_cursor_under_mouse()

            if cursor:
                referenced = cursor.referenced

                if referenced:
                    location = referenced.location
                    file = os.path.normpath(location.file.name)

                    if file == self.clang.file:
                        line = location.line - self.clang.get_header_line_offset()
                        column = location.column - 1
                        if line > 0 and self.clang.type == "header":
                            self.code_editor.jump_to_position(("", line - 5, column))
                        elif line > 0:
                            iter = self.code_editor.buffer.get_iter_at_line(line - 1)
                            iter.set_line_offset(column)
                            self.code_editor.buffer.place_cursor(iter)
                            self.code_editor.view.scroll_to_iter(iter, 0.1)
                        else:
                            offsets = self.clang.get_header_line_offset() - self.clang.get_invisible_code_line_count()
                            line_in_head = offsets + line - 5
                            if line_in_head >= 0:
                                #TODO:add to lineno column
                                self.app.edit_head(lineno = line_in_head)
                        return

                    from mainwindow import Tab
                    from codeedit import CodeFileEditor

                    code_editor = CodeFileEditor(self.app, self.project.get_syntax_highlight_key(), file)
                    code_editor.view.set_highlight_current_line(True)
                    code_editor.jump_to_position(("", location.line, 0))
                    window = self.app.window
                    tab_name = os.path.basename(file)
                    tab = Tab(tab_name, code_editor)
                    window.add_tab(tab, True)

    def _key_released(self, w, key):
        self.key_map.key_released(key)

    def _key_pressed(self, w, key):
        self.key_map.key_pressed(key)

        if self.active_place_holder:
            if key.keyval == gtk.keysyms.Escape:
                self.active_place_holder.dismiss()
                return True

            if key.keyval == gtk.keysyms.Tab:
                self.active_place_holder.next()
                return True

            if self.active_place_holder.is_on_end():
                self.active_place_holder.dismiss()
                pressed_key = key.keyval

                if (pressed_key == gtk.keysyms.space or 
                     pressed_key == gtk.keysyms.Return):
                    return True

    def set_prefix(self):
        end_pos = self.view.buffer.get_cursor_position()
        end_iter = self.view.buffer.get_iter_at_offset(end_pos)
        start_iter = end_iter.copy()

        while True:
            moved = start_iter.backward_char()
            if not moved:
                break
            if start_iter.get_char() in Completion.prefix_chars:
                start_iter.forward_char()
                break
        self.prefix = self.view.buffer.get_text(start_iter, end_iter)

    def item_selected(self, iter, proposal):
        if self.view.buffer.get_has_selection():
            start,end = self.view.buffer.get_selection_bounds()
            self.view.buffer.delete(start,end)

        if proposal.get_placeholders():
            if self.active_place_holder:
                self.active_place_holder.hide()
            self.active_place_holder = PlaceHolderObject(iter, proposal, self)

    def get_cursor(self, line, col):
        if self.tu:
            file = clanglib.File.from_name(self.tu, self.clang.file)
            location =  clanglib.SourceLocation.from_position(self.tu, file, line + self.clang.line_offset, col)
            cursor = clanglib.Cursor.from_location(self.tu, location)
            return cursor
        else:
            return None

    def get_cursor_under_mouse(self):
        position = self.view.buffer.get_cursor_position()
        iter = self.view.buffer.get_iter_at_offset(position)
        line = iter.get_line() + 1
        col = iter.get_line_offset()
        cursor_left = self.get_cursor(line, col)
        cursor_right = self.get_cursor(line, col + 1)

        if not cursor_left and not cursor_right:
            return None
        #if cursor on the left side of mouse cursor is valid -> return him first
        if not (cursor_left.kind.is_invalid() or cursor_left.kind.is_unexposed()):
            return cursor_left
        elif not (cursor_right.kind.is_invalid() or cursor_right.kind.is_unexposed()):
            return cursor_right
        else:
            return None

    def is_object_pointer(self, cursor):
        type = cursor.type.kind.spelling
        if type == "Pointer":
            pp = cursor.type.get_pointee()
            while pp.kind.spelling == "Pointer":
                pp = pp.get_pointee()

            pp = pp.kind.spelling

            if pp == "Record":
                pointer_type =  cursor.type.get_pointee()
                while pointer_type.kind.spelling == "Pointer":
                    pointer_type = pointer_type.get_pointee()
                #pointer_type = pointer_type.get_declaration().spelling
                return True
            else:
                return False
        else:
            return False

    def is_object(self, cursor):
        type = cursor.type.kind.spelling
        if type == "Record":
            #type_name =  cursor.type.get_declaration().spelling
            return True
        else:
            return False

    def parse_source_code(self):
        try:
            if self.tu is None:
                self.tu = self.clang.parse()
                self.clang.reparse()
                self._show_code_errors()
            else:
                self.clang.reparse()
                self._show_code_errors()
        except Exception, e:
            self.app.window.console.write("Parse error: " + e.message + "\n", "error")

    def _show_code_errors(self):
        self.view.buffer.remove_tag_by_name("error", self.view.buffer.get_start_iter(), self.view.buffer.get_end_iter())
        self.code_error_map.clear()

        for d in self.tu.diagnostics:
            if d.severity == 3:
                location = d.location
                info = d.spelling
                line = location.line - 1 - self.clang.line_offset
                iter = self.view.buffer.get_iter_at_line(line)
                if iter.get_chars_in_line() < location.column:
                    continue
                start_highlight_loc = location.column - 1
                end_highlight_loc = None
                fix_hit = ""

                for range in d.ranges:
                    s = range.start.column
                    e = range.end.column
                    start_highlight_loc = s - 1
                    end_highlight_loc = e
                for fix in d.fixits:
                    s = fix.range.start.column
                    e = fix.range.end.column
                    val = fix.value
                    fix_hit = val
                    start_highlight_loc = s - 2
                    end_highlight_loc = e - 1

                if fix_hit:
                    info += "\nFix hit: \" " + fix_hit + " \""

                start_iter = self.view.buffer.get_iter_at_line(line)
                if start_highlight_loc >= 0 and start_highlight_loc <= start_iter.get_chars_in_line():
                    start_iter.set_line_offset(start_highlight_loc)

                end_iter = start_iter.copy()

                if not end_highlight_loc:
                    end_iter.forward_visible_word_ends(1)
                else:
                    if end_highlight_loc >= start_highlight_loc and end_highlight_loc <= end_iter.get_chars_in_line():
                        end_iter.set_line_offset(end_highlight_loc)

                end_highlight_loc = end_iter.get_line_offset()
                self.view.buffer.apply_tag_by_name("error", start_iter, end_iter)
                self.code_error_map[line] = (start_highlight_loc, end_highlight_loc, info)

            if d.severity > 2 and d.location.file == self.tu.spelling:
                pass

    def get_proposals(self, context):
        iter = context.get_iter()
        line = iter.get_line() + 1
        col = iter.get_line_offset() + 1 - len(self.prefix)
        is_same_line = self.last_cursor_position[0] + 1 == line
        is_same_column = self.last_cursor_position[1] + 1 == col
        is_new_position = not (is_same_line and is_same_column)

        def get_results_now():
            new_results = self.clang.code_complete(line, col)
            if new_results and len(new_results.results) > 0:
                error_in_code = False
                for i in range(len(new_results.diagnostics)):
                    diag = new_results.diagnostics[i]
                    if diag.severity > 2:
                        loc = diag.location
                        if loc.line - self.clang.line_offset <= line:
                            error_in_code = True #if some error in above code -> fail completion
                            break
                if not error_in_code:
                    return new_results
                else:
                    return None

        def filter_fce(item):
            return item.get_label().startswith(self.prefix)

        new_results = None
        results_to_show = None
        #window is hidden and cursor position was changed or code was changed -> get new resutls
        #else we can use old results
        #if not self.window_showed and (self.code_changed or is_new_position):
        if not self.window_showed and (is_new_position or self.code_change_completion):
            new_results = get_results_now()
            if new_results:
                #cProfile.runctx("self.format_results2(new_results)", globals(), locals(),sort = 1)
                self.results = self.format_results(new_results)
                results_to_show = self.results
                if self.prefix:
                    results_to_show = filter(filter_fce, self.results)
            else:
                results_to_show = []
                del self.results[:]
        else:
            results_to_show = filter(filter_fce, self.results)

        self.code_change_completion = False
        self.last_cursor_position = (iter.get_line(), iter.get_line_offset())
        self.provider.set_proposals_count(len(results_to_show))
        context.props.completion.move_window(iter)
        context.add_proposals(self.provider, results_to_show, True)

    def format_results(self, results):
        proposals = []
        result_type_chunk = None
        icon_name = ""
        info = ""
        typed_text = ""
        label_text = ""
        place_holder = None
        brief_comment = None
        availability_string = None

        for result in results.results:
            result_kind = result.cursorKind

            if self.kind_map.has_key(result_kind):
                priority, icon_name = self.kind_map[result_kind]
            else:
                continue

            completion_string = result.string
            availability_string = completion_string.availability

            if availability_string.name != "Available":
                continue

            brief_comment = completion_string.briefComment
            chunks_list = []
            place_holder = 0
            result_type_chunk = None

            for chunk in completion_string:
                if chunk.isKindInformative():
                    continue
                if chunk.isKindResultType():
                    result_type_chunk = chunk
                    continue

                chunk_info = chunk.spelling
                chunks_list.append(chunk_info)

                
                if chunk.isKindPlaceHolder():
                    place_holder += 1
                    
                    #place_holder.append(chunks_list[-1])
                
                if chunk.isKindTypedText():
                    typed_text = label_text = chunks_list[-1]
                else:
                    typed_text = ''.join(chunks_list)

            info = ''.join(chunks_list)

            if result_kind == 72 and not place_holder:
                icon_name = "keyword"
                #priority = -35

            if result_type_chunk:
                label_text = ''.join((label_text, "  -->  ", result_type_chunk.spelling))

            icon = self.icons[icon_name]
            item = ProposalItem(label_text, typed_text, info, icon)

            b_comment = brief_comment.spelling
            if b_comment:
                info += "\nInformation: " + b_comment

            if place_holder > 0:
                chunks_list.append(place_holder)
                item.set_placeholders(chunks_list)

            proposals.append(item)
        return proposals

    def sort_proposals(self, proposals):
        def compare(a, b):
            if a[1] < b[1]:
                return -1
            if a[1] > b[1]:
                return 1
            return cmp(a[0].get_label(), b[0].get_label())

        proposals.sort(compare)

    def set_filter_mask(self, mask):
        if type(mask) is type([]):
            self.result_kind_filter.filter_by_name(mask)
            self.kind_map = self.result_kind_filter.get_map()
