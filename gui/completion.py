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

    def __init__(self, defaultmap):
        self.defaultmap = defaultmap
        self.filteredmap = dict(defaultmap)

    def filter_by_name(self, names):
        for k in self.filteredmap.keys():
            if self.filteredmap[k][1] not in names:
                del self.filteredmap[k]

    def reset_filter(self):
        self.filteredmap = dict(self.defaultmap)

    def get_map(self):
        return self.filteredmap

resultKindMap = dict()


def init_kind_map(resultKindMap):
    maps = {
        0 : ([9,6,10],-100,"var"),
        1 : ([2,3,4,20,31,32],-85,"class"),
        2 : ([8,26,27,28,29,30],-90,"func"),
        3 : ([21],-92,"func"),
        4 : ([22,33],-80,"namespace"),
        5 : ([5],-78,"enum"),
        6 : ([7],-75,"enumerator"),
        #7 : ([500,501,502,503],-70,"macro"),
        8 : ([72],-40,"snippet")
    }
    for v in maps.values():
        iconname = v[2]
        priority = v[1]
        for value in v[0]:
            resultKindMap[value] = (priority,iconname)

init_kind_map(resultKindMap)


class KeyPressedMap():

    def __init__(self):
        self.keys = []

    def count(self):
        return len(self.keys)

    def key_pressed(self, key):
        if len(self.keys) > 0:
            if self.keys[-1] == key.keyval:
                self.remove_all()
        self.keys.append(key.keyval)

    def key_released(self, key):
        for i in range(len(self.keys)):
            if self.keys[i] == key.keyval:
                del self.keys[i]
                return

    def remove_all(self):
        self.keys = []
        return
        for i in range(len(self.keys)):
            del self.keys[i]


class PlaceHolderObject():
    items = []

    def __init__(self, iter, proposal, buffer, view):
        self.enditer = iter.copy()
        self.proposal = proposal
        self.buffer = buffer
        self.place = proposal.get_placeholders()
        self.maxnum = len(self.place)
        self.current = 0
        self.text = proposal.get_property("text")
        self.startiter = iter.copy()
        self.startiter.set_offset(iter.get_offset() - len(self.text))
        self.view = view
        self.marks = []
        self.labels = []

        self.eb = gtk.EventBox()
        self.lab = gtk.Label("")

        for place in self.place:
            temp = place.replace("&","&amp;")
            temp = temp.replace(">","&gt;")
            temp = temp.replace("<","&lt;")
            self.labels.append(temp)

        self._set_marks()

        self.eb.add(self.lab)
        self.eb.modify_bg(gtk.STATE_NORMAL,gtk.gdk.Color("black"))

        self.view.add_child_in_window(self.eb,gtk.TEXT_WINDOW_TEXT,0,0)
        self.select_index(0)
        self.view.show_all()
        PlaceHolderObject.items.append(self.eb)

    def _set_marks(self):
        tempiter = self.startiter.copy()
        lastmarkchar = ""
        for i in range(len(self.place)):
            startmark = None
            endmark = None

            placeholder = self.text.find(self.place[i],0)
            iter = self.buffer.get_iter_at_offset(tempiter.get_offset() + placeholder-1)

            if i ==0 or lastmarkchar != ",":
                startmark = self.buffer.create_mark(None,iter)

            iter.set_offset(iter.get_offset() + len(self.place[i])+1)
            lastmarkchar = iter.get_char()

            endmark = self.buffer.create_mark(None,iter)

            if not startmark:
                lastendmark = self.marks[len(self.marks)-1][1]
                startmark = lastendmark

            self.marks.append((startmark,endmark))

    def hide(self):
        for mark in self.marks:
            self.buffer.delete_mark(mark[0])
            self.buffer.delete_mark(mark[1])

        childrens = PlaceHolderObject.items
        for child in childrens:
            self.view.remove(child)
            PlaceHolderObject.items.remove(child)

    def select_index(self, index):
        if index < 0 or index > self.maxnum:
            return

        startmark,endmark = self.marks[index]
        startiter = self.buffer.get_iter_at_mark(startmark)
        enditer = self.buffer.get_iter_at_mark(endmark)
        startiter.set_offset(startiter.get_offset() + 1)
        self.buffer.select_range(startiter,enditer)
        rectangle = self.view.get_iter_location(startiter)
        rectangle.x,rectangle.y = self.view.buffer_to_window_coords(gtk.TEXT_WINDOW_TEXT,rectangle.x,rectangle.y)
        info = self.labels[index]
        text = '<span color="white">' + info +  '</span>'
        self.lab.set_markup(text)
        self.view.move_child(self.eb,rectangle.x,rectangle.y-20)

    def next(self):
        if self.current +1 < self.maxnum:
            self.current += 1
        else:
            self.current = 0
        self.select_index(self.current)


class InfoBox(gtk.EventBox):

    def __init__(self, completion):
        gtk.EventBox.__init__(self)
        self.completion = completion
        self.label = gtk.Label("")
        self.add(self.label)
        self.completion.view.add_child_in_window(self,gtk.TEXT_WINDOW_TEXT,0,0)
        self.completion.view.connect("motion_notify_event",self.mouse_move)
        self.completion.app.window.connect("leave_notify_event", self.hide_box)
        self.delay = 0
        self.offsetx = -40
        self.offsety = 20
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
        cursor = self.completion.get_cursor(line,col)
        self.mouse_x = e.x
        self.mouse_y = e.y

        def is_valid_cursor():
            if not (cursor.kind.is_invalid() or cursor.kind.is_unexposed() or cursor.kind.is_statement()):
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
            if self.show_request is False:
                    self.show_request = True
                    gobject.timeout_add(self.delay,_prepare_box, message)

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
            line-= self.completion.clang.get_header_line_offset()

        if self.completion.codeErrorList.has_key(line - 1):
                errorcodeinfo = self.completion.codeErrorList[line-1]
                if col >= errorcodeinfo[0] and col <= errorcodeinfo[1]:
                    message = "Error: " + errorcodeinfo[2]
                    request_show_box(message)

    def _info_from_cursor(self, cursor):
        type = cursor.type.kind.spelling
        resulttype = cursor.result_type.kind.spelling
        name = cursor.displayname
        definition = cursor.kind.name
        infotext = ""

        if name:
            infotext += "Name: " + name + "\n"
        if type and type != "Unexposed" and type != "Invalid":
            infotext += "Type: " + type + "\n"
        if resulttype and resulttype != "Invalid" and resulttype != "Unexposed":
            infotext += "Result Type: " + resulttype + "\n"
        if definition:
            infotext += "Definition: " + definition
        return infotext

def load_proposals_icons():
    theme = gtk.IconTheme()
    theme.set_search_path([paths.ICONS_COMPLETION_DIR])
    data = theme.list_icons()
    icons = {}

    for icon in data:
        icons[icon] = theme.load_icon(icon,16,0)
    return icons

icons = load_proposals_icons()


class Completion(gobject.GObject):
    prefixchars = [";"," ","(",".",">",":","<","[","]",")","#","-","{","}","=","\n"]

    def __init__(self, codeeditor, project):
        gobject.GObject.__init__(self)
        self.codeeditor = codeeditor
        self.app = codeeditor.app
        self.view = codeeditor.view
        self.resultKindFilter = ResultKindFilter(resultKindMap)
        self.completion = self.view.get_completion()

        self.completion.set_property("remember-info-visibility",True)
        self.completion.set_property("auto-complete-delay",0)
        self.completion.set_property("accelerators",0)
        self.completion.set_property("select-on-show",True)

        self.project = project
        self.keymap = KeyPressedMap()
        self.clang = ClangParser(self)
        self.provider = CompletionProvider(self)
        self.completion.add_provider(self.provider)
        self.kindmap = self.resultKindFilter.get_map()
        self.icons = icons
        self.infoBox = None
        self.placeHolderObject = None
        self._load_settings()

        self.view.connect("key_press_event",self._key_pressed)
        self.view.connect("key_release_event",self._key_released)
        self.view.connect("button_press_event",self.mouse_click)
        self.completion.connect("show",self.window_showed)
        self.completion.connect("hide",self.window_hidden)
        self.view.buffer.connect("changed",self.text_changed)
        self.view.buffer.connect_after("insert-text",self.text_inserted)
        self.view.connect("populate-popup",self.populate_context_menu)

        self.tu = None
        self.prefix = ""
        self.window_showed = False
        self.results = []
        self.lastCC = (1,1)
        self.codeChanged = True
        self.insertedItem = False
        self.lastSelectedItem = None
        self.view.show_all()
        self.codeErrorList = {}
        self.enabled_refactoring = False

    def _load_settings(self):
        self.view.set_highlight_current_line (self.app.settings.getboolean("code_completion","enable_highlight_current_line"))
        self.view.set_show_line_numbers (self.app.settings.getboolean("code_completion","enable_show_line_numbers"))
        self.view.set_tab_width(int(self.app.settings.getfloat("code_completion","tab_width")))

    def set_info_box(self, enable):
        if enable is True:
            self.infoBox = InfoBox(self)
            self.infoBox.set_show_delay(self.app.settings.getfloat("code_completion","delay_info_box"))
        else:
            self.infoBox = None

    def set_refactoring(self, val):
        self.enabled_refactoring = val

    def populate_context_menu(self, view, menu):
        goto_declaration = gtk.MenuItem("Go to declaration under cursor")
        goto_declaration.connect("activate",self.goto_declaration)
        menu.append(goto_declaration)

        if self.enabled_refactoring:
            item = gtk.MenuItem("Refactoring")
            refactoringMenu = gtk.Menu()
            renameMenu = gtk.MenuItem("Rename under cursor")
            refactoringMenu.add(renameMenu)
            item.set_submenu(refactoringMenu)
            renameMenu.connect("activate",self.refactor_code)
            menu.append(item)

        menu.show_all()

    def window_hidden(self, w):
        if self.window_showed:
            self.window_showed = False

    def window_showed(self, w):
        self.window_showed = True

    def refactor_code(self, widget):
        cursor = self.get_cursor_under_mouse()
        referenced = None

        if cursor:
            referenced = cursor.referenced
        if not referenced or not(cursor.kind.is_expression() or cursor.kind.is_declaration()):
            return

        oldtext = cursor.spelling
        if not oldtext:
            oldtext = cursor.displayname

        dialog = gtk.Dialog("Insert new name",None,gtk.DIALOG_MODAL,
                            (gtk.STOCK_OK, gtk.RESPONSE_ACCEPT, gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL))
        dialog.set_position(gtk.WIN_POS_CENTER_ON_PARENT)
        dialog.set_size_request(300,100)
        dialog.set_default_response(gtk.RESPONSE_ACCEPT)
        entry = gtk.Entry()
        entry.set_property("activates-default",True)
        entry.set_text(oldtext)
        dialog.vbox.pack_start(entry)
        entry.show()
        response = dialog.run()
        newtext = ""

        if response == gtk.RESPONSE_ACCEPT:
            newtext = entry.get_text()
            dialog.destroy()
        else:
            dialog.destroy()
            return

        if not newtext:
            return

        loc = referenced.location
        if self.clang.type == "header" or (loc.line < self.clang.get_header_line_offset() and (self.clang.type == "node")):
            #Parse all project
            self.rename_code_in_nodes(referenced,oldtext,newtext)
        else:
            self.rename_code_in_node(self.codeeditor,referenced,oldtext, newtext)

    def rename_code_in_node(self, node, referencedCursor, oldname, newname):
        places = []
        self.find_cursor_uses(self.tu, self.tu.cursor, referencedCursor, places)

        tempbuffer = gtk.TextBuffer()
        tempbuffer.set_text(node.buffer.get_all_text())

        s,e = node.get_section_iters("")

        sline = s.get_line()
        eline = e.get_line()

        length = len(oldname)
        marks = []

        for location in places:
            line = location.line - 1 - self.clang.get_header_line_offset()
            col = location.column - 1
            iter = tempbuffer.get_iter_at_line(line)
            iter.set_line_offset(col)
            marks.append(tempbuffer.create_mark(None,iter))

        for mark in marks:
            startiter = tempbuffer.get_iter_at_mark(mark)
            enditer = startiter.copy()
            enditer.set_line_offset(enditer.get_line_offset() + length)
            tempbuffer.delete(startiter,enditer)
            tempbuffer.insert(startiter,newname)
            tempbuffer.delete_mark(mark)
        newcode = tempbuffer.get_text(tempbuffer.get_iter_at_line(sline),tempbuffer.get_iter_at_line(eline))
        self.codeeditor.set_text(newcode)

    def rename_code_in_nodes(self, referencedCursor, oldname, newname):
        headcode = self.project.get_head_comment() + self.clang.get_invisible_string()
        if self.clang.type == "header":
            headcode += self.codeeditor.get_text("")
        else:
            headcode += self.project.get_head_code()
        line_offset = headcode.count("\n")
        net = self.project.nets[0]
        gen = self.project.get_generator()
        temptu = clanglib.TranslationUnit.from_source("c.cpp", None, [("c.cpp",headcode)])

        location = referencedCursor.location
        tempbuffer = gtk.TextBuffer()
        places = net.places()
        transitions = net.transitions()
        length = len(oldname)
        items = []
        items.append(headcode)
        nodeinfo = []
        indexline = line_offset

        for place in places:
            placecode = "".join(["\n",gen.get_place_user_fn_header(place.get_id(),True),"{\n",place.code,"}\n"])
            endlines = placecode.count("\n")
            items.append(placecode)
            nodeinfo.append((indexline+3,indexline+endlines-1,place))
            indexline+=endlines
 
        for transition in transitions:
            headcode = gen.get_transition_user_fn_header(transition.get_id(),True)
            transitioncode = "".join([headcode,"{\n",transition.code,"}\n"])
            endlines = transitioncode.count("\n")
            items.append(transitioncode)
            nodeinfo.append((indexline + headcode.count("\n")+1,indexline+endlines-1,transition))
            indexline+=endlines

        allcode = "".join(items)
        tempbuffer.set_text(allcode)
        temptu.reparse([("c.cpp",allcode)])
        ref = clanglib.Cursor.from_location(temptu, location)
        where = []
        self.find_cursor_uses(temptu, temptu.cursor, ref, where)

        for loc in where:
            line = loc.line
            line -= 1
            col = loc.column - 1
            iter = tempbuffer.get_iter_at_line(line)
            iter.set_line_offset(col)
            mark = tempbuffer.create_mark(None,iter)
            startiter = tempbuffer.get_iter_at_mark(mark)
            enditer = startiter.copy()
            enditer.set_line_offset(enditer.get_line_offset() + length)
            tempbuffer.delete(startiter,enditer)
            tempbuffer.insert(startiter,newname)
            tempbuffer.delete_mark(mark)

        for info in nodeinfo:
            startline = info[0]
            endline = info[1]
            place = info[2]
            startiterline = tempbuffer.get_iter_at_line(startline)
            enditerline = tempbuffer.get_iter_at_line(endline)
            place.set_code(tempbuffer.get_text(startiterline,enditerline))

        startiterhead = tempbuffer.get_iter_at_line(self.clang.get_invisible_code_line_count() + 5)
        enditerhead = tempbuffer.get_iter_at_line(line_offset)
        enditerhead.forward_to_line_end()
        self.project.set_head_code(tempbuffer.get_text(startiterhead,enditerhead))
        window = self.app.window

        def reload_tab(tab):
            from codeedit import CodeEditor

            if(issubclass(tab.widget.__class__, CodeEditor)):
                if(isinstance(tab.key,str)):
                    tab.widget.set_text(self.project.get_head_code())
                else:
                    newcode = tab.key.get_code()
                    tab.widget.set_text(newcode)
        window.foreach_tab(reload_tab)

    def find_cursor_uses(self, tu, rootcursor, referencedCursor, places):
        currentfile = tu.spelling
        for c in rootcursor.get_children():
            if not c.location.file:
                continue
            if c.location.file.name == currentfile:
                ref = c.referenced
                if ref and ref == referencedCursor and not c.kind.is_unexposed():
                    kind = c.kind
                    if kind.value != 103:
                        places.append(c.location)
                self.find_cursor_uses(tu,c, referencedCursor, places)

    def text_inserted(self, buffer, iter, text, length):
        if self.insertedItem:
            proposal = self.lastSelectedItem
            placeholders = proposal.get_placeholders()
            if placeholders:
                if self.placeHolderObject is None:
                    self.placeHolderObject = PlaceHolderObject(iter,proposal,buffer,self.view)
                    self.insertedItem = False
                else:
                    self.placeHolderObject.hide()
                    self.placeHolderObject = PlaceHolderObject(iter,proposal,buffer,self.view)
                    self.insertedItem = False

        if text == "." and not self.window_showed:
            self.view.emit("show-completion")
        elif text == ">" and not self.window_showed:
            pos = buffer.get_property("cursor-position")
            iter = buffer.get_iter_at_offset(pos-2)
            char = iter.get_char()
            if char == "-":
                self.view.emit("show-completion")
        elif text == ":" and not self.window_showed:
            pos = buffer.get_property("cursor-position")
            iter = buffer.get_iter_at_offset(pos-2)
            char = iter.get_char()
            if char == ":":
                self.view.emit("show-completion")
        elif text == "(":
            self.view.buffer.insert_at_cursor(")")
            pos = self.view.buffer.get_property("cursor-position")
            iter = self.view.buffer.get_iter_at_offset(pos)
            iter.set_offset(iter.get_offset() -1)
            self.view.buffer.place_cursor(iter)
        elif text == "{":
            self.view.buffer.insert_at_cursor("}")
            pos = self.view.buffer.get_property("cursor-position")
            iter = self.view.buffer.get_iter_at_offset(pos)
            iter.set_offset(iter.get_offset() -1)
            self.view.buffer.place_cursor(iter)

    def text_changed(self, buffer):
        if self.window_showed:
            return
        else:
            self.codeChanged = True
            self.parse_source_code()

    def mouse_click(self, w, e):
        if gtk.keysyms.Control_L in self.keymap.keys and e.button == 1:
            self.goto_declaration(None)
            self.keymap.remove_all()
            return True

    def goto_declaration(self, w):
            cursor = self.get_cursor_under_mouse()

            if cursor:
                referenced = cursor.referenced

                if referenced:
                    location = referenced.location
                    file = os.path.normpath(location.file.name)

                    if file == self.clang.file:
                        line = location.line - self.clang.get_header_line_offset()
                        column = location.column - 1 # TODO: changed from +1 to -1?? clang cisluje od 1 
                        if line > 0 and self.clang.type == "header":
                            self.codeeditor.jump_to_position(("", line - 5, column))
                        elif line > 0:
                            iter = self.codeeditor.buffer.get_iter_at_line(line - 1)
                            iter.set_line_offset(column)
                            self.codeeditor.buffer.place_cursor(iter)
                            self.codeeditor.view.scroll_to_iter(iter, 0.1)
                        else:
                            offsets = self.clang.get_header_line_offset() - self.clang.get_invisible_code_line_count()
                            line_in_head = offsets + line - 5
                            if line_in_head >= 0:
                                self.app.edit_head(lineno = line_in_head)
                        return

                    from mainwindow import Tab
                    from codeedit import CodeFileEditor

                    codeedit = CodeFileEditor(self.app, self.project.get_syntax_highlight_key(), file)
                    codeedit.view.set_highlight_current_line(True)
                    codeedit.jump_to_position(("", location.line, 0))
                    window = self.app.window
                    tabname = os.path.basename(file)
                    tab = Tab(tabname, codeedit)
                    window.add_tab(tab,True)

    def _key_released(self, w, key):
        self.keymap.key_released(key)

    def _key_pressed(self, w, key):
        self.keymap.key_pressed(key)
        self.set_prefix(key)

        if self.placeHolderObject:
            if key.keyval in [gtk.keysyms.Escape,gtk.keysyms.semicolon]:
                self.placeHolderObject.hide()
                self.placeHolderObject = None
            if key.keyval == gtk.keysyms.Tab:
                self.placeHolderObject.next()
                return True

    def set_prefix(self, key):
        buffer = self.view.buffer
        pos = buffer.get_cursor_position()
        iter = buffer.get_iter_at_offset(pos)

        tmp = iter.copy()
        start = buffer.get_iter_at_line(iter.get_line()-1)
        text = buffer.get_text(start,tmp)
        text+= key.string

        if key.keyval == gtk.keysyms.BackSpace:
            text = text[:-1]

        char = ""
        pre = ""
        while len(text) > 0:
            if len(text) > 0:
                char = text[-1]
                if(char in Completion.prefixchars or len(text) == 0):
                    break
                else:
                    if char != "\r" and char != "\n" and char != "" and char != "\t":
                        pre+=char
                    text = text[:-1]

        pre = pre[::-1]
        if pre.isdigit():
            pre = ""
        self.prefix = pre

    def item_selected(self, iter, proposal):
        if self.view.buffer.get_has_selection():
            start,end = self.view.buffer.get_selection_bounds()
            self.view.buffer.delete(start,end)
        self.lastSelectedItem = proposal
        self.insertedItem = True

    def get_cursor(self, line, col):
        if self.tu:
            file = clanglib.File.from_name(self.tu,self.clang.file)
            location =  clanglib.SourceLocation.from_position(self.tu, file, line + self.clang.lineoffset, col)
            cursor = clanglib.Cursor.from_location(self.tu, location)
            return cursor
        else:
            return None

    def get_cursor_under_mouse(self):
        position = self.view.buffer.get_property("cursor-position")
        iter = self.view.buffer.get_iter_at_offset(position)
        line = iter.get_line() + 1
        col = iter.get_line_offset()
        cursor_left = self.get_cursor(line, col)
        cursor_right = self.get_cursor(line, col + 1)

        if not cursor_left and not cursor_right:
            return None

        if not (cursor_left.kind.is_invalid() or cursor_left.kind.is_unexposed()):
            return cursor_left
        elif not (cursor_right.kind.is_invalid() or cursor_right.kind.is_unexposed()):
            return cursor_right
        else:
            return None

    def is_object_pointer(self, cursor):
        bitype = cursor.type.kind.spelling
        if bitype == "Pointer":
            pp = cursor.type.get_pointee()
            while pp.kind.spelling == "Pointer":
                pp = pp.get_pointee()

            pp = pp.kind.spelling

            if pp == "Record":
                pointertype =  cursor.type.get_pointee()
                while pointertype.kind.spelling == "Pointer":
                    pointertype = pointertype.get_pointee()
                pointertype = pointertype.get_declaration().spelling
                return True
            else:
                return False
        else:
            return False

    def is_object(self, cursor):
        bitype = cursor.type.kind.spelling
        if bitype == "Record":
            type =  cursor.type.get_declaration().spelling
            return True
        else:
            return False

    def parse_source_code(self):
        if not self.window_showed:
            try:
                if self.tu is None:
                    self.tu = self.clang.parse()
                    self.clang.reparse()
                    self._show_code_errors()
                else:
                    self.clang.reparse()
                    self._show_code_errors()
            except Exception,e:
                self.app.window.console.write(e,"normal")

    def _show_code_errors(self):
        self.view.buffer.remove_tag_by_name("error",self.view.buffer.get_start_iter(),self.view.buffer.get_end_iter())
        self.codeErrorList.clear()

        for d in self.tu.diagnostics:
            if d.severity == 3:
                location = d.location
                info = d.spelling
                line = location.line-1 - self.clang.lineoffset
                iter = self.view.buffer.get_iter_at_line(line)
                if iter.get_chars_in_line() < location.column:
                    continue
                starthighlightcol = location.column-1
                endhighlightcol = None
                fixhit = ""

                for range in d.ranges:
                    s = range.start.column
                    e = range.end.column
                    starthighlightcol = s-1
                    endhighlightcol = e
                for fix in d.fixits:
                    s = fix.range.start.column
                    e = fix.range.end.column
                    val = fix.value
                    fixhit = val
                    starthighlightcol = s-2
                    endhighlightcol = e-1

                if fixhit:
                    info += "\nFix hit: \" " + fixhit + " \""

                startiter = self.view.buffer.get_iter_at_line(line)
                if starthighlightcol >= 0 and starthighlightcol <= startiter.get_chars_in_line():
                    startiter.set_line_offset(starthighlightcol)

                enditer = startiter.copy()

                if not endhighlightcol:
                    enditer.forward_visible_word_ends(1)
                else:
                    if endhighlightcol >= starthighlightcol and endhighlightcol <= enditer.get_chars_in_line():
                        enditer.set_line_offset(endhighlightcol)

                endhighlightcol = enditer.get_line_offset()
                self.view.buffer.apply_tag_by_name("error",startiter,enditer)
                self.codeErrorList[line] = (starthighlightcol,endhighlightcol,info)

            if d.severity > 2 and d.location.file == self.tu.spelling:
                pass

    def get_proposals(self, context):
        iter = context.get_iter()
        results = []
        line,col = (iter.get_line() +1, iter.get_line_offset() +1)
        col -= len(self.prefix)
        resultsToShow = None

        if not self.window_showed and (self.codeChanged or ((self.lastCC[0] != iter.get_line() or self.lastCC[1] != iter.get_line_offset() - len(self.prefix)))):
            results = self.clang.code_complete(line,col)

            if results and len(results.results) > 0:
                #cProfile.runctx("self.format_results(results)", globals(), locals(),sort = 1)
                for i in range(len(results.diagnostics)):
                    diag = results.diagnostics[i]
                    if diag.severity > 2:
                        loc = diag.location
                        if loc.line - self.clang.lineoffset == line:
                            context.add_proposals(self.provider,[],True)
                            return
                self.results = self.format_results(results)
            else:
                self.results = []

            self.lastCC = (iter.get_line(),iter.get_line_offset())
            self.codeChanged = False
            resultsToShow = self.results
        else:
            temp = list(self.results)
            filtered = []
            for i in xrange(len(temp)):
                t = temp[i]
                label = t.get_label()
                if label.startswith(self.prefix):
                    filtered.append(t)
            resultsToShow = filtered

        context.add_proposals(self.provider,resultsToShow,True)

    def format_results(self, results):
        proposals = []

        for result in results.results:
            resultType = None
            resultKind = result.cursorKind
            priority = 0
            iconname = ""
            info = ""
            typedtext = ""
            labeltext = ""
            placeholder = None
            #briefcomment = result.string.briefComment
            avail = result.string.availability

            if avail.name != "Available":
                continue
            if self.kindmap.has_key(resultKind):
                priority,iconname = self.kindmap[resultKind]
            else:
                continue
            chunkslist = []

            for chunk in result.string:

                if chunk.isKindInformative():
                    continue
                if chunk.isKindResultType():
                    resultType = chunk
                    continue

                chunkinfo = chunk.spelling
                chunkslist.append(chunkinfo)

                if chunk.isKindTypedText():
                    typedtext = labeltext = chunkslist[-1]
                else:
                    typedtext = ''.join(chunkslist)
                if chunk.isKindPlaceHolder():
                    if not placeholder:
                        placeholder = []
                    placeholder.append(chunkslist[-1])

            info = ''.join(chunkslist)

            if self.prefix and not typedtext.startswith(self.prefix):
                continue

            if priority == -40 and not placeholder:
                iconname = "keyword"
                priority = -35

            icon = self.icons[iconname]

            if resultType:
                labeltext = ''.join((labeltext,"  ->  ",resultType.spelling))

            item = ProposalItem(labeltext,typedtext,icon)
            #item.setInfoText(info)

            #bc = briefcomment.spelling
            # if bc:
            #    info += "\nInformation: " + bc
            if placeholder:
                item.set_placeholders(placeholder)
            item.set_property("info",info)

            proposals.append((item,priority))

        self.sort_proposals(proposals)
        prop = [ r[0] for r in proposals ]
        return prop

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
            self.resultKindFilter.filter_by_name(mask)
            self.kindmap = self.resultKindFilter.get_map()
