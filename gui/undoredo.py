#
#    Copyright (C) 2012 Martin Kozubek
#                  2012 Lukas Tomaszek
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


class UndoRedo:

    def __init__(self, button_undo, button_redo):
        self.button_undo = button_undo
        self.button_redo = button_redo
        self.undolist = []
        self.redolist = []
        self.action = False
        self.set_buttons()

    def undo(self):
        if len(self.undolist) != 0:
            self.action = True
            action_undo = self.undolist.pop()
            action_undo.undo()
            self.redolist.append(action_undo)
            self.button_redo.set_sensitive(True)
            self.button_undo.set_sensitive(len(self.undolist) != 0)
            self.action = False
            
        else:
            self.button_undo.set_sensitive(False)


    def redo(self):
        if len(self.redolist) != 0:
            self.action = True
            action_redo = self.redolist.pop()
            action_redo.redo()
            self.undolist.append(action_redo)
            self.button_undo.set_sensitive(True)
            self.button_redo.set_sensitive(len(self.redolist) != 0)
            self.action = False
        else:
            self.button_redo.set_sensitive(False)

    def add_to_list(self, action):
        if not self.action:
            self.undolist.append(action)
            self.redolist = []
            self.button_undo.set_sensitive(True)
            self.button_redo.set_sensitive(False)

    def set_buttons(self):
        self.button_undo.set_sensitive(len(self.undolist) != 0)
        self.button_redo.set_sensitive(len(self.redolist) != 0)
 
            
class IUndoRedoAction:

    def __init__(self):
        if self.__class__ is IUndoRedoAction: raise NotImplementedError

    def undo(self):
        raise NotImplementedError

    def redo(self):
        raise NotImplementedError


class AddAction(IUndoRedoAction):

    def __init__(self, net, item):
        self.item = item
        self.net = net

    def undo(self):
        self.net.remove_item(self.item)

    def redo(self):
        self.net.add_item(self.item)


class RemoveAction(IUndoRedoAction):

    def __init__(self, net, item):
        self.item = item
        self.net = net

    def undo(self):
        self.net.add_item(self.item)

    def redo(self):
        self.net.remove_item(self.item)


class MoveAction(IUndoRedoAction):

    def __init__(self, item, original_poz, new_poz):
        self.item = item
        self.new_poz = new_poz
        self.original_poz = original_poz

    def undo(self):
        self.item.set_position(self.original_poz)

    def redo(self):
        self.item.set_position(self.new_poz)


class MoveActionEdge(IUndoRedoAction):

    def __init__(self, item, original_points, new_points):
        self.item = item
        self.new_points = new_points
        self.original_points = original_points

    def undo(self):
        self.item.set_all_points(self.original_points)

    def redo(self):
        self.item.set_all_points(self.new_points)

class MoveActionTextEdge(IUndoRedoAction):

    def __init__(self, item, original_pozition_text, new_pozition_text):
        self.item = item
        self.new_pozition_text = new_pozition_text
        self.original_pozition_text = original_pozition_text

    def undo(self):
        self.item.set_inscription_position(self.original_pozition_text)

    def redo(self):
        self.item.set_inscription_position(self.new_pozition_text)

class ResizeAction(IUndoRedoAction):

    def __init__(self, item, original_size, new_size):
        self.item = item
        self.new_size = new_size
        self.original_size = original_size

    def undo(self):
        self.item.resize(self.original_size)

    def redo(self):
        self.item.resize(self.new_size)


class RenameAction(IUndoRedoAction):

    def __init__(self, last_text, get, set):
        self.last_text = last_text
        self.get = get
        self.set = set

    def undo(self):
        text = self.last_text
        self.last_text = self.get()
        self.set(text)

    def redo(self):
        text = self.last_text
        self.last_text = self.get()
        self.set(text)

class ResizeArea(IUndoRedoAction):

    def __init__(self, item, size, position, new_size, new_position):
        self.item = item
        self.size = size
        self.position = position
        self.new_size = new_size
        self.new_position = new_position

    def undo(self):
        self.item.set_position(self.position)
        self.item.set_size(self.size)

    def redo(self):
        self.item.set_position(self.new_position)
        self.item.set_size(self.new_size)














