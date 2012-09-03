#
#    Copyright (C) 2012 Martin Kozubek
#                  2012 Lukas Tomaszek
#                  2012 Stanislav Bohm
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


class UndoList:

    def __init__(self):
        self.undolist = []
        self.redolist = []
        self.block_add = False

    def undo(self):
        if len(self.undolist) > 0:
            self.block_add = True
            action_undo = self.undolist.pop()
            action_undo.undo()
            self.redolist.append(action_undo)
            self.block_add = False

    def redo(self):
        if len(self.redolist) > 0:
            self.block_add = True
            action_redo = self.redolist.pop()
            action_redo.redo()
            self.undolist.append(action_redo)
            self.block_add = False

    def add(self, action):
        if not self.block_add:
            self.undolist.append(action)
            self.redolist = []

    def set_buttons(self, button_undo, button_redo):
        button_undo.set_sensitive(len(self.undolist) > 0)
        button_redo.set_sensitive(len(self.redolist) > 0)


class UndoRedoAction:
    pass

class AddItemAction(UndoRedoAction):

    def __init__(self, net, item):
        self.item = item
        self.net = net

    def undo(self):
        self.net.remove_item(self.item)

    def redo(self):
        self.net.add_item(self.item)


class RemoveAction(UndoRedoAction):

    def __init__(self, net, item):
        self.item = item
        self.net = net

    def undo(self):
        self.net.add_item(self.item)

    def redo(self):
        self.net.remove_item(self.item)


class SetValueAction(UndoRedoAction):

    def __init__(self, set_fn, original_value, new_value):
        self.set_fn = set_fn
        self.new_value = new_value
        self.original_value = original_value

    def undo(self):
        self.set_fn(self.original_value)

    def redo(self):
        self.set_fn(self.new_value)
