#
#    Copyright (C) 2013 Stanislav Bohm
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


class UndoManager:

    def __init__(self):
        self.undo_actions = []
        self.redo_actions = []

    def add_action(self, action):
        if self.undo_actions and self.undo_actions[-1].check_supress_action(action):
            return
        if self.redo_actions:
            self.redo_actions = []
        self.undo_actions.append(action)

    def perform_undo(self):
        if self.undo_actions:
            action = self.undo_actions.pop()
            self.redo_actions.append(action.perform())

    def perform_redo(self):
        if self.redo_actions:
            action = self.redo_actions.pop()
            self.undo_actions.append(action.perform())

    def has_undo(self):
        return bool(self.undo_actions)

    def has_redo(self):
        return bool(self.redo_actions)


class ActionBase:

    def check_supress_action(self, action):
        False


class Action(ActionBase):

    def __init__(self, fn, reverse_fn):
        self.fn = fn
        self.reverse_fn = reverse_fn

    def perform(self):
        self.fn()
        return Action(self.reverse_fn, self.fn)


class ActionSet(ActionBase):

    suppress_similar = False

    def __init__(self, get_fn, set_fn, value, suppress_similar=False):
        self.get_fn = get_fn
        self.set_fn = set_fn
        self.value = value
        if suppress_similar:
            self.suppress_similar = True

    def perform(self):
        old_value = self.get_fn()
        self.set_fn(self.value)
        return ActionSet(self.get_fn, self.set_fn, old_value)

    def check_supress_action(self, action):
        if self.suppress_similar:
            return (isinstance(action, ActionSet) and
                    action.get_fn == self.get_fn and
                    action.set_fn == self.set_fn)
        else:
            return False


class ActionSetAttr(ActionBase):

    def __init__(self, obj, name, value):
        self.obj = obj
        self.name = name
        self.value = value

    def perform(self):
        old_value = getattr(self.obj, self.name)
        setattr(self.obj, self.name, self.value)
        return ActionSetAttr(self.obj, self.name, old_value)


class GroupAction(ActionBase):

    def __init__(self, actions=None):
        if actions is None:
            self.actions = []
        else:
            self.actions = actions

    def add_action(self, action):
        self.actions.append(action)

    def perform(self):
        return GroupAction([ action.perform() for action in reversed(self.actions) ])
