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


import gtksourceview2 as gtksource
import gobject

class ProposalItem(gobject.GObject, gtksource.CompletionProposal):

    def __init__(self, label, text, info, icon):
        gobject.GObject.__init__(self)
        self.placeholders = None
        self.label = label
        self.text = text
        self.info = info
        self.icon = icon

    def do_get_info(self):
        return self.info

    def do_get_label(self):
        return self.label

    def do_get_icon(self):
        return self.icon

    def do_get_text(self):
        return self.text

    def get_placeholders(self):
        return self.placeholders

    def set_placeholders(self, list):
        self.placeholders = list

    def set_info(self, info):
        self.info = info

gobject.type_register(ProposalItem)