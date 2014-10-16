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

class ProposalItem(gtksource.CompletionItem):

    def __init__(self, label, typedtext, icon):
        gtksource.CompletionItem.__init__(self, label, typedtext, icon)
        self.placeholders = None
        self.infotext = ""

    def set_placeholders(self, list):
        self.placeholders = list

    def get_placeholders(self):
        return self.placeholders

    def set_info_text(self, text):
        self.infotext = text

    def get_info_text(self):
        return self.infotext