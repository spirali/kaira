#!/usr/bin/env python
# -*- coding: utf-8 -*-
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


import gobject
import gtksourceview2 as gtksource

import time
import cProfile

class CompletionProvider(gobject.GObject,gtksource.CompletionProvider):

    def __init__(self, completion):
        gobject.GObject.__init__(self)
        self.completion = completion

    def do_match(self, context):
        return True

    def do_get_info_widget(self, w):
        win = self.completion.completion.get_info_window()
        wid = win.get_widget()
        wid.set_text(w.get_property("info"))
        return wid

    def do_activate_proposal(self, proposals, iter):
        self.completion.item_selected(iter,proposals)

    def do_get_name(self):
        return "All Proposals"

    def do_populate(self, context):
        if context.get_activation() == gtksource.COMPLETION_ACTIVATION_USER_REQUESTED:
            firsttime = time.time()
            #cProfile.runctx("self.completion.getProposals(context)", globals(), locals(),sort = 1)
            self.completion.get_proposals(context)
            secondtime = time.time()

gobject.type_register(CompletionProvider)
