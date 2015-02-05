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

class CompletionProvider(gobject.GObject, gtksource.CompletionProvider):

    def __init__(self, completion):
        gobject.GObject.__init__(self)
        self.completion = completion
        self.proposals_count = 0

    def do_match(self, context):
        return True

    def set_proposals_count(self, value):
        self.proposals_count = value 

    def do_get_info_widget(self, proposal):
        window = self.completion.completion.get_info_window()
        window.props.default_width = 100
        window.props.shrink_width = False
        window.props.default_height = 60
        window.props.shrink_height = False

        label = window.get_widget()
        info_text = proposal.get_text()

#         place_holders = proposal.get_placeholders()
#         if place_holders:
#             info_text += "\nPlaceholders: "
#             for i in range(1, len(place_holders) - 1):
#                 ph = place_holders[i]
#                 info_text += ph

        label.set_text(info_text)
        return label

    def do_activate_proposal(self, proposals, iter):
        self.completion.item_selected(iter, proposals)

    def do_get_name(self):
        return "All Proposals - count " + str(self.proposals_count)

    def do_populate(self, context):
        if context.get_activation() == gtksource.COMPLETION_ACTIVATION_USER_REQUESTED:
            #cProfile.runctx("self.completion.get_proposals(context)", globals(), locals(),sort = 1)
            self.completion.get_proposals(context)

gobject.type_register(CompletionProvider)
