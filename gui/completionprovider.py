#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
