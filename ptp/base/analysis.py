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

import utils

def all_free_variables(edges):
    return utils.unions(edges, lambda edge: edge.get_free_vars())

def get_variable_sources(inscriptions):
    sources = {}
    for inscription in inscriptions:
        if not inscription.is_expr_variable():
            continue

        if sources.get(inscription.expr):
            continue

        if inscription.is_bulk():
            sources[inscription.expr] = None
        else:
            sources[inscription.expr] = inscription.uid

    return sources

def is_dependant(inscription1, inscription2):
    if inscription1.edge is inscription2.edge and \
       inscription2.index < inscription1.index:
        return True
    if not inscription2.is_expr_variable():
        return False
    return inscription2.expr in inscription1.get_foreign_variables()

def analyze_transition(tr):
    variable_sources = {} # string -> uid - which inscriptions carry input variables
    reuse_tokens = {} # uid -> uid - identification number of token for output inscpription
    fresh_tokens = [] # (uid, type) - what tokens has to be created for output
    used_tokens = []  # [uid] - Tokens from input inscriptions that are reused on output
    variable_sources_out = {} # string -> uid or None
    bulk_overtake = [] # [uid]
    overtaken_variables = set()

    def inscription_out_weight(inscription):
        # Reorder edges, bulk edges first because we want them send first
        # Otherwise it can cause problems like in sending results in "workers" example
        s = inscription.config.get("seq")
        if s is None:
            seq = 0
        else:
            seq = int(s) * 3
        if inscription.is_bulk():
            return seq
        # Unconditional edges has higher priority
        if inscription.is_conditioned():
            return seq + 2
        else:
            return seq + 1

    def inscription_in_weight(inscription):
        if inscription.is_conditioned():
            return 1
        else:
            return 0

    inscriptions_in = sum((edge.inscriptions for edge in tr.edges_in), [])
    inscriptions_in.sort(key=inscription_in_weight)
    inscriptions_out = sum((edge.inscriptions for edge in tr.edges_out), [])
    inscriptions_out.sort(key=inscription_out_weight)

    variable_sources = get_variable_sources(inscriptions_in)
    # Order input inscriptions by variable dependancy
    inscriptions_in = utils.topological_ordering(inscriptions_in, is_dependant)
    if inscriptions_in is None:
        raise utils.PtpException("Circle variable dependancy", tr.get_source())

    # Try reuse tokens
    for inscription in inscriptions_out:
        if inscription.is_bulk() or not inscription.is_local():
            continue # Bulk and nonlocal edge cannot use token reusage
        if not inscription.is_expr_variable():
            continue # Current implementation reuses tokens only for variable expression
        if inscription.is_collective():
            continue # Collective operations cannot use token reusage
        token_uid = variable_sources.get(inscription.expr)
        if token_uid is None or token_uid in used_tokens:
            # Variable is not taken from input as token
            # or token is already reused --> reusage not possible
            continue

        reuse_tokens[inscription.uid] = token_uid
        used_tokens.append(token_uid)

    # Setup fresh variables where token was not reused
    for inscription in inscriptions_out:
        if not inscription.is_expr_variable():
            continue # We are interested only in variables
        variable = inscription.expr
        if variable in variable_sources:
            # Variable take from input so we do not have to deal here with it
            continue
        if variable in variable_sources_out:
            # Variable already prepared for output
            continue
        if inscription.is_bulk():
            # No token, just build variable
            variable_sources_out[variable] = None
            continue
        if inscription.is_local():
            # Local send, we prepare token
            fresh_tokens.append((inscription.uid, inscription.edge.place.type))
            variable_sources_out[variable] = inscription.uid
            reuse_tokens[inscription.uid] = inscription.uid # Use this fresh new token
        else:
            # Just create variable
            variable_sources_out[variable] = None

    for inscription in reversed(inscriptions_out):
        # Now we are checking overtake. It has to be in reversed order
        # becacase overtake has to be the last operation on variable
        if not inscription.is_bulk() or not inscription.is_expr_variable():
            continue # We are interested only in variables and bulk inscriptions
        if inscription.expr not in overtaken_variables:
            overtaken_variables.add(inscription.expr)
            bulk_overtake.append(inscription.uid)

    for inscription in inscriptions_out:
        for variable in inscription.get_other_variables():
            if variable not in variable_sources and \
               variable not in variable_sources_out:
                variable_sources_out[variable] = None

    tr.inscriptions_in = inscriptions_in
    tr.inscriptions_out = inscriptions_out
    tr.variable_sources = variable_sources
    tr.reuse_tokens = reuse_tokens
    tr.variable_sources_out = variable_sources_out
    tr.fresh_tokens = fresh_tokens
    tr.bulk_overtake = bulk_overtake
