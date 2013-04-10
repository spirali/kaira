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

def analyze_transition(tr):
    variable_sources = {} # string -> uid
    reuse_tokens = {} # uid -> uid
    fresh_tokens = [] # (uid, type)
    used_tokens = []  # [uid]
    variable_sources_out = {} # string -> uid

    def edge_out_weight(edge):
        # Unconditional edges has higher priority
        if edge.config.get("if"):
            return 1
        else:
            return 0

    for edge in tr.edges_in:
        for name, uid in edge.get_variable_sources().items():
            if name not in variable_sources:
                variable_sources[name] = uid

    edges_out = tr.edges_out[:]
    # Sort by weights
    edges_out.sort(key=edge_out_weight)

    for edge in edges_out:
        for name, uid in edge.get_variable_sources().items():
            if uid is None: # Bulk edge
                if name not in variable_sources:
                    variable_sources_out[name] = None
                continue

            if name in variable_sources:
                token_uid = variable_sources[name]
                if edge.is_local() and token_uid not in used_tokens:
                    reuse_tokens[uid] = token_uid
                    used_tokens.append(token_uid)
            elif edge.is_local() and variable_sources_out.get(name) is None:
                # Edge is local and variable sources is not defined or
                # it is None (it means fresh variable, without tokens)
                fresh_tokens.append((uid, edge.get_place_type()))
                variable_sources_out[name] = uid
                reuse_tokens[uid] = uid
            elif name not in variable_sources_out:
                variable_sources_out[name] = None

    for edge in edges_out:
        for variable in edge.get_nontoken_variables():
            if variable not in variable_sources and \
               variable not in variable_sources_out:
                variable_sources_out[variable] = None

    tr.variable_sources = variable_sources
    tr.reuse_tokens = reuse_tokens
    tr.variable_sources_out = variable_sources_out
    tr.fresh_tokens = fresh_tokens
