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
    variable_sources = {}

    for edge in tr.edges_in:
        for name, uid in edge.get_variable_sources().items():
            if name not in variable_sources:
                variable_sources[name] = uid

    reuse_tokens = {}
    variable_freshes = []

    used_tokens = []

    for edge in tr.edges_out:
        for name, uid in edge.get_variable_sources().items():
            if name in variable_sources:
                token_uid = variable_sources[name]
                if edge.is_local() and token_uid not in used_tokens:
                    reuse_tokens[uid] = token_uid
                    used_tokens.append(token_uid)
            elif name not in variable_freshes:
                variable_freshes.append(name)

    tr.variable_sources = variable_sources
    tr.reuse_tokens = reuse_tokens
    tr.variable_freshes = variable_freshes
