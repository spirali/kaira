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

    # Get rid of packing edges or with taget or with guard
    project = tr.net.project
    edges_in = tr.get_normal_edges_in()

    variables = []
    var_edges = []
    match_edges = []

    for edge in edges_in:
        expr = edge.expr
        if project.is_expr_variable(expr) and expr not in variables:
            variables.append(expr)
            var_edges.append(edge)
        else:
            match_edges.append(edge)

    tr.var_edges = var_edges
    tr.match_edges = match_edges
