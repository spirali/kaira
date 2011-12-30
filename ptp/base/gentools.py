#
#    Copyright (C) 2011 Stanislav Bohm
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

from base.expressions import ISet, IIf, ExprExtern, ExprVar, IExtern, ExprCall, INoop
from base.utils import topological_ordering
from base.project import order_input_edges

def match_expression(env, context, expr, covered_vars, token):
        def depends_on(x, y):
            _, a = x
            _, b = y
            return len(a.get_direct_vars().intersection(b.get_undirect_vars())) != 0

        pairing = expr.get_direct_pairing(token)

        for a, b in pairing:
            a.inject_types(env, context)
            b.inject_types(env, context)

        ordered = topological_ordering(pairing, depends_on)
        assert ordered is not None

        c = covered_vars.copy()
        code = []
        for e1, e2 in ordered:
            if isinstance(e2, ExprVar) and e2.name not in c:
                c.add(e2.name)
                code.append(ISet(e2.name, e1))
            else:
                code.append(IIf(ExprCall("!=", [ e2, e1 ]), IExtern("fail")),)
        return code, c

def get_all_subtypes(types):
    s = set()
    for t in types:
        s.update(t.get_subtypes())
    return s

def get_ordered_types(project):
    types_set = get_all_subtypes(project.get_all_types())
    return topological_ordering(list(types_set), lambda a, b: b.depends_on(a))

def get_edges_mathing(project, tr):
    env = project.get_env()
    context = tr.get_context()
    matches = []
    covered = set()
    guard = tr.guard
    initcode = []

    if guard and len(guard.get_free_vars()) == 0:
        initcode.append(IIf(guard, INoop(), IExtern("fail")))
        guard = None

    for edge in order_input_edges(tr.get_normal_edges_in()):
        token = ExprExtern("token", edge.get_place().type)
        instrs, covered = match_expression(env, context, edge.expr, covered, token)
        if guard and guard.get_free_vars().issubset(covered):
            instrs.append(IIf(guard, INoop(), IExtern("fail")))
            guard = None
        matches.append((edge, instrs))

    return matches, initcode
