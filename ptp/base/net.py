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

import utils as utils
from base.expressions import ExprVar
from base.neltypes import derive_context
from base.neltypes import t_bool, t_array, t_int, t_double, t_string
import analysis

class EdgeBase(utils.EqMixin):

    """
        Edge has attribute uid because id (provided by gui) is not necessary unique.
        It occurs in net with (graphical) edge with inscription "x;y", then gui creates
        two edges with same id
    """
    def __init__(self, id, place, transition):
        self.id = id
        self.uid = utils.get_unique_id()
        self.place = place
        self.transition = transition

    def get_place(self):
        return self.place

    def get_place_type(self):
        return self.place.type

    def get_transition(self):
        return self.transition

class EdgeIn(EdgeBase):

    # setup by analysis
    token_reused = False

    def __init__(self, id, place, transition, expr):
        EdgeBase.__init__(self, id, place, transition)
        self.expr = expr

    def get_equations(self):
        return [ (self.expr, self.get_place_type()) ]

    def inject_types(self, env, context):
        self.expr.inject_types(env, context)

    def is_normal(self):
        return True

    def is_packing(self):
        return False

    def get_free_vars(self):
        return self.expr.get_free_vars()

class EdgeInPacking(EdgeBase):

    def __init__(self, id, place, transition, varname, limit):
        EdgeBase.__init__(self, id, place, transition)
        self.varname = varname
        self.limit = limit

    def get_equations(self):
        source = utils.get_source_path(self.id, "inscription")
        return [ (ExprVar(self.varname, source), t_array(self.get_place_type())),
                (self.limit, t_int.copy(source)) ]

    def inject_types(self, env, context):
        self.limit.inject_types(env, context)

    def is_normal(self):
        return False

    def is_packing(self):
        return True

    def get_free_vars(self):
        return set([self.varname])

class EdgeOut(EdgeBase):

    # setup by analysis
    token_source = None # EdgeIn or None

    def __init__(self, id, place, transition, expr, mode, sendmode, target, guard):
        EdgeBase.__init__(self, id, place, transition)
        self.expr = expr
        self.mode = mode # 'normal' | 'packing'
        self.sendmode = sendmode # 'unicast' | 'multicast' | 'local'
        self.target = target
        self.guard = guard

    def is_normal(self):
        return self.mode == 'normal'

    def is_packing(self):
        return self.mode == 'packing'

    def is_local(self):
        return self.sendmode == 'local'

    def is_unicast(self):
        return self.sendmode == 'unicast'

    def is_multicast(self):
        return self.sendmode == 'multicast'

    def get_equations(self):
        if self.is_normal():
            eq = [ (self.expr, self.get_place_type()) ]
        else:
            eq = [ (self.expr, t_array(self.get_place_type())) ]
        if self.is_unicast():
            eq.append((self.target, t_int))
        if self.is_multicast():
            eq.append((self.target, t_array(t_int)))
        if self.guard:
            eq.append((self.guard, t_bool))
        return eq

    def inject_types(self, env, context):
        self.expr.inject_types(env, context)
        if self.target:
            self.target.inject_types(env, context)
        if self.guard:
            self.guard.inject_types(env, context)

class Place(utils.EqByIdMixin):

    code = None

    def __init__(self, net, id, type, init_expression):
        self.net = net
        self.id = id
        self.type = type
        self.init_expression = init_expression
        self.tracing = []

    def inject_types(self):
        if self.init_expression is not None:
            inject_types_for_empty_context(self.net.project.get_env(),
                                           self.init_expression,
                                           t_array(self.type))

    def get_pos_id(self):
        return self.net.places.index(self)

    def get_edges_in(self, with_interface = False):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_out:
                if edge.get_place() == self:
                    result.append(edge)
        if with_interface:
            for edge in self.net.get_interface_edges_out():
                if edge.get_place() == self:
                    result.append(edge)
        return result

    def get_edges_out(self):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_in:
                if edge.get_place() == self:
                    result.append(edge)
        return result

    def get_transitions_out(self):
        return list(set([ edge.get_transition() for edge in self.get_edges_out() ]))

    def get_transitions_in(self):
        return list(set([ edge.get_transition() for edge in self.get_edges_in() ]))

    def get_areas(self):
        return self.net.get_areas_with_place(self)

    def get_functions_for_tracing(self, project):
        p = [ self.type ]
        if self.type.name == "":
            q = list(self.type.args)
            return (project.get_user_functions_by_declaration(t_int, p) +
               project.get_user_functions_by_declaration(t_double, p) +
               project.get_user_functions_by_declaration(t_string, p) +
               project.get_user_functions_by_declaration(t_int, q) +
               project.get_user_functions_by_declaration(t_double, q) +
               project.get_user_functions_by_declaration(t_string, q))

        return (project.get_user_functions_by_declaration(t_int, p) +
               project.get_user_functions_by_declaration(t_double, p) +
               project.get_user_functions_by_declaration(t_string, p))


class Transition(utils.EqByIdMixin):

    code = None

    # After analysis it is dictionary variable_name -> EdgeIn
    # It returns edge (= token) where variable should be gather
    var_edge = None

    def __init__(self, net, id, guard):
        self.net = net
        self.id = id
        self.guard = guard
        self.edges_in = []
        self.edges_out = []
        self.tracing = []

    def trace(self):
        if self.is_any_place_traced():
            return True
        if self.code and self.code.find("ctx.trace_") != -1:
            return True
        return self.tracing

    def get_normal_edges_out(self):
        return [ edge for edge in self.edges_out if edge.is_normal() ]

    def get_packing_edges_out(self):
        return [ edge for edge in self.edges_out if edge.is_packing() ]

    def get_normal_edges_in(self):
        return [ edge for edge in self.edges_in if edge.is_normal() ]

    def get_packing_edges_in(self):
        return [ edge for edge in self.edges_in if edge.is_packing() ]

    def get_context(self):
        return derive_context(self.net.project.get_env(), self.get_equations())

    def get_all_edges(self):
        return self.edges_in + self.edges_out

    def get_basic_input_edges(self):
        return self.edges_in

    def get_input_places(self):
        return set([ edge.place for edge in self.edges_in ])

    def get_output_places(self):
        return set([ edge.place for edge in self.edges_in ])

    def get_places(self):
        return self.get_input_places() & self.get_output_places()

    def is_any_place_traced(self):
        return any(place.tracing for place in self.get_places())

    def get_equations(self):
        result = []
        for e in self.get_all_edges():
            result += e.get_equations()
        if self.guard:
            result.append((self.guard, t_bool))
        return result

    def get_types(self):
        return set([ t for _, t in self.get_equations() ])

    def inject_types(self):
        env = self.net.project.get_env()
        eq = []
        for e in self.get_all_edges():
            eq += e.get_equations()
        context = derive_context(env, eq)

        for e in self.get_all_edges():
            e.inject_types(env, context)

    def get_pos_id(self):
        return self.net.transitions.index(self)

    def is_local(self):
        return all((edge.is_local() for edge in self.edges_out))

    def get_source(self):
        return "*{0}".format(self.id)

class Area(object):

    def __init__(self, net, id, expr, places):
        self.net = net
        self.id = id
        self.places = places
        self.expr = expr

    def inject_types(self):
        inject_types_for_empty_context(self.net.project.get_env(), self.expr, t_array(t_int))

    def is_place_inside(self, place):
        return place in self.places

def inject_types_for_empty_context(env, expr, t):
    eq = [ (expr, t) ]
    context = derive_context(env, eq)
    if context != {}:
        raise Exception("Variables occurs in initial expression")
    expr.inject_types(env, context)

class Net(object):

    def __init__(self, project, id, name):
        self.project = project
        self.id = id
        self.name = name
        self.places = []
        self.transitions = []
        self.areas = []
        self.interface_edges_in = []
        self.interface_edges_out = []
        self.module_flag = False

    def get_name(self):
        return self.name

    def is_module(self):
        return self.module_flag

    def get_interface_edges_out(self):
        return self.interface_edges_out

    def get_interface_edges_in(self):
        return self.interface_edges_in

    def get_place(self, id):
        for place in self.places:
            if place.id == id:
                return place

    def get_transition(self, id):
        for transition in self.transitions:
            if transition.id == id:
                return transition

    def get_all_types(self):
        result = set()
        for place in self.places:
            result.add(place.type)
        for tr in self.transitions:
            for t in tr.get_types():
                result.update(t.get_subtypes())
        return result

    def get_index(self):
        return self.project.nets.index(self)

    def get_module_index(self):
        index = 0
        for net in self.project.nets:
            if net.is_module():
                if net == self:
                    return index
                index += 1

    def inject_types(self):
        for place in self.places:
            place.inject_types()
        for tr in self.transitions:
            tr.inject_types()
        for area in self.areas:
            area.inject_types()
        self.inject_types_interface()

    def get_interface_context(self):
        eq = []
        env = self.project.get_env()
        for e in self.interface_edges_out + self.interface_edges_in:
            eq += e.get_equations()
        return derive_context(env, eq)

    def get_module_input_vars(self):
        return set().union(*[ e.expr.get_free_vars() for e in self.interface_edges_out ])

    def get_module_output_vars(self):
        return set().union(* [ e.get_free_vars() for e in self.interface_edges_in ])

    def inject_types_interface(self):
        context = self.get_interface_context()
        env = self.project.get_env()
        for e in self.interface_edges_in + self.interface_edges_out:
            e.inject_types(env, context)

    def get_areas_with_place(self, place):
        return [ area for area in self.areas if area.is_place_inside(place) ]

    def is_local(self):
        return all((tr.is_local() for tr in self.transitions))

    def check(self):
        """ Check consistency of net, raise exception if something is wrong,
            inject_types exptected before calling check"""

        for t in self.get_all_types():
            t.check(self.project)

    def analyze(self):
        for tr in self.transitions:
            analysis.analyze_transition(tr)
