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
import analysis


class Edge(utils.EqMixin):

    def __init__(self, id, transition, place, inscriptions, config):
        self.id = id
        self.uid = utils.get_unique_id()
        self.place = place
        self.transition = transition
        for inscription in inscriptions:
            inscription.edge = self
        self.inscriptions = inscriptions
        self.config = config

    def get_source(self):
        return "*{0}/inscription".format(self.id)

    def get_place_type(self):
        return self.place.type

    def check(self, checker):
        for inscription in self.inscriptions:
            checker.check_expression(inscription.expr,
                                     self.transition.get_decls(),
                                     self.get_place_type(),
                                     self.get_source())

    def get_decls(self):
        return [ (inscription.expr, self.get_place_type()) for inscription in self.inscriptions
                    if inscription.is_variable() ]

    def get_variable_sources(self):
        sources = {}
        for inscription in self.inscriptions:
            if inscription.is_variable() and inscription.expr not in sources:
                sources[inscription.expr] = inscription.uid
        return sources

    def get_tokens_number(self):
        return len(self.inscriptions)

    def get_token_inscriptions(self):
        return self.inscriptions

    def is_local(self):
        return True


class EdgeInscription(utils.EqMixin):

    edge = None

    def __init__(self, expr):
        self.expr = expr
        self.uid = utils.get_unique_id()

    def get_type(self):
        return self.edge.get_place_type()

    def is_variable(self):
        return self.edge.transition.net.project.is_expr_variable(self.expr)


class Place(utils.EqByIdMixin):

    code = None

    def __init__(self, net, id, type, init_exprs):
        self.net = net
        self.id = id
        self.type = type
        self.init_exprs = init_exprs
        self.tracing = []

    def get_pos_id(self):
        return self.net.places.index(self)

    def get_edges_in(self, with_interface = False):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_out:
                if edge.place == self:
                    result.append(edge)
        if with_interface:
            for edge in self.net.get_interface_edges_out():
                if edge.place == self:
                    result.append(edge)
        return result

    def get_edges_out(self):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_in:
                if edge.place == self:
                    result.append(edge)
        return result

    def get_transitions_out(self):
        return list(set([ edge.get_transition() for edge in self.get_edges_out() ]))

    def get_transitions_in(self):
        return list(set([ edge.get_transition() for edge in self.get_edges_in() ]))

    def get_areas(self):
        return self.net.get_areas_with_place(self)

    def check(self, checker):
        checker.check_type(self.type, self.get_source("type"))
        source = self.get_source("type")
        for expr in self.init_exprs:
            checker.check_expression(expr, [], self.type, source)

    def get_source(self, location):
        return "*{0}/{1}".format(self.id, location)


class Transition(utils.EqByIdMixin):

    code = None

    def __init__(self, net, id, guard):
        self.net = net
        self.id = id
        self.guard = guard
        self.edges_in = []
        self.edges_out = []
        self.tracing = []
        self.var_exprs = None
        self.match_exprs = None

    def get_token_inscriptions_in(self):
        return sum([ edge.get_token_inscriptions() for edge in self.edges_in ], [])

    def get_token_inscriptions_out(self):
        return sum([ edge.get_token_inscriptions() for edge in self.edges_out ], [])

    def need_trace(self):
        if self.is_any_place_traced():
            return True
        if self.code and self.code.find("ctx.trace_") != -1:
            return True
        return len(self.tracing) > 0

    def get_all_edges(self):
        return self.edges_in + self.edges_out

    def get_input_places(self):
        return set([ edge.place for edge in self.edges_in ])

    def get_output_places(self):
        return set([ edge.place for edge in self.edges_in ])

    def get_places(self):
        return self.get_input_places() & self.get_output_places()

    def is_any_place_traced(self):
        return any(place.tracing for place in self.get_places())

    def get_pos_id(self):
        return self.net.transitions.index(self)

    def is_local(self):
        return all((edge.is_local() for edge in self.edges_out))

    def get_source(self):
        return "*{0}".format(self.id)

    def get_decls_dict(self):
        decls_dict = {}
        from_input = []
        for edge in self.edges_in:
            for name, t in edge.get_decls():
                if name not in decls_dict:
                    decls_dict[name] = t
                    from_input.append(name)
                elif name != decls_dict[name]:
                    raise utils.PtpException(
                        "Inconsistent types for variable '{0}'".format(name))

        for edge in self.edges_out:
            for name, t in edge.get_decls():
                if name not in decls_dict:
                    decls_dict[name] = t
                elif name != decls_dict[name] and name not in from_input:
                    raise utils.PtpException(
                        "Inconsistent types for variable '{0}'".format(name))
        return decls_dict

    def get_decls(self):
        decls = self.get_decls_dict().items()
        decls.sort(key=lambda x: x[1])
        return decls

    def check(self, checker):
        for edge in self.edges_in:
            edge.check(checker)

        for edge in self.edges_out:
            edge.check(checker)


class Area(object):

    def __init__(self, net, id, expr, places):
        self.net = net
        self.id = id
        self.places = places
        self.expr = expr

    def is_place_inside(self, place):
        return place in self.places


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

    def get_all_edges(self):
        return sum([ t.edges_in + t.edges_out for t in self.transitions ], []) + \
               self.interface_edges_in + self.interface_edges_out

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

    def get_index(self):
        return self.project.nets.index(self)

    def get_module_index(self):
        index = 0
        for net in self.project.nets:
            if net.is_module():
                if net == self:
                    return index
                index += 1

    def get_module_input_vars(self):
        return set().union(*[ e.expr.get_free_vars() for e in self.interface_edges_out ])

    def get_module_output_vars(self):
        return set().union(* [ e.get_free_vars() for e in self.interface_edges_in ])

    def get_areas_with_place(self, place):
        return [ area for area in self.areas if area.is_place_inside(place) ]

    def is_local(self):
        return all((tr.is_local() for tr in self.transitions))

    def check(self, checker):
        for place in self.places:
            place.check(checker)

        for transition in self.transitions:
            transition.check(checker)

        """
        for t in self.get_all_types():
            t.check(self.project)
        """

    def analyze(self):
        for tr in self.transitions:
            analysis.analyze_transition(tr)
