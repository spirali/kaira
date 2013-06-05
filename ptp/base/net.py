#
#    Copyright (C) 2011-2013 Stanislav Bohm
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

def get_container_type(typename):
    return "std::vector<{0} >".format(typename)

def get_token_container_type(typename):
    return "ca::TokenList<{0} >".format(typename)


class Declarations:

    def __init__(self, source=None):
        self.types = {}
        self.source = source

    def set(self, var, t, source=None):
        if var in self.types:
            if self.types[var] != t:
                if source is None:
                    source = self.source
                msg = "Invalid type of variable '{0}' ({1}/{2})".format(var, self.types[var], t)
                raise utils.PtpException(msg, source)
        else:
            self.types[var] = t

    def get_variables(self):
        return self.types.keys()

    def get_types(self):
        return self.types.values()

    def get_items(self):
        return self.types.items()

    def merge(self, decls):
        if self.source is None:
            self.source = decls.source
        for var, t in decls.get_items():
            self.set(var, t, decls.source)

    def get_list(self):
        lst = self.types.items()
        lst.sort(key=utils.first)
        return lst

    def __getitem__(self, variable):
        return self.types[variable]


class Edge(utils.EqMixin):

    def __init__(self, id, transition, place, inscriptions, config, target):
        self.id = id
        self.uid = utils.get_unique_id()
        self.place = place
        self.transition = transition
        for inscription in inscriptions:
            inscription.edge = self
        self.inscriptions = inscriptions
        self.config = config
        self.target = target

    def get_source(self):
        return "*{0}/inscription".format(self.id)

    def get_place_type(self):
        return self.place.type

    def get_type(self):
        if self.is_bulk_edge():
            return get_token_container_type(self.place.type)
        else:
            return self.place.type

    def check(self, checker):
        if self.is_bulk_edge():
            checker.check_expression(self.inscriptions[0].expr,
                                     self.transition.get_decls(),
                                     self.get_type(),
                                     self.get_source())
        else:
            for inscription in self.inscriptions:
                checker.check_expression(inscription.expr,
                                         self.transition.get_decls(),
                                         self.get_place_type(),
                                         self.get_source())

    def check_config(self, valid_keys):
        invalid_key = utils.key_not_in_list(self.config, valid_keys)
        if invalid_key is not None:
            raise utils.PtpException("Invalid config item '{0}'".format(invalid_key),
                self.get_source())

    def check_edge_in(self, checker):
        if self.target is not None:
            raise utils.PtpException("Input edges cannot contain '@'",
                self.get_source())

        self.check_config(("bulk", "guard", "origin"))
        if "bulk" in self.config:
            if len(self.inscriptions) != 1 or not self.inscriptions[0].is_variable():
                raise utils.PtpException(
                    "'bulk' requires a single variable as main expression",
                    self.get_source())
        if "guard" in self.config:
            if self.config["guard"] is None:
                raise utils.PtpException(
                    "'guard' requires an expression",
                    self.get_source())
            decls = self.transition.get_input_decls_with_size(self.get_source())
            checker.check_expression(self.config["guard"],
                                     decls,
                                     "bool",
                                     self.get_source())
        self.check(checker)

    def check_edge_out(self, checker):
        self.check_config(("bulk", "multicast", "if"))
        if "bulk" in self.config:
            if len(self.inscriptions) != 1 or not self.inscriptions[0].is_variable():
                raise utils.PtpException(
                    "'bulk' requires a single variable as main expression",
                    self.get_source())

        if "if" in self.config:
            if self.config["if"] is None:
                raise utils.PtpException(
                    "'if' requires an expression",
                    self.get_source())
            decls = self.transition.get_input_decls_with_size(self.get_source())
            checker.check_expression(self.config["if"],
                                     decls,
                                     "bool",
                                     self.get_source())
        if self.target is not None:
            checker.check_expression(self.target,
                                     self.transition.get_decls(),
                                     self.get_target_type(),
                                     self.get_source())
        self.check(checker)

    def get_target_type(self):
        if self.is_multicast():
            return get_container_type("int")
        else:
            return "int"

    def get_decls(self):
        decls = Declarations(self.get_source())
        if self.is_bulk_edge():
            decls.set(self.inscriptions[0].expr, self.get_type())
            if self.is_origin_reader():
                decls.set(self.inscriptions[0].expr + "_origins",
                          get_container_type("int"))
        else:
            for variable in self.get_single_token_variables():
                decls.set(variable, self.get_place_type())
            if self.is_origin_reader():
                for variable in self.get_single_token_variables():
                    decls.set(variable + "_origin", "int")

        if self.target and self.transition.net.project.is_expr_variable(self.target):
            decls.set(self.target, self.get_target_type())
        return decls

    def get_single_token_variables(self):
        if self.is_bulk_edge():
            return []
        return [ inscription.expr
                 for inscription in self.inscriptions
                 if inscription.is_variable() ]

    def get_nontoken_variables(self):
        if self.target and self.transition.net.project.is_expr_variable(self.target):
            return [ self.target ]
        else:
            return []

    def get_variable_sources(self):
        sources = {}
        if self.is_token_edge():
            for inscription in self.inscriptions:
                if inscription.is_variable() and inscription.expr not in sources:
                    sources[inscription.expr] = inscription.uid
        else:
            sources[self.inscriptions[0].expr] = None
        return sources

    def get_tokens_number(self):
        if self.is_token_edge():
            return len(self.inscriptions)

    def get_token_inscriptions(self):
        if self.is_token_edge():
            return self.inscriptions
        else:
            return []

    def is_local(self):
        return self.target is None

    def is_bulk_edge(self):
        return "bulk" in self.config

    def is_token_edge(self):
        return not self.is_bulk_edge()

    def is_unicast(self):
        return not self.is_multicast()

    def is_origin_reader(self):
        return "origin" in self.config

    def is_multicast(self):
        return "multicast" in self.config


class EdgeInscription(utils.EqMixin):

    edge = None

    def __init__(self, expr):
        self.expr = expr
        self.uid = utils.get_unique_id()

    def get_type(self):
        return self.edge.get_place_type()

    def is_variable(self):
        return (self.edge.transition.net.project.is_expr_variable(self.expr) and
                not self.expr.endswith("_origin") and
                not self.expr.endswith("_origins"))

    def get_place(self):
        return self.edge.place

    def is_origin_reader(self):
        return self.edge.is_origin_reader() and self.is_variable()


class Place(utils.EqByIdMixin):

    code = None

    def __init__(self, net, id, type, init_type, init_value):
        self.net = net
        self.id = id
        self.type = type
        self.init_type = init_type
        self.init_value = init_value
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
        return list(set([ edge.transition for edge in self.get_edges_out() ]))

    def get_transitions_in(self):
        return list(set([ edge.transition for edge in self.get_edges_in() ]))

    def get_areas(self):
        return self.net.get_areas_with_place(self)

    def check(self, checker):
        functions = [ "token_name" ]
        if self.is_receiver():
            functions.append("pack")
            functions.append("unpack")
        checker.check_type(self.type, self.get_source("type"), functions)

        source = self.get_source("init")
        decls = self.net.project.get_minimal_decls()
        if self.init_type == "exprs":
            for expr in self.init_value:
                checker.check_expression(expr, decls, self.type, source)
        elif self.init_type == "vector":
            checker.check_expression(self.init_value,
                                     decls,
                                     get_container_type(self.type),
                                     source)

        for name, return_type in self.tracing:
            decls = Declarations(source)
            decls.set("a", self.type)
            checker.check_expression("{0}(a)".format(name),
                                     decls,
                                     return_type,
                                     self.get_source("type"),
                                     "Invalid trace function '{0}'".format(name))

    def get_source(self, location):
        return "*{0}/{1}".format(self.id, location)

    def is_receiver(self):
        edges = self.get_edges_in(with_interface=True)
        return any(not edge.is_local() for edge in edges)

    def need_origin(self):
        return any(edge.is_origin_reader()
                   for edge in self.get_edges_out())


class Transition(utils.EqByIdMixin):

    code = None
    time_substitution = None

    def __init__(self, net, id, name, guard):
        self.net = net
        self.id = id
        self.name = name
        self.guard = guard
        self.edges_in = []
        self.edges_out = []
        self.tracing = []
        self.var_exprs = None
        self.match_exprs = None

    def has_code(self):
        return self.code is not None

    def get_token_inscriptions_in(self):
        return sum([ edge.get_token_inscriptions() for edge in self.edges_in ], [])

    def get_token_inscriptions_out(self):
        return sum([ edge.get_token_inscriptions() for edge in self.edges_out ], [])

    def get_bulk_edges_in(self):
        return [ edge for edge in self.edges_in if edge.is_bulk_edge() ]

    def get_bulk_edges_out(self):
        return [ edge for edge in self.edges_out if edge.is_bulk_edge() ]

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
        return set([ edge.place for edge in self.edges_out ])

    def get_places(self):
        return self.get_input_places() | self.get_output_places()

    def is_any_place_traced(self):
        return any(place.tracing for place in self.get_places())

    def get_pos_id(self):
        return self.net.transitions.index(self)

    def is_local(self):
        return all((edge.is_local() for edge in self.edges_out))

    def get_source(self, location=None):
        if location is None:
            # FIXME: To just show error somewhere, we use guard
            location = "guard"
        return "*{0}/{1}".format(self.id, location)

    def get_decls(self):
        decls = self.net.project.get_minimal_decls()
        for edge in self.edges_in:
            decls.merge(edge.get_decls())
        for edge in self.edges_out:
            decls.merge(edge.get_decls())
        return decls

    def get_input_decls(self):
        # FIXME: Return only variables on input edges
        return self.get_decls()

    def get_input_decls_with_size(self, source=None):
        decls = self.get_input_decls()
        decls.set("size", "size_t", source)
        return decls

    def check(self, checker):

        for place, number in utils.multiset([ edge.place for edge in self.edges_in ]).items():
            if number != 1:
                raise utils.PtpException("There can be at most one input edge "
                                         "between place and transition",
                                         self.get_source())

        for edge in self.edges_in:
            edge.check_edge_in(checker)

        for edge in self.edges_out:
            edge.check_edge_out(checker)

        if self.guard:
            checker.check_expression(self.guard,
                                     self.get_input_decls(),
                                     "bool",
                                     self.get_source("guard"))
        if self.time_substitution:
            decls = self.net.project.get_minimal_decls()
            decls.set("transitionTime", "ca::IntTime", self.get_source())
            checker.check_expression(self.time_substitution,
                                     decls,
                                     "ca::IntTime",
                                     self.get_source())


class Area(object):

    def __init__(self, net, id, init_type, init_value, places):
        self.net = net
        self.id = id
        self.places = places
        self.init_type = init_type
        self.init_value = init_value

    def is_place_inside(self, place):
        return place in self.places

    def check(self, checker):
        source = self.get_source("init")
        decls = self.net.project.get_minimal_decls()
        if self.init_type == "exprs":
            for expr in self.init_value:
                checker.check_expression(expr, decls, "int", source)
        elif self.init_type == "vector":
            checker.check_expression(self.init_value,
                                     decls,
                                     get_container_type("int"),
                                     source)
    def get_source(self, location):
        return "*{0}/{1}".format(self.id, location)


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

    def get_edges_out(self):
        return sum([ t.edges_out for t in self.transitions ], []) + self.interface_edges_out

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

        for area in self.areas:
            area.check(checker)

    def analyze(self):
        for tr in self.transitions:
            analysis.analyze_transition(tr)
