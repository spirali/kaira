#
#    Copyright (C) 2011-2014 Stanislav Bohm
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

    def set(self, var, t, source=None, force=False):
        if not force and var in self.types:
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

    size_substitution = None

    def __init__(self, id, transition, place, inscriptions):
        self.id = id
        self.uid = utils.get_unique_id()
        self.place = place
        self.transition = transition
        for inscription in inscriptions:
            inscription.edge = self
        self.inscriptions = inscriptions

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
            and self.uid == other.uid)

    def __ne__(self, other):
        return not self.__eq__(other)

    @property
    def source(self):
        return "*{0}/inscription".format(self.id)

    def check(self, checker):
        bulk = any(inscription.is_bulk() for inscription in self.inscriptions)

        if bulk and len(self.inscriptions) > 1:
            raise utils.PtpException(
                "Bulk inscription cannot be combined with others",
                self.source)

    def check_edge_in(self, checker):
        self.check(checker)

        for inscription in self.inscriptions:
            inscription.check_edge_in(checker)

        if self.size_substitution:
            raise utils.PtpException(
                "Size substition can be used only for output edges",
                self.source)

    def check_edge_out(self, checker):
        self.check(checker)

        for inscription in self.inscriptions:
            inscription.check_edge_out(checker)

        if self.size_substitution:
            decls = self.transition.get_decls()
            decls.set("size", "size_t", self.source)
            decls.set("ctx", "casr::Context", self.source, force=True)
            checker.check_expression(self.size_substitution,
                                     decls,
                                     "size_t",
                                     self.source)

    def get_decls(self):
        decls = Declarations(self.source)
        for inscription in self.inscriptions:
            decls.merge(inscription.get_decls())
        return decls

    def is_bulk(self):
        return self.inscriptions and self.inscriptions[0].is_bulk()

    def is_token(self):
        return not self.is_bulk()

    def is_source_reader(self):
        return any(inscription.is_source_reader() for inscription in self.inscriptions)

    def get_tokens_number(self):
        if self.is_bulk():
            return None
        else:
            count = 0
            for inscription in self.inscriptions:
                if inscription.is_token() and not inscription.is_conditioned():
                    count += 1
            if count > 0:
                return count
            else:
                return None

    def get_token_prefix_size(self):
        """ Returns the number of (first) tokens in the place that can influence the inscription """

        if self.is_bulk():
            return None

        count = 0
        for inscription in self.inscriptions:
            if "filter" in inscription.config or "from" in inscription.config:
                return None
            if inscription.is_token():
                count += 1
        return count

    def get_token_inscriptions(self):
        if self.is_bulk():
           return ()
        else:
           return [ inscription for inscription in self.inscriptions
                    if inscription.has_expr() ]

    def is_local(self):
        return all(inscription.is_local() for inscription in self.inscriptions)

    def has_fixed_target(self):
        return all(i.has_fixed_target() for i in self.inscriptions)


class EdgeInscription(utils.EqMixin):

    edge = None

    def __init__(self, config, expr, target):
        self.uid = utils.get_unique_id()
        self.config = config
        self.expr = expr
        self.target = target

    @property
    def source(self):
        return self.edge.source

    @property
    def type(self):
        if self.is_bulk():
            return get_token_container_type(self.edge.place.type)
        else:
            return self.edge.place.type

    @property
    def target_type(self):
        if self.is_multicast():
            return get_container_type("int")
        else:
            return "int"

    @property
    def index(self):
        return self.edge.inscriptions.index(self)

    def has_expr(self):
        return self.expr is not None

    def is_conditioned(self):
        return "if" in self.config or "root" in self.config

    def is_expr_variable(self):
        return self.edge.transition.net.project.is_expr_variable(self.expr)

    def get_variables(self):
        variables = self.edge.transition.net.project.get_expr_variables(self.expr)
        for name in [ "guard", "filter", "from", "if" ]:
            if self.config.get(name):
                variables.update(self.edge.transition.net.project.get_expr_variables(
                    self.config.get(name)))
        return variables

    def get_foreign_variables(self):
        variables = self.get_variables()
        if self.is_expr_variable():
            variables.remove(self.expr)
        if self.edge.transition.root:
            variables.update(
                self.edge.transition.net.project.get_expr_variables(self.edge.transition.root))
        return variables

    def is_target_variable(self):
        if self.target is not None:
            return self.edge.transition.net.project.is_expr_variable(self.target)
        return False

    def check(self, checker):
        if self.has_expr():
            op = self.get_collective_operation()
            if op == "scatter":
                checker.check_expression(self.expr,
                                         self.edge.transition.get_decls(),
                                         get_container_type(self.type),
                                         self.source)
            elif op == "gather" or op == "allgather":
                t = self.edge.transition.net.project.parse_typename(self.type, self.source)
                if len(t) < 2 or len(t[1]) < 0 or t[0] != "std::vector":
                    raise utils.PtpException("Invalid type of expression", self.source)
                typename = t[1][0]
                checker.check_expression(self.expr,
                                         self.edge.transition.get_decls(),
                                         typename,
                                         self.source)
            else:
                checker.check_expression(self.expr,
                                         self.edge.transition.get_decls(),
                                         self.type,
                                         self.source)

    def check_config(self, valid_keys):
        invalid_key = utils.key_not_in_list(self.config, valid_keys)
        if invalid_key is not None:
            raise utils.PtpException(
                "Invalid config item '{0}'".format(invalid_key),
                self.source)

    def check_config_with_expression(self, name, variable=False):
        if name in self.config:
            if self.config[name] is None:
                raise utils.PtpException(
                    "'{0}' requires an expression".format(name),
                    self.source)
            if variable and \
                not self.edge.transition.net.project.is_expr_variable(self.config[name]):
                raise utils.PtpException(
                    "'{0}' requires an variable".format(name))
            return True
        else:
            return False

    def get_svar_type(self):
        t = "int"
        if self.is_bulk():
            t = get_container_type(t)
        return t

    def check_edge_in(self, checker):
        if self.target is not None:
            raise utils.PtpException("Input edges cannot contain '@'",
                self.source)

        allowed = ["bulk", "guard", "svar", "filter", "from", "if", "sort_by_source"]
        if self.edge.transition.collective and self.edge.transition.root:
            allowed.append("root")

        self.check_config(allowed)

        if self.check_config_with_expression("svar", variable=True):
            decls = self.edge.transition.get_input_decls()
            checker.check_expression(self.config["svar"],
                                     decls,
                                     self.get_svar_type(),
                                     self.source)

        if self.check_config_with_expression("filter"):
            decls = self.edge.transition.get_input_decls()
            checker.check_expression(self.config["filter"],
                                     decls,
                                     "bool",
                                     self.source)

        if self.check_config_with_expression("from"):
            decls = self.edge.transition.get_input_decls()
            checker.check_expression(self.config["from"],
                                     decls,
                                     "int",
                                     self.source)

        if self.check_config_with_expression("guard"):
            decls = self.edge.transition.get_input_decls_with_size(self.source)
            checker.check_expression(self.config["guard"],
                                     decls,
                                     "bool",
                                     self.source)

        if self.check_config_with_expression("if"):
            decls = self.edge.transition.get_input_decls_with_size(self.source)
            checker.check_expression(self.config["if"],
                                     decls,
                                     "bool",
                                     self.source)
        self.check(checker)

        if "sort_by_source" in self.config and "bulk" not in self.config:
            raise utils.PtpException("Configuration option 'sort_by_source' "
                                     "can be used only with 'bulk'")

    def check_edge_out(self, checker):
        if not self.expr:
            raise utils.PtpException("Main expression of output inscription is empty",
                                     self.source)

        if self.edge.transition.collective and self.target is not None:
            raise utils.PtpException("Output edges of collective transitions cannot contain '@'",
                self.source)

        allowed = [ "bulk", "multicast", "if", "seq" ]

        if self.edge.transition.collective:
            if self.edge.transition.root:
                allowed.append("root")
            allowed += [ "scatter", "gather", "allgather", "bcast" ]
        self.check_config(allowed)

        if self.check_config_with_expression("if"):
            decls = self.edge.transition.get_input_decls_with_size(self.source)
            checker.check_expression(self.config["if"],
                                     decls,
                                     "bool",
                                     self.source)

        if self.check_config_with_expression("seq"):
            if not utils.is_integer(self.config["seq"]):
                raise utils.PtpException("Parameter of 'seq' has to be a constant integer",
                                         self.source)
            self.config["seq"] = int(self.config["seq"])

        if self.target is not None:
            checker.check_expression(self.target,
                                     self.edge.transition.get_decls(),
                                     self.target_type,
                                     self.source)
        self.check(checker)

    def get_other_variables(self):
        if self.is_target_variable():
            return [ self.target ]
        else:
            return []

    def get_decls(self):
        decls = Declarations(self.source)
        if self.is_expr_variable() and not self.is_collective():
            decls.set(self.expr, self.type)
        if self.config.get("svar"):
            decls.set(self.config["svar"], self.get_svar_type())
        if self.is_target_variable():
            decls.set(self.target, self.target_type)
        return decls

    def is_local(self):
        return self.target is None

    def is_bulk(self):
        return "bulk" in self.config

    def is_token(self):
        return not self.is_bulk() and self.has_expr()

    def has_fixed_target(self):
        if self.is_multicast():
            return False
        v = self.edge.transition.net.project.get_expr_variables(self.target)
        return v == set() or v == set(["ctx"])

    def is_multicast(self):
        return "multicast" in self.config

    def is_unicast(self):
        return not self.is_multicast()

    def is_source_reader(self):
        return "svar" in self.config or \
               "from" in self.config or \
               "sort_by_source" in self.config

    def has_same_pick_rule(self, inscription):
        return (inscription.config.get("filter") == self.config.get("filter") and
                inscription.config.get("from") == self.config.get("from"))

    def get_collective_operation(self):
        if "scatter" in self.config:
            return "scatter"
        if "gather" in self.config:
            return "gather"
        if "allgather" in self.config:
            return "allgather"
        if "bcast" in self.config:
            return "bcast"
        return None

    def is_collective(self):
        return self.get_collective_operation() is not None

    def is_root_only(self):
        return "root" in self.config


class Place(utils.EqByIdMixin):

    code = None
    interface_input = None
    interface_output = None

    # Verification
    final_marking = False

    def __init__(self, net, id, type, init_type, init_value):
        self.net = net
        self.id = id
        self.type = type
        self.init_type = init_type
        self.init_value = init_value
        self.trace_tokens = False
        self.trace_tokens_functions = []

    def get_pos_id(self):
        return self.net.places.index(self)

    def get_edges_in(self):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_out:
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

    def get_inscriptions_in(self):
        return sum((edge.inscriptions for edge in self.get_edges_in()), [])

    def get_transitions_out(self):
        return list(set([ edge.transition for edge in self.get_edges_out() ]))

    def get_transitions_in(self):
        return list(set([ edge.transition for edge in self.get_edges_in() ]))

    def get_areas(self):
        return self.net.get_areas_with_place(self)

    def is_io_place(self):
        return self.interface_input is None or self.interface_output is None

    def get_token_prefix_size(self):
        sizes = [ edge.get_token_prefix_size() for edge in self.get_edges_out() ]
        if not sizes:
            return 0
        if any(s is None for s in sizes):
            return None
        return max(sizes)

    def check(self, checker):
        functions = [ "token_name" ]
        if self.is_receiver() or self.is_collective_receiver() or \
                (self.net.project.library_rpc and self.is_io_place()):
            functions.append("pack")
            functions.append("unpack")
        if self.net.project.library_octave:
            if self.interface_input:
                functions.append("from_octave_value")
            if self.interface_output:
                functions.append("to_octave_value")
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

        if self.trace_tokens:
            for name, return_type in self.trace_tokens_functions:
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
        return any(not edge.is_local() for edge in self.get_edges_in())

    def is_collective_receiver(self):
        return any(i.is_collective() for i in self.get_inscriptions_in())

    def need_remember_source(self):
        return any(edge.is_source_reader()
                   for edge in self.get_edges_out())


class Transition(utils.EqByIdMixin):

    code = None
    collective = False
    root = ""

    # Time substitution
    time_substitution = None
    clock_substitution = None

    # Verification
    calls_quit = False
    occurrence_analysis = False
    occurrence_analysis_compare_process = False
    occurrence_analysis_compare_binding = False

    def __init__(self, net, id, name, guard):
        self.net = net
        self.id = id
        self.name = name
        self.guard = guard
        self.edges_in = []
        self.edges_out = []
        self.trace_fire = False
        self.var_exprs = None
        self.match_exprs = None
        self.clock = False

        # Filled by function "analyze_transition"
        self.inscriptions_in = None
        self.inscriptions_out = None
        self.variable_sources = None
        self.reuse_tokens = None
        self.variable_sources_out = None
        self.fresh_tokens = None
        self.bulk_overtake = None

    def has_code(self):
        return self.code is not None

    def get_token_inscriptions_in(self):
        return [ inscription for inscription in self.inscriptions_in
                 if inscription.is_token() ]

    def get_token_inscriptions_out(self):
        return [ inscription for inscription in self.inscriptions_out
                 if inscription.is_token() ]

    def get_bulk_edges_in(self):
        return [ edge for edge in self.edges_in if edge.is_bulk() ]

    def get_bulk_edges_out(self):
        return [ edge for edge in self.edges_out if edge.is_bulk() ]

    def need_trace(self):
        if self.is_any_place_traced():
            return True
        return self.trace_fire

    def get_all_edges(self):
        return self.edges_in + self.edges_out

    def get_input_places(self):
        return set([ edge.place for edge in self.edges_in ])

    def get_output_places(self):
        return set([ edge.place for edge in self.edges_out ])

    def get_places(self):
        return self.get_input_places() | self.get_output_places()

    def is_any_place_traced(self):
        return any(place.trace_tokens for place in self.get_places())

    def get_pos_id(self):
        return self.net.transitions.index(self)

    def is_local(self):
        return all((edge.is_local() for edge in self.edges_out))

    def has_fixed_target(self):
        return all((edge.has_fixed_target() for edge in self.edges_out))

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

        if self.collective:
            inscriptions = [ inscription for edge in self.edges_out for inscription in edge.inscriptions
                                         if inscription.is_collective() ]
            if len(inscriptions) > 1:
                raise utils.PtpException("At most one collective inscription may be defined",
                                         inscriptions[-1].source)
            if inscriptions:
                op = inscriptions[0].get_collective_operation()
                need_root = op == "scatter" or op == "gather" or op == "bcast"
                if not self.root and need_root:
                    raise utils.PtpException("Root not defined", self.get_source())
            else:
                need_root = False

            if not need_root and self.root:
                raise utils.PtpException("Root cannot be defined for this operation",
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
            decls = self.get_decls()
            decls.set("ctx", "casr::Context", force=True)
            decls.set("transitionTime", "ca::IntTime", self.get_source(), force=True)
            checker.check_expression(self.time_substitution,
                                     decls,
                                     "ca::IntTime",
                                     self.get_source())
        if self.clock_substitution:
            decls = self.get_decls()
            decls.set("ctx", "casr::Context", force=True)
            decls.set("clockTime", "ca::IntTime", self.get_source(), force=True)
            checker.check_expression(self.clock_substitution,
                                     decls,
                                     "ca::IntTime",
                                     self.get_source())

    def get_collective_inscription(self):
        for inscription in self.inscriptions_out:
            if inscription.is_collective():
                return inscription
        return None

    def get_collective_operation(self):
        if not self.collective:
            return None
        inscription = self.get_collective_inscription()
        if inscription is None:
            return "barrier"
        else:
            return inscription.get_collective_operation()

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

    def get_name(self):
        return self.name

    def get_all_edges(self):
        return sum([ t.edges_in + t.edges_out for t in self.transitions ], [])

    def get_edges_out(self):
        return sum([ t.edges_out for t in self.transitions ], [])

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

    def get_input_places(self):
        places = [ place for place in self.places
                   if place.interface_input ]
        places.sort(key=lambda place: place.interface_input)
        return places

    def get_output_places(self):
        places = [ place for place in self.places
                   if place.interface_output ]
        places.sort(key=lambda place: place.interface_output)
        return places

    def get_areas_with_place(self, place):
        return [ area for area in self.areas if area.is_place_inside(place) ]

    def check(self, checker):
        for place in self.places:
            place.check(checker)

        for transition in self.transitions:
            transition.check(checker)

        for area in self.areas:
            area.check(checker)

        place = utils.check_uniquness(self.get_input_places(),
                                      lambda p: p.interface_input)
        if place is not None:
            raise utils.PtpException("Input iterfaces have not unique names",
                                     place.get_source("type"))

        place = utils.check_uniquness(self.get_output_places(),
                                      lambda p: p.interface_output)
        if place is not None:
            raise utils.PtpException("Output iterfaces have not unique names",
                                     place.get_source("type"))

    def analyze(self):
        for tr in self.transitions:
            analysis.analyze_transition(tr)
