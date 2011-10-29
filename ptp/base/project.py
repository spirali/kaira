
import base.utils as utils
import base.parser as parser
from base.expressions import nel_true, Env
from base.neltypes import t_bool, derive_context, t_array, t_int

import xml.etree.ElementTree as xml

class Edge(utils.EqMixin):

    def __init__(self, id, expr, from_element, to_element, target = None):
        self.id = id
        self.expr = expr
        self.from_element = from_element
        self.to_element = to_element
        self.target = target

    def get_place(self):
        if isinstance(self.from_element, Place):
            return self.from_element
        else:
            return self.to_element

    def get_place_type(self):
        return self.get_place().type

    def get_exprs_and_types(self):
        return [ (self.expr, self.get_place().type) ]

    def get_equations(self):
        eq = [ (self.expr, self.get_place_type()) ]
        if self.target:
            eq.append((self.target, t_int))
        return eq

    def inject_types(self, env, context):
        self.expr.inject_types(env, context)
        if self.target:
            self.expr.inject_types(env, context)

    def __repr__(self):
        return "Edge({0}, {1}, {2}, {3})".format(self.id, repr(self.expr), repr(self.from_element), repr(self.to_element))

class Place(utils.EqMixin):

    code = None

    def __init__(self, net, id, type, init_expression):
        self.net = net
        self.id = id
        self.type = type
        self.init_expression = init_expression

    def inject_types(self):
        if self.init_expression is not None:
            inject_types_for_empty_context(self.net.project.get_env(), self.init_expression, t_array(self.type))

    def get_pos_id(self):
        return self.net.places.index(self)

    def get_edges_in(self):
        result = []
        for tr in self.net.transitions:
            for edge in tr.edges_out:
                if edge.get_place() == self:
                    result.append(edge)
        return result

    def get_areas(self):
        return self.net.get_areas_with_place(self)

class Transition(utils.EqMixin):

    code = None
    subnet = None

    def __init__(self, net, id, guard):
        self.net = net
        self.id = id
        self.guard = guard
        self.edges_in = []
        self.edges_out = []

    def get_context(self):
        return derive_context(self.net.project.get_env(), self.get_exprs_and_types())

    def get_all_edges(self):
        return self.edges_in + self.edges_out

    def get_basic_input_edges(self):
        return self.edges_in

    def get_exprs_and_types(self):
        result = []
        for e in self.get_all_edges():
            result += e.get_exprs_and_types()
        result.append((self.guard, t_bool))
        return result

    def get_types(self):
        return set([ t for _, t in self.get_exprs_and_types() ])

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

    id = None

    def __init__(self, project):
        self.project = project
        self.places = []
        self.transitions = []
        self.areas = []

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
        for tr in self.transitions:
            for t in tr.get_types():
                result.update(t.get_subtypes())
        return result

    def get_index(self):
        return self.project.nets.index(self)

    def inject_types(self):
        for place in self.places:
            place.inject_types()
        for tr in self.transitions:
            tr.inject_types()
        for area in self.areas:
            area.inject_types()

    def get_areas_with_place(self, place):
        return [ area for area in self.areas if area.is_place_inside(place) ]

class Project(object):

    def __init__(self, description):
        self.nets = []
        self.description = description

    def get_env(self):
        return Env()

    def get_all_types(self):
        return set().union( *[ net.get_all_types() for net in self.nets ] )

    def get_net(self, id):
        for net in self.nets:
            if net.id == id:
                return net

    def get_place(self, place_id):
        for net in self.nets:
            place = net.get_place(place_id)
            if place:
                return place

    def inject_types(self):
        for net in self.nets:
            net.inject_types()

def load_edge_in(element, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    expr = parser.parse_expression(utils.xml_str(element, "expr"))
    return Edge(id, expr, net.get_place(place_id), transition)

def load_edge_out(element, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    expr, target = parser.parse_output_inscription(utils.xml_str(element, "expr"))
    return Edge(id, expr, transition, net.get_place(place_id), target)

def load_transition(element, project, net):
    id = utils.xml_int(element, "id")

    if utils.xml_str(element, "guard").strip() == "":
        guard = nel_true
    else:
        guard = parser.parse_expression(utils.xml_str(element, "guard"))
    transition = Transition(net, id, guard)
    transition.edges_in = order_input_edges(map(lambda e: load_edge_in(e, net, transition), element.findall("edge-in")))
    transition.edges_out = map(lambda e: load_edge_out(e, net, transition), element.findall("edge-out"))

    subnet_id = utils.xml_int(element, "subnet", -1)
    if subnet_id > 0:
        transition.subnet = project.get_net(subnet_id)

    if element.find("code") is not None:
        transition.code = element.find("code").text
    return transition

def load_place(element, net):
    id = utils.xml_int(element, "id")
    type = parser.parse_type(utils.xml_str(element, "type"))
    init_expr = parser.parse_expression_or_empty(utils.xml_str(element, "init-expr"))

    place = Place(net, id, type, init_expr)
    if element.find("code") is not None:
        place.code = element.find("code").text
    return place

def load_area(element, net):
    id = utils.xml_int(element, "id")
    expr = parser.parse_expression(utils.xml_str(element, "init-expr"))
    places = [ net.get_place(utils.xml_int(e, "id")) for e in element.findall("place") ]
    return Area(net, id, expr, places)

def load_net(element, project):
    net = Net(project)
    net.id = utils.xml_int(element, "id")
    return net

def load_net_content(element, project, net):
    net.places = [ load_place(e, net) for e in element.findall("place") ]
    net.transitions = [ load_transition(e, project, net) for e in element.findall("transition") ]
    net.areas = [ load_area(e, net) for e in element.findall("area") ]

def load_project(element):
    description = element.find("description").text
    p = Project(description)

    nets = [ (e, load_net(e, p)) for e in element.findall("net") ]
    p.nets = [ net for e, net in nets ]
    for e, net in nets:
        load_net_content(e, p, net)
    return p

def load_project_from_file(filename):
    doc = xml.parse(filename)
    return load_project(doc.getroot())

def order_input_edges(edges):
    def depends_on(x, y):
        return not y.expr.get_undirect_vars().isdisjoint(x.expr.get_direct_vars())
    ordered = utils.topological_ordering(edges, depends_on)
    if ordered is None:
        raise Exception("Edges cannot be ordered")
    covered = set()
    for edge in ordered:
        if not edge.expr.get_undirect_vars().issubset(covered):
            raise Exception("Edges cannot be ordered")
        covered.update(edge.expr.get_direct_vars())
    return ordered
