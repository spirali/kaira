
import base.utils as utils
import base.parser as parser
from base.expressions import nel_true, Env, ExprCall

import xml.etree.ElementTree as xml
from base.utils import PtpException
from net import Net, Area, Place, Transition, EdgeIn, EdgeInPacking, EdgeOut

class ExternType(object):

    def __init__(self, name, rawtype, transport_mode, codes):
        self.name = name
        self.rawtype = rawtype
        self.transport_mode = transport_mode
        self.codes = codes

    def get_name(self):
        return self.name

    def get_rawtype(self):
        return self.rawtype

    def get_transport_mode(self):
        return self.transport_mode

    def get_code(self, name):
        return self.codes.get(name)

    def has_code(self, name):
        return name in self.codes

class Project(object):

    def __init__(self, description):
        self.nets = []
        self.description = description
        self.extern_types = {}

    def get_env(self):
        return Env()

    def get_all_types(self):
        return set().union( *[ net.get_all_types() for net in self.nets ] )

    def get_extern_types(self):
        return self.extern_types.values()

    def get_extern_type(self, name):
        return self.extern_types.get(name)

    def get_net(self, id):
        for net in self.nets:
            if net.id == id:
                return net

    def get_place(self, place_id):
        for net in self.nets:
            place = net.get_place(place_id)
            if place:
                return place

    def get_transition(self, transition_id):
        for net in self.nets:
            tr = net.get_transition(transition_id)
            if tr:
                return tr

    def inject_types(self):
        for net in self.nets:
            net.inject_types()

def get_source(element, name):
    id = utils.xml_int(element, "id")
    return "*{0}/{1}".format(id, name)

def load_edge_in(element, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    source = get_source(element, "inscription")
    mode, expr = parser.parse_input_inscription(utils.xml_str(element, "expr"), source)
    if mode == 'normal':
        return EdgeIn(id, net.get_place(place_id), transition, expr)
    else:
        if not isinstance(expr, ExprCall) or len(expr.args) != 1:
            raise PtpException("Invalid syntax for input packing expression", source)
        return EdgeInPacking(id, net.get_place(place_id), transition, expr.name, expr.args[0])

def load_edge_out(element, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    mode, expr, target = parser.parse_output_inscription(utils.xml_str(element, "expr"), get_source(element, "inscription"))
    return EdgeOut(id, net.get_place(place_id), transition, expr, mode, target)

def load_transition(element, project, net):
    id = utils.xml_int(element, "id")

    if utils.xml_str(element, "guard").strip() == "":
        guard = nel_true
    else:
        guard = parser.parse_expression(utils.xml_str(element, "guard"))
    transition = Transition(net, id, guard)
    transition.edges_in = map(lambda e: load_edge_in(e, net, transition), element.findall("edge-in"))
    transition.edges_out = map(lambda e: load_edge_out(e, net, transition), element.findall("edge-out"))

    subnet_id = utils.xml_int(element, "subnet", -1)
    if subnet_id > 0:
        transition.subnet = project.get_net(subnet_id)

    if element.find("code") is not None:
        transition.code = element.find("code").text
    return transition

def load_place(element, net):
    id = utils.xml_int(element, "id")
    type = parser.parse_type(utils.xml_str(element, "type"), get_source(element, "type"))
    init_expr = parser.parse_expression_or_empty(utils.xml_str(element, "init-expr"), get_source(element, "init"))

    place = Place(net, id, type, init_expr)
    if element.find("code") is not None:
        place.code = element.find("code").text
    return place

def load_area(element, net):
    id = utils.xml_int(element, "id")
    expr = parser.parse_expression(utils.xml_str(element, "init-expr"), get_source(element, "instances"))
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

def load_extern_type(element):
    name = utils.xml_str(element, "name")
    rawtype = utils.xml_str(element, "raw-type")
    transport_mode = utils.xml_str(element, "transport-mode")
    codes = dict((utils.xml_str(e, "name"), e.text) for e in element.findall("code"))
    return ExternType(name, rawtype, transport_mode, codes)

def load_configuration(element, project):
    etypes = [ load_extern_type(e) for e in element.findall("extern-type") ]
    project.extern_types = utils.create_dict(etypes, lambda etype: etype.name)

def load_project(element):
    description = element.find("description").text
    p = Project(description)

    load_configuration(element.find("configuration"), p)

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
