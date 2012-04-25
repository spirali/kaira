#
#    Copyright (C) 2011, 2012 Stanislav Bohm
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

import base.utils as utils
import base.parser as parser
from base.expressions import Env, ExprCall, ExprVar, ExprInt

import xml.etree.ElementTree as xml
from base.utils import PtpException
from net import Net, Area, Place, Transition, EdgeIn, EdgeInPacking, EdgeOut

class ExternType(object):

    def __init__(self, name, rawtype, transport_mode, codes, transferable_to_octave):
        self.name = name
        self.rawtype = rawtype
        self.transport_mode = transport_mode
        self.codes = codes
        self.transferable_to_octave =  transferable_to_octave

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

    def is_transferable_to_octave(self):
        return self.transferable_to_octave

class UserFunction(object):

    """
        @param name str
        @param parameters list of tuples (String, Type)
        @param returntype Type
        @param with_context bool
        @param code str
    """
    def __init__(self, id, name, parameters, returntype, with_context, code):
        self.id = id
        self.name = name
        self.parameters = parameters
        self.returntype = returntype
        self.with_context = with_context
        self.code = code

    def get_name(self):
        return self.name

    def get_code(self):
        return self.code

    def get_returntype(self):
        return self.returntype

    def get_parameters(self):
        return self.parameters

    def get_all_types(self):
        ts = [ t for _, t in self.parameters ]
        ts.append(self.returntype)
        return set(ts)

    def check(self, project):
        for t in self.get_all_types():
            t.check(project)

class Parameter(object):

    def __init__(self, name, type, description):
        self.name = name
        self.type = type
        self.description = description

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

    def get_description(self):
        return self.description


class Project(object):

    def __init__(self, name, extenv, target_mode, description):
        self.name = name
        self.extenv = extenv
        self.target_mode = target_mode
        self.nets = []
        self.description = description
        self.extern_types = {}
        self.user_functions = {}
        self.parameters = {}
        self.build_options = {}

    def get_name(self):
        return self.name

    def get_target_mode(self):
        return self.target_mode

    def get_modules(self):
        return [ net for net in self.nets if net.is_module() ]

    def get_env(self):
        env = Env()
        for ufunction in self.get_user_functions():
            params = [ t for _, t in ufunction.get_parameters() ]
            env.add_function(ufunction.get_name(), ufunction.get_returntype(), params)
        for param in self.get_parameters():
            env.add_parameter(param.get_name(), param.get_type())
        return env

    def get_build_option(self, name):
        value = self.build_options.get(name)
        if value is None:
            return ""
        else:
            return value

    def get_extenv(self):
        return self.extenv

    def get_all_types(self):
        ts = set().union( *[ net.get_all_types() for net in self.nets ] )
        ts.update(*[ ufunction.get_all_types() for ufunction in self.get_user_functions() ])
        return ts

    def get_extern_types(self):
        return self.extern_types.values()

    def get_extern_type(self, name):
        return self.extern_types.get(name)

    def get_user_functions(self):
        return self.user_functions.values()

    def get_user_function(self, name):
        return self.user_functions.get(name)

    def get_parameter(self, name):
        return self.parameters.get(name)

    def get_parameters(self):
        return self.parameters.values()

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

    def check(self):
        for net in self.nets:
            net.check()
        for t in self.get_all_types():
            t.check(self)

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
        if isinstance(expr, ExprCall) and len(expr.args) == 1:
            limit = expr.args[0]
        elif isinstance(expr, ExprVar):
            limit = ExprInt(0)
            limit.set_source(source)
        else:
            raise PtpException("Invalid syntax for input packing expression", source)
        return EdgeInPacking(id, net.get_place(place_id), transition, expr.name, limit)

def load_edge_out(element, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    mode, expr, send, guard = parser.parse_output_inscription(utils.xml_str(element, "expr"), get_source(element, "inscription"))
    sendmode, target = send
    return EdgeOut(id, net.get_place(place_id), transition, expr, mode, sendmode, target, guard)

def load_transition(element, project, net):
    id = utils.xml_int(element, "id")

    guard = parser.parse_expression_or_empty(utils.xml_str(element, "guard"), get_source(element, "guard"))
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
    net = Net(project, utils.xml_int(element, "id"), utils.xml_str(element, "name"))
    net.autohalt = utils.xml_bool(element, "autohalt")
    return net

def load_net_content(element, project, net):
    net.places = [ load_place(e, net) for e in element.findall("place") ]
    net.transitions = [ load_transition(e, project, net) for e in element.findall("transition") ]
    net.areas = [ load_area(e, net) for e in element.findall("area") ]

    interface = element.find("interface")
    if interface is not None:
        net.module_flag = True
        net.interface_edges_out = [ load_edge_out(e, net, None) for e in interface.findall("edge-out") ]
        net.interface_edges_in = [ load_edge_in(e, net, None) for e in interface.findall("edge-in") ]

def load_extern_type(element):
    t = utils.xml_str(element, "type")
    name = utils.xml_str(element, "name")

    if t == "native":
        rawtype = utils.xml_str(element, "raw-type")
        transport_mode = utils.xml_str(element, "transport-mode")
        transferable_to_octave = utils.xml_bool(element, "transferable-to-octave", False)
        codes = dict((utils.xml_str(e, "name"), e.text) for e in element.findall("code"))
        return ExternType(name, rawtype, transport_mode, codes,transferable_to_octave)

    if t == "protobuffer":
        raise Exception("Need implementation")

    raise Exception("Unkown extern type")

def load_user_function(element):
    id = utils.xml_int(element, "id")
    name = utils.xml_str(element, "name")
    with_context = utils.xml_bool(element, "with-context")
    parameters = parser.parse_parameters_declaration(utils.xml_str(element, "parameters"), None)
    returntype = parser.parse_type(utils.xml_str(element, "return-type"), None)
    if element.text is None:
        code = "\n"
    else:
        code = element.text
    return UserFunction(id, name, parameters, returntype, with_context, code)

def load_parameter(element):
    name = utils.xml_str(element, "name")
    description = utils.xml_str(element, "description")
    type = parser.parse_type(utils.xml_str(element, "type"), None)
    return Parameter(name, type, description)

def load_build_option(element, project):
    name = utils.xml_str(element, "name")
    value = element.text
    project.build_options[name] = value

def load_configuration(element, project):
    etypes = [ load_extern_type(e) for e in element.findall("extern-type") ]
    project.extern_types = utils.create_dict(etypes, lambda item: item.get_name())

    ufunctions = [ load_user_function(e) for e in element.findall("function") ]
    project.user_functions = utils.create_dict(ufunctions, lambda item: item.get_name())

    parameters = [ load_parameter(e) for e in element.findall("parameter") ]
    project.parameters = utils.create_dict(parameters, lambda item: item.get_name())

    for e in element.findall("build-option"):
        load_build_option(e, project)

def load_project(element):
    description = element.find("description").text
    name = utils.xml_str(element, "name")
    extenv = utils.xml_str(element, "extenv")
    target_mode = utils.xml_str(element, "target-mode", "default")
    p = Project(name, extenv, target_mode, description)

    load_configuration(element.find("configuration"), p)

    nets = [ (e, load_net(e, p)) for e in element.findall("net") ]
    p.nets = [ net for e, net in nets ]
    for e, net in nets:
        load_net_content(e, p, net)

    p.inject_types()
    p.check()
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
