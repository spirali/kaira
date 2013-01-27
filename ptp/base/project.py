#
#    Copyright (C) 2011, 2012, 2013 Stanislav Bohm
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

import xml.etree.ElementTree as xml
from base.utils import get_source_path
from net import Net, Area, Place, Transition, Edge

class ExternType(object):

    def __init__(self,
                 id,
                 name,
                 rawtype,
                 transport_mode,
                 codes,
                 octave_value,
                 hash_function):
        self.id = id
        self.name = name
        self.rawtype = rawtype
        self.transport_mode = transport_mode
        self.codes = codes
        self.octave_value =  octave_value
        self.hash_function = hash_function

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

    def is_octave_value(self):
        return self.octave_value

    def has_hash_function(self):
        return self.hash_function


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

    def meet_declaration(self, returntype, parameter_types):
        return self.returntype == returntype and \
               [ t for _, t in self.parameters ] == parameter_types


class Parameter(object):

    def __init__(self, name, type, default, description, policy):
        self.name = name
        self.type = type
        self.default = default
        self.description = description
        self.policy = policy

    def get_name(self):
        return self.name

    def get_type(self):
        return self.type

    def get_description(self):
        return self.description

    def get_policy(self):
        return self.policy


class Project(object):

    def __init__(self, name, root_directory, target_env, target_mode, description):
        self.name = name
        self.root_directory = root_directory
        self.target_env = target_env
        self.target_mode = target_mode
        self.nets = []
        self.description = description
        self.extern_types = {}
        self.user_functions = {}
        self.parameters = {}
        self.build_options = {}
        self.head_code = ""

    def get_root_directory(self):
        return self.root_directory

    def get_name(self):
        return self.name

    def get_target_mode(self):
        return self.target_mode

    def get_modules(self):
        return [ net for net in self.nets if net.is_module() ]

    def get_build_option(self, name):
        value = self.build_options.get(name)
        if value is None:
            return ""
        else:
            return value

    def get_extern_types(self):
        return self.extern_types.values()

    def get_extern_type(self, name):
        return self.extern_types.get(name)

    def get_user_functions(self):
        return self.user_functions.values()

    def get_user_function(self, name):
        return self.user_functions.get(name)

    def get_user_functions_by_declaration(self, returntype, parameter_types):
        return [ function for function in self.user_functions.values()
                    if function.meet_declaration(returntype, parameter_types) ]

    def get_parameter(self, name):
        return self.parameters.get(name)

    def get_parameters(self):
        return self.parameters.values()

    def get_net(self, id):
        for net in self.nets:
            if net.id == id:
                return net

    def get_net_of_edge(self, edge):
        for net in self.nets:
            if edge.uid in [ e.uid for e in net.get_all_edges() ]:
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

    def get_head_code(self):
        return self.head_code

    def check(self):
        checker = self.target_env.get_checker(self)
        for net in self.nets:
            net.check(checker)
        checker.run()

    def analyze(self):
        for net in self.nets:
            net.analyze()

    def is_expr_variable(self, expr):
        return self.target_env.is_expr_variable(expr)

    def parse_typename(self, string, source):
        return self.target_env.parse_typename(string, source)

    def parse_expressions(self, string, source):
        return self.target_env.parse_expressions(string, source)

    def parse_init_expression(self, string, source):
        return self.target_env.parse_init_expression(string, source)

    def parse_expression(self, string, source, allow_empty=False):
        return self.target_env.parse_expression(string, source, allow_empty)

    def parse_edge_expression(self, string, source):
        return self.target_env.parse_edge_expression(string, source)

    def get_generator(self):
        return self.target_env.get_generator(self)


def get_source(element, name):
    id = utils.xml_int(element, "id")
    return get_source_path(id, name)

def load_edge_in(element, project, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    config, expressions, target = project.parse_edge_expression(
        element.get("expr"), get_source(element, "inscription"))
    return Edge(id, transition, net.get_place(place_id), expressions, config, target)

def load_edge_out(element, project, net, transition):
    # Loading edge-in and edge-out are now same
    return load_edge_in(element, project, net, transition)

def load_tracing(element):
    trace = []
    for t in element.findall("trace"):
        trace.append(t.text)
    return trace

def load_transition(element, project, net):
    id = utils.xml_int(element, "id")

    guard = project.parse_expression(element.get("guard"),
                                     get_source(element, "guard"),
                                     allow_empty=True)
    transition = Transition(net, id, guard)
    transition.edges_in = map(lambda e:
        load_edge_in(e, project, net, transition), element.findall("edge-in"))
    transition.edges_out = map(lambda e:
        load_edge_out(e, project, net, transition), element.findall("edge-out"))

    if element.find("code") is not None:
        transition.code = element.find("code").text
    transition.tracing = load_tracing(element)
    return transition

def load_place(element, project, net):
    id = utils.xml_int(element, "id")
    type_name = project.parse_typename(element.get("type"),
                                       get_source(element, "type"))
    init_type, init_value = project.parse_init_expression(element.get("init-expr"),
                                                      get_source(element, "init-expr"))
    place = Place(net, id, type_name, init_type, init_value)
    if element.find("code") is not None:
        place.code = element.find("code").text
    place.tracing = load_tracing(element)
    return place

def load_area(element, project, net):
    id = utils.xml_int(element, "id")
    init_type, init_value = project.parse_init_expression(element.get("init-expr"),
                                                      get_source(element, "init-expr"))
    places = [ net.get_place(utils.xml_int(e, "id")) for e in element.findall("place") ]
    return Area(net, id, init_type, init_value, places)

def load_net(element, project):
    net = Net(project, utils.xml_int(element, "id"), utils.xml_str(element, "name"))
    return net

def load_net_content(element, project, net):
    net.places = [ load_place(e, project, net) for e in element.findall("place") ]
    net.transitions = [ load_transition(e, project, net) for e in element.findall("transition") ]
    net.areas = [ load_area(e, project, net) for e in element.findall("area") ]

    interface = element.find("interface")
    if interface is not None:
        net.module_flag = True
        net.interface_edges_out = [ load_edge_out(e, net, None) for e in interface.findall("edge-out") ]
        net.interface_edges_in = [ load_edge_in(e, net, None) for e in interface.findall("edge-in") ]

def load_extern_type(element):
    t = utils.xml_str(element, "type")
    name = utils.xml_str(element, "name")
    id = utils.xml_int(element, "id")

    if t == "native":
        rawtype = utils.xml_str(element, "raw-type")
        transport_mode = utils.xml_str(element, "transport-mode")
        octave_value = utils.xml_bool(element, "octave-value")
        hash_function = utils.xml_bool(element, "hash")
        codes = dict((utils.xml_str(e, "name"), e.text)
                      for e in element.findall("code"))
        return ExternType(id,
                          name,
                          rawtype,
                          transport_mode,
                          codes,
                          octave_value,
                          hash_function)

    if t == "protobuffer":
        raise Exception("Need implementation")

    raise Exception("Unkown extern type")

def load_parameter(element, project):
    name = utils.xml_str(element, "name")
    default = utils.xml_str(element, "default")
    description = utils.xml_str(element, "description")
    type = project.parse_typename(utils.xml_str(element, "type"), None)
    policy = utils.xml_str(element, "policy")
    return Parameter(name, type, default, description, policy)

def load_build_option(element, project):
    name = utils.xml_str(element, "name")
    value = element.text
    project.build_options[name] = value

def load_configuration(element, project):
    etypes = [ load_extern_type(e) for e in element.findall("extern-type") ]
    project.extern_types = utils.create_dict(etypes, lambda item: item.get_name())

    parameters = [ load_parameter(e, project) for e in element.findall("parameter") ]
    project.parameters = utils.create_dict(parameters, lambda item: item.get_name())

    for e in element.findall("build-option"):
        load_build_option(e, project)

    head_code = element.find("head-code")
    if head_code is not None:
        project.head_code = head_code.text

def load_project(element, target_envs):
    target_env = utils.xml_str(element, "extenv")
    if target_env not in target_envs:
        raise utils.PtpException("Unknown target environment")

    description = element.find("description").text
    name = utils.xml_str(element, "name")
    target_mode = utils.xml_str(element, "target-mode", "default")
    root_directory = utils.xml_str(element, "root-directory")

    p = Project(name,
                root_directory,
                target_envs[target_env],
                target_mode,
                description)

    load_configuration(element.find("configuration"), p)

    nets = [ (e, load_net(e, p)) for e in element.findall("net") ]
    p.nets = [ net for e, net in nets ]
    for e, net in nets:
        load_net_content(e, p, net)

    p.check()
    p.analyze()
    return p

def load_project_from_file(filename, target_envs):
    doc = xml.parse(filename)
    return load_project(doc.getroot(), target_envs)

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
