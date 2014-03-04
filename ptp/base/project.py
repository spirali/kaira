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

import base.utils as utils

import xml.etree.ElementTree as xml
from base.utils import get_source_path
from net import Net, Area, Place, Transition, Edge, Declarations, EdgeInscription

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

    def __init__(self, name, root_directory, target_env, description):
        self.name = name
        self.root_directory = root_directory
        self.target_env = target_env
        self.nets = []
        self.description = description
        self.parameters = {}
        self.build_options = {}
        self.head_code = ""
        self.communication_model_code = ""
        self.library_rpc = False
        self.library_octave = False
        self.tracing = False
        self.build_target = None

    def get_root_directory(self):
        return self.root_directory

    def get_name(self):
        return self.name

    def get_build_option(self, name):
        value = self.build_options.get(name)
        if value is None:
            return ""
        else:
            return value

    def get_build_with_octave(self):
        return self.library_octave or self.get_build_option("USE_OCTAVE") == "True"

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
        if self.library_octave:
            import ptp # Import here to avoid cyclyc import
            if ptp.get_config("Main", "OCTAVE") != "True":
                raise utils.PtpException("Cannot build a module for Octave, "
                                         "Kaira is not build with Octave support.\n"
                                         "Run './waf configure' in Kaira root directory")

        if self.get_build_with_octave():
            import ptp # Import here to avoid cyclyc import
            if ptp.get_config("Main", "OCTAVE") != "True":
                raise utils.PtpException("Cannot build a project with Octave C++ API, "
                                         "Kaira is not build with Octave support.\n"
                                         "Run './waf configure' in Kaira root directory")

        checker = self.target_env.get_checker(self)
        for net in self.nets:
            net.check(checker)
        checker.run()

    def analyze(self):
        for net in self.nets:
            net.analyze()

    def is_expr_variable(self, expr):
        return self.target_env.is_expr_variable(expr)

    def get_expr_variables(self, expr):
        return self.target_env.get_expr_variables(expr)

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

    def get_minimal_decls(self):
        decls = Declarations()
        decls.set("ctx", "ca::Context");
        return decls


def get_source(element, name):
    id = utils.xml_int(element, "id")
    return get_source_path(id, name)

def load_edge_in(element, project, net, transition):
    id = utils.xml_int(element, "id")
    place_id = utils.xml_int(element, "place-id")
    inscriptions = [ EdgeInscription(config, expr, target)
                     for (config, expr, target) in
                         project.parse_edge_expression(element.get("expr"),
                                                       get_source(element, "inscription")) ]
    edge = Edge(id, transition, net.get_place(place_id), inscriptions)
    if element.find("size-substitution") is not None:
        edge.size_substitution = element.find("size-substitution").text
    return edge

def load_edge_out(element, project, net, transition):
    # Loading edge-in and edge-out are now same
    return load_edge_in(element, project, net, transition)

def load_transition(element, project, net):
    id = utils.xml_int(element, "id")

    name = element.get("name")
    guard = project.parse_expression(element.get("guard"),
                                     get_source(element, "guard"),
                                     allow_empty=True)
    transition = Transition(net, id, name, guard)
    transition.collective = utils.xml_bool(element, "collective", False)
    transition.edges_in = map(lambda e:
        load_edge_in(e, project, net, transition), element.findall("edge-in"))
    transition.edges_out = map(lambda e:
        load_edge_out(e, project, net, transition), element.findall("edge-out"))
    if element.find("code") is not None:
        transition.code = element.find("code").text
    transition.trace_fire = element.find("trace") is not None
    transition.clock = utils.xml_bool(element, "clock", False)

    if transition.collective:
        transition.root = project.parse_expression(element.get("root"),
                                                   get_source(element, "root"),
                                                   allow_empty=True)

    if element.find("time-substitution") is not None:
        transition.time_substitution = element.find("time-substitution").text
    if element.find("clock-substitution") is not None:
        transition.clock_substitution = element.find("clock-substitution").text

    if element.find("verif-quit_flag") is not None:
        transition.calls_quit = True
    e = element.find("verif-occurrence")
    if e is not None:
        transition.occurrence_analysis = True
        transition.occurrence_analysis_compare_process = utils.xml_bool(e, "process")
        transition.occurrence_analysis_compare_binding = utils.xml_bool(e, "binding")

    priority = element.get("priority").strip()
    if priority == "":
        transition.priority = 0
    elif utils.is_integer(priority):
        transition.priority = int(priority)
    else:
        raise utils.PtpException("Priority has to be integer or empty",
                                 get_source(element, "priority"))
    return transition

def load_place_tracing(element, place):
    if element is None:
        return
    place.trace_tokens = utils.xml_bool(element, "trace-tokens", False)
    for e in element.findall("function"):
        place.trace_tokens_functions.append((e.get("name"), e.get("return-type")))

def load_place(element, project, net):
    id = utils.xml_int(element, "id")
    typename = element.get("type")
    project.parse_typename(typename,
                           get_source(element, "type")) # Throws exception if incorrect type
    init_type, init_value = project.parse_init_expression(element.get("init-expr", ""),
                                                      get_source(element, "init"))
    place = Place(net, id, typename, init_type, init_value)
    if element.find("code") is not None:
        place.code = element.find("code").text

    load_place_tracing(element.find("trace"), place)

    if element.find("verif-final-marking") is not None:
        place.final_marking = bool(element.find("verif-final-marking").text)
    place.interface_input = element.get("in")
    place.interface_output = element.get("out")
    return place

def load_area(element, project, net):
    id = utils.xml_int(element, "id")
    init_type, init_value = project.parse_init_expression(
        element.get("init-expr"),
        get_source(element, "init"))
    if init_type is None:
        raise utils.PtpException("Expression is empty", get_source(element, "init"))
    places = [ net.get_place(utils.xml_int(e, "id"))
               for e in element.findall("place") ]
    return Area(net, id, init_type, init_value, places)

def load_net(element, project):
    net = Net(project, utils.xml_int(element, "id"), utils.xml_str(element, "name"))
    return net

def load_net_content(element, project, net):
    net.places = [ load_place(e, project, net) for e in element.findall("place") ]
    net.transitions = [ load_transition(e, project, net)
                        for e in element.findall("transition") ]
    net.transitions.sort(key=lambda t: t.priority, reverse=True)
    net.areas = [ load_area(e, project, net) for e in element.findall("area") ]

def load_parameter(element, project):
    name = utils.xml_str(element, "name")
    default = utils.xml_str(element, "default")
    description = utils.xml_str(element, "description")
    typename = element.get("type")
    project.parse_typename(typename, None)
    policy = utils.xml_str(element, "policy")
    return Parameter(name, typename, default, description, policy)

def load_build_option(element, project):
    name = utils.xml_str(element, "name")
    value = element.text
    project.build_options[name] = value

def load_configuration(element, project):
    parameters = [ load_parameter(e, project) for e in element.findall("parameter") ]
    project.parameters = utils.create_dict(parameters, lambda item: item.get_name())

    for e in element.findall("build-option"):
        load_build_option(e, project)

    head_code = element.find("head-code")
    if head_code is not None:
        project.head_code = head_code.text

    communication_model_code = element.find("communication-model")
    if communication_model_code is not None:
        project.communication_model_code = communication_model_code.text

def load_project(element, target_envs, build_target="build"):
    target_env = utils.xml_str(element, "target_env")
    if target_env not in target_envs:
        raise utils.PtpException("Unknown target environment")

    description = element.find("description").text
    name = utils.xml_str(element, "name")
    root_directory = utils.xml_str(element, "root-directory")

    p = Project(name,
                root_directory,
                target_envs[target_env],
                description)
    p.build_target = build_target

    p.library_rpc = utils.xml_bool(element, "library-rpc", False)
    p.library_octave = utils.xml_bool(element, "library-octave", False)
    p.tracing = utils.xml_bool(element, "tracing", False)

    load_configuration(element.find("configuration"), p)

    nets = [ (e, load_net(e, p)) for e in element.findall("net") ]
    p.nets = [ net for e, net in nets ]
    for e, net in nets:
        load_net_content(e, p, net)

    p.check()
    p.analyze()
    return p

def load_project_from_file(filename, target_envs, build_target="build"):
    doc = xml.parse(filename)
    return load_project(doc.getroot(), target_envs, build_target)
