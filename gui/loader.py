#
#    Copyright (C) 2010-2013 Stanislav Bohm
#    Copyright (C) 2011, 2012 Ondrej Meca
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

from net import Net, load_net, BasicLoader, NewIdLoader
import xml.etree.ElementTree as xml
import utils
import os

from project import Parameter
import projectcpp
import controlseq

projects = [
    projectcpp.ProjectCpp,
]

def create_project(filename, target_env_name):
    for project_class in projects:
        if project_class.get_target_env_name() == target_env_name:
            return project_class(filename)
    raise Exception("Target environment '{0}' not found".format(target_env_name))

def load_project(filename):
    doc = xml.parse(filename)
    return load_project_from_xml(doc.getroot(), filename)

def load_project_from_xml(root, filename):
    target_env_name = root.get("target_env")
    if target_env_name is None: # For backward compatability
        target_env_name = root.get("extenv", "C++")
    project = create_project(filename, target_env_name)
    project.library_rpc = utils.xml_bool(root, "library-rpc", False)
    project.library_octave = utils.xml_bool(root, "library-octave", False)
    loader = BasicLoader(project)
    if root.find("configuration") is not None:
        load_configuration(root.find("configuration"), project, loader)
    for e in root.findall("net"):
        project.add_net(load_net(e, project, loader))
    assert project.nets
    for e in root.findall("sequence"):
        project.sequences.append(controlseq.ControlSequence(element=e))
    project.build_net = project.nets[0]

    project.id_counter += 1

    return project

def load_parameter(element, project):
    p = Parameter()
    p.name = utils.xml_str(element, "name")
    p.description = utils.xml_str(element, "description", "")
    p.default = utils.xml_str(element, "default", "0")
    p.type = utils.xml_str(element, "type")
    p.policy = utils.xml_str(element, "policy", "mandatory")
    p.changed()
    project.add_parameter(p)

def load_build_option(element, project):
    name = utils.xml_str(element, "name")
    value = element.text
    if value is None: # For backward compatability
        return
    if name == "CC": # For backward compatability
        return
    project.set_build_option(name, value)

def load_configuration(element, project, loader):
    for e in element.findall("parameter"):
        load_parameter(e, project)
    for e in element.findall("build-option"):
        load_build_option(e, project)
    if element.find("head-code") is not None:
        project.set_head_code(element.find("head-code").text)
    if element.find("communication-model") is not None:
        project.communication_model_code = element.find("communication-model").text

def import_project(project, filename):
    doc = xml.parse(filename)
    return import_project_from_xml(project, doc.getroot(), filename)

def import_project_from_xml(project, root, filename):
    loader = NewIdLoader(project)
    if root.find("configuration") is not None:
        load_configuration(root.find("configuration"), project, loader)
    for e in root.findall("net"):
        net = load_net(e, project, loader)
        project.add_net(net)
    project.id_counter += 1
    return project

def new_empty_project(directory, target_env_name):
    os.mkdir(directory)
    name = os.path.basename(directory)
    project_filename = os.path.join(directory,name + ".proj")
    project = create_project(project_filename, target_env_name)

    project.add_net(Net(project, name))
    project.build_net = project.nets[0]

    project.save()
    return project
