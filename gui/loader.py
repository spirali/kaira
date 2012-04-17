#
#    Copyright (C) 2010, 2011, 2012 Stanislav Bohm
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

from net import Net, load_net, nets_postload_process, BasicLoader, NewIdLoader
import xml.etree.ElementTree as xml
import utils
import os

from project import Parameter, Function
import projectcpp
import projectjava

projects = [
    projectcpp.ProjectCpp,
    projectcpp.ProjectCppLibrary,
    projectjava.ProjectJava
]

def create_project(filename, extenv_name):
    for project_class in projects:
        if project_class.get_extenv_name() == extenv_name:
            return project_class(filename)
    raise Exception("Extern environment '{0}' not found".format(extenv_name))

def load_project(filename):
    doc = xml.parse(filename)
    return load_project_from_xml(doc.getroot(), filename)

def load_project_from_xml(root, filename):
    extenv_name = root.get("extenv", "C++")
    project = create_project(filename, extenv_name)
    if root.get("target-mode"):
        project.target_mode = root.get("target-mode")
    loader = BasicLoader(project)
    if root.find("configuration") is not None:
        load_configuration(root.find("configuration"), project, loader)
    for e in root.findall("net"):
        project.add_net(load_net(e, project, loader))
    nets_postload_process(project, loader)
    project.id_counter += 1
    return project

def load_parameter(element, project):
    p = Parameter()
    p.set_name(utils.xml_str(element, "name"))
    p.set_description(utils.xml_str(element, "description", ""))
    p.set_default(utils.xml_str(element, "default", "0"))
    p.set_type(utils.xml_str(element, "type"))
    project.add_parameter(p)

def load_extern_type(element, project):
    # Default value is "native" for backward compatability
    t = utils.xml_str(element, "type", "native")
    p = project.create_extern_type(t)
    p.set_name(utils.xml_str(element, "name"))

    if t == "native":
        p.set_raw_type(utils.xml_str(element, "raw-type"))
        p.set_transport_mode(utils.xml_str(element, "transport-mode"))

        for e in element.findall("code"):
            name = utils.xml_str(e, "name")
            p.set_function_code(name, e.text)
    elif t == "protobuffer":
        p.code = element.text
    project.add_extern_type(p)

def load_function(element, project, loader):
    id = loader.get_id(element)
    f =  Function(id)
    f.set_name(utils.xml_str(element, "name"))
    f.set_return_type(utils.xml_str(element, "return-type"))
    f.set_parameters(utils.xml_str(element, "parameters"))
    f.set_with_context(utils.xml_bool(element, "with-context", False))
    f.set_function_code(element.text)
    project.add_function(f)

def load_build_option(element, project):
    name = utils.xml_str(element, "name")
    value = element.text
    if value is None: # For backward compatability
        return
    project.set_build_option(name, value)

def load_configuration(element, project, loader):
    for e in element.findall("parameter"):
        load_parameter(e, project)
    for e in element.findall("extern-type"):
        load_extern_type(e, project)
    for e in element.findall("build-option"):
        load_build_option(e, project)
    for e in element.findall("function"):
        load_function(e, project, loader)

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
    nets_postload_process(project, loader)
    project.id_counter += 1
    return project

def new_empty_project(directory, extenv_name):
    os.mkdir(directory)
    name = os.path.basename(directory)
    project_filename = os.path.join(directory,name + ".proj")
    project = create_project(project_filename, extenv_name)
    if project.is_library():
        net = Net(project, name)
        net.add_interface_box((20, 20), (400, 300))
        project.add_net(net)
    else:
        project.add_net(Net(project, "Main"))
    project.write_project_files()
    project.save()
    return project
