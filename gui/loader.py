#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#    Copyright (C) 2011 Ondrej Meca
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

from net import Net, load_net, nets_postload_process
import xml.etree.ElementTree as xml
import utils
import os

import cpp
import java

languages = {
    "C++" : cpp.ProjectCpp,
    "Java" : java.ProjectJava
}

def create_project(filename, language):
    return languages[language](filename, language)

class BasicLoader:
    def __init__(self, project):
        self.project = project

    def get_id(self, element):
        id = utils.xml_int(element, "id", 0)
        self.project.id_counter = max(self.project.id_counter, id)
        return id

    def translate_id(self, id):
        return id

def load_project(filename):
    doc = xml.parse(filename)
    return load_project_from_xml(doc.getroot(), filename)

def load_project_from_xml(root, filename):
    lang = utils.xml_str(root, "language", "C++")
    project = create_project(filename, lang)
    loader = BasicLoader(project)
    if root.find("configuration") is not None:
        load_configuration(root.find("configuration"), project, loader)
    for e in root.findall("net"):
        project.add_net(load_net(e, project, loader))
    nets_postload_process(project)
    project.id_counter += 1
    return project

def load_parameter(element, project):
    p = project.get_instance_of("parameter")()
    p.set_name(utils.xml_str(element, "name"))
    p.set_description(utils.xml_str(element, "description", ""))
    p.set_default(utils.xml_str(element, "default", "0"))
    p.set_type(utils.xml_str(element, "type"))
    project.add_parameter(p)

def load_extern_type(element, project):
    p = project.get_instance_of("extern_type")()
    p.set_name(utils.xml_str(element, "name"))
    p.set_raw_type(utils.xml_str(element, "raw-type"))
    p.set_transport_mode(utils.xml_str(element, "transport-mode"))

    for e in element.findall("code"):
        name = utils.xml_str(e, "name")
        p.set_function_code(name, e.text)
    project.add_extern_type(p)

def load_function(element, project, loader):
    id = loader.get_id(element)
    f =  project.get_instance_of("function")(id)
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

def new_empty_project(directory, language):
    os.mkdir(directory)
    name = os.path.basename(directory)
    project_filename = os.path.join(directory,name + ".proj")
    project = create_project(project_filename, language)
    project.add_net(Net(project, "Main"))
    project.write_project_files()
    project.save()
    return project
