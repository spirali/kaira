#
#    Copyright (C) 2010-2013 Stanislav Bohm
#                  2011       Ondrej Meca
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

import xml.etree.ElementTree as xml
import os
import ptp
import utils

from events import EventSource
from simconfig import SimConfig

class Project(EventSource):
    """
        Events: changed,
                netlist_changed,
                filename_changed,
                sequences_changed
    """

    communication_model_code = ""
    head_code = ""

    def __init__(self, file_name):
        assert file_name is not None
        EventSource.__init__(self)
        self.id_counter = 100
        self.set_filename(file_name)
        self.nets = []
        self.parameters = []
        self.sequences = []
        self.simconfig = SimConfig()
        self.error_messages = {}
        self.generator = None # PTP generator
        self.build_net = None

        # Library options
        self.library_rpc = False
        self.library_octave = False

    def get_build_option(self, name):
        if name in self.build_options:
            return self.build_options[name]
        else:
            return ""

    def get_generator(self):
        """ Can raise PtpException """
        if self.generator:
            return self.generator
        build_config = BuildConfig()
        build_config.project_name = self.get_name()
        build_config.target_env = self.get_target_env_name()
        build_config.tracing = False
        build_config.nets = self.nets
        self.generator = ptp.get_generator_from_xml(self.export_xml(build_config))
        return self.generator

    def get_build_config(self, name):
        build_config = BuildConfig()
        build_config.directory = os.path.join(self.get_directory(), name)
        build_config.project_name = self.get_name()

        use_build_net = True

        if name == "statespace":
            build_config.operation = "statespace"
            build_config.verification = True
        elif name == "simrun":
            build_config.operation = "simrun"
            build_config.substitutions = True
        elif name == "lib" or name == "libtraced":
            build_config.operation = "lib"
            build_config.library = True
            use_build_net = False
        else:
            build_config.operation = "build"

        if name == "traced" or name == "simrun" or name == "libtraced":
            build_config.tracing = True

        build_config.target_env = self.get_target_env_name()

        if use_build_net:
            nets = [ self.build_net ]
            nets += [ net for net in self.nets if net != self.build_net ]
        else:
            nets = self.nets[:]

        build_config.nets = nets
        return build_config

    def set_build_net(self, net):
        self.build_net = net
        self.emit_event("netlist_changed")

    def set_build_option(self, name, value):
        self.build_options[name] = value
        self.changed()

    def new_id(self):
        self.id_counter += 1
        return self.id_counter

    def add_net(self, net):
        self.nets.append(net)
        net.set_change_callback(self._net_changed)
        self.emit_event("netlist_changed")

    def add_sequence(self, sequence):
        self.sequences.append(sequence)
        self.emit_event("sequences_changed")

    def remove_sequence(self, sequence):
        self.sequences.remove(sequence)
        self.emit_event("sequences_changed")

    def find_net(self, id):
        for net in self.nets:
            if net.id == id:
                return net

    def remove_net(self, net):
        self.nets.remove(net)
        if self.build_net == net:
            self.build_net = self.nets[0]
        self.emit_event("netlist_changed")

    def get_modules(self):
        return [ net for net in self.nets if net.is_module() ]

    def get_simconfig(self):
        return self.simconfig

    def get_nets(self):
        return self.nets

    def get_nets_with_interface(self):
        return [ net for net in self.nets if net.is_module() ]

    def get_name(self):
        d, fname = os.path.split(self.filename)
        name, ext = os.path.splitext(fname)
        return name

    def reset_param_values(self):
        self.simconfig.reset_param_values()

    def get_filename(self):
        return self.filename

    def get_filename_without_ext(self):
        name, ext = os.path.splitext(self.filename)
        return name

    def get_directory(self):
        return os.path.dirname(self.filename)

    def set_filename(self, filename):
        self.filename = os.path.abspath(filename)
        self.emit_event("filename_changed")

    def set_error_messages(self, messages):
        self.error_messages = messages
        self.changed("error_messages")

    def get_item(self, id):
        for net in self.nets:
            item = net.get_item(id)
            if item is not None:
                return item

    def get_net_and_item(self, id):
        for net in self.nets:
            item = net.get_item(id)
            if item is not None:
                return net, item
        return None, None

    def has_error_messages(self, item):
        return item.get_id() in self.error_messages

    def get_error_messages(self, item):
        if item.get_id() in self.error_messages:
            return self.error_messages[item.get_id()]
        else:
            return None

    def changed(self, obj=None):
        self.generator = None # Invalidate generator cache
        self.emit_event("changed", obj)

    def _net_changed(self, net):
        self.changed(net)

    def as_xml(self):
        root = xml.Element("project")
        root.set("target_env", self.get_target_env_name())
        root.set("library-rpc", str(self.library_rpc))
        root.set("library-octave", str(self.library_octave))
        root.append(self._configuration_element(None))
        for net in self.nets:
            root.append(net.as_xml())
        for sequence in self.sequences:
            root.append(sequence.as_xml())
        return root

    def save(self):
        assert self.filename is not None
        f = open(self.filename, "w")
        try:
            f.write(xml.tostring(self.as_xml()))
        finally:
            f.close()

    # takes instace of BuildConfig
    # Returns xml.Element
    def export_xml(self, build_config):
        root = xml.Element("project")
        root.set("name", self.get_name())
        root.set("target_env", build_config.target_env)
        root.set("root-directory", self.get_directory())

        if build_config.library:
            root.set("library-rpc", str(self.library_rpc))
            root.set("library-octave", str(self.library_octave))

        if build_config.tracing:
            root.set("tracing", "True")

        root.append(self._configuration_element(build_config))

        description = xml.Element("description")
        description.text = xml.tostring(self.as_xml())
        root.append(description)

        for net in build_config.nets:
            root.append(net.export_xml(build_config))

        return root

    def export(self, build_config):
        utils.makedir_if_not_exists(build_config.directory)
        content = xml.tostring(self.export_xml(build_config))
        f = open(build_config.get_export_filename(), "w")
        try:
            f.write(content)
        finally:
            f.close()

    def add_parameter(self, obj):
        obj.project = self
        self.reset_param_values()
        self.parameters.append(obj)
        self.changed()

    def get_parameters(self):
        return self.parameters

    def remove_parameter(self, parameter):
        self.reset_param_values()
        self.parameters.remove(parameter)
        self.changed()

    def get_head_code(self):
        return self.head_code

    def set_head_code(self, code):
        self.head_code = code
        self.changed()

    def _build_option_as_xml(self, name):
        element = xml.Element("build-option")
        element.set("name", name)
        element.text = self.get_build_option(name)
        return element

    def _configuration_element(self, build_config):
        e = xml.Element("configuration")
        for p in self.parameters:
            e.append(p.as_xml())
        for t in self.build_options:
            e.append(self._build_option_as_xml(t))

        if self.get_head_code():
            element = xml.Element("head-code")
            element.text = self.get_head_code()
            e.append(element)

        if build_config is None or build_config.substitutions:
            if self.communication_model_code.strip():
                element = xml.Element("communication-model")
                element.text = self.communication_model_code
                e.append(element)

        return e


class BuildConfig:

    tracing = False
    substitutions = False
    verification = False
    directory = None
    project_name = None
    nets = None
    target_env = None
    operation = None
    library = False

    def get_filename(self, filename):
        return os.path.join(self.directory, filename)

    def get_export_filename(self):
        return self.get_filename(self.project_name + ".xml")

    def get_executable_filename(self):
        return self.get_filename(self.project_name)


class Parameter:
    project = None

    def __init__(self):
        self.name = ""
        self.type = "int"
        self.description = ""
        self.policy = "mandatory"
        self.default = "0"

    def changed(self):
        if self.project:
            self.project.reset_param_values()

    def is_editable(self):
        return self.policy != "constant"

    def as_xml(self):
        e = xml.Element("parameter")
        e.set("name", self.name)
        e.set("type", self.type)
        e.set("description", self.description)
        e.set("default", self.default)
        e.set("policy", self.policy)
        return e
