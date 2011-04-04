#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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

from net import Net, load_net, ExportException
import xml.etree.ElementTree as xml
import utils
import copy
import os
import paths
from events import EventSource

class Project(EventSource):
	""" 
		Events: changed, filename_changed
	"""
	
	def __init__(self, file_name):
		assert file_name is not None
		EventSource.__init__(self)
		self.id_counter = 100
		self.set_filename(file_name)
		self.parameters = []
		self.extern_types = []
		self.param_values_cache = None
		self.error_messages = {}
		self.functions = []
		self.events = [
			Event("node_init", "void", "CaContext *ctx"),
			Event("node_quit", "void", "CaContext *ctx")
		]
		self.build_options = {
			"CC" : "g++",
			"CFLAGS" : "-O2",
			"LIBS" : ""
		}

	def new_id(self):
		self.id_counter += 1
		return self.id_counter

	def set_net(self, net):
		self.net = net
		net.set_change_callback(self._net_changed)
		self.changed()

	def get_net(self):
		return self.net

	def copy(self):
		return load_project_from_xml(self.as_xml(), self.filename)

	def get_name(self):
		d, fname = os.path.split(self.filename)
		name, ext = os.path.splitext(fname)
		return name

	def get_filename(self):
		return self.filename

	def get_filename_without_ext(self):
		name, ext = os.path.splitext(self.filename)
		return name

	def get_executable_filename(self):
		return self.get_filename_without_ext()

	def get_exported_filename(self):
		return self.get_filename_without_ext() + ".xml"

	def get_emitted_source_filename(self):
		return self.get_filename_without_ext() + ".cpp"

	def get_head_filename(self):
		return os.path.join(self.get_directory(), "head.cpp")

	def get_directory(self):
		return os.path.dirname(self.filename)

	def set_filename(self, filename):
		self.filename = os.path.abspath(filename)
		self.emit_event("filename_changed")

	# Cache is used for rerunning simulation with same parameters
	def get_param_value_cache(self):
		return self.param_values_cache

	def set_param_values_cache(self, param_values):
		self.param_values_cache = param_values

	def reset_param_values_cache(self):
		self.param_values_cache = None

	def set_error_messages(self, messages):
		self.error_messages = messages
		self.changed()

	def get_item(self, id):
		for function in self.functions:
			if function.id == id:
				return function
		for extern in self.extern_types:
			if extern.id == id:
				return extern
		return self.net.get_item(id)

	def has_error_messages(self, item):
		return item.get_id() in self.error_messages

	def get_error_messages(self, item):
		if item.get_id() in self.error_messages:
			return self.error_messages[item.get_id()]
		else:
			return None

	def get_build_option(self, name):
		if name in self.build_options:
			return self.build_options[name]
		else:
			return ""

	def set_build_option(self, name, value):
		self.build_options[name] = value
		self.changed()

	def changed(self):
		self.emit_event("changed")

	def _net_changed(self, net):
		self.changed()

	def as_xml(self):
		root = xml.Element("project")
		root.append(self._configuration_element())

		xml_net = self.net.as_xml()
		root.append(xml_net)
		return root

	def save(self):
		assert self.filename is not None
		f = open(self.filename, "w")
		try:
			f.write(xml.tostring(self.as_xml()))
		finally:
			f.close()

	def export(self, filename):
		root = xml.Element("project")

		root.append(self._configuration_element())

		description = xml.Element("description")
		description.text = xml.tostring(self.as_xml())
		root.append(description)

		xml_nets = self.net.export_xml()
		for e in xml_nets:
			root.append(e)

		f = open(filename, "w")
		try:
			f.write(xml.tostring(root))
		finally:
			f.close()

	def get_extern_types(self):
		return self.extern_types

	def get_functions(self):
		return self.functions

	def get_events(self):
		return self.events

	def get_event(self, name):
		for e in self.events:
			if e.get_name() == name:
				return e
		raise "Event '" + name + "' not found"

	def add_extern_type(self, obj):
		self.extern_types.append(obj)
		self.changed()

	def remove_extern_type(self, obj):
		self.extern_types.remove(obj)
		self.changed()

	def add_function(self, obj):
		obj.project = self
		self.functions.append(obj)
		self.changed()

	def remove_function(self, obj):
		self.functions.remove(obj)
		self.changed()

	def type_to_ctype(self, t):
		if t == "__Context":
			return "CaContext"
		if t == "Int":
			return "int"
		if t == "Bool":
			return "bool"
		if t == "String":
			return "std::string"
		for et in self.extern_types:
			if et.get_name() == t:
				return et.get_raw_type()
		return None

	def add_parameter(self, obj):
		obj.project = self
		self.reset_param_values_cache()
		self.parameters.append(obj)
		self.changed()

	def get_parameters(self):
		return self.parameters

	def remove_parameter(self, parameter):
		self.reset_param_values_cache()
		self.parameters.remove(parameter)
		self.changed()

	def write_project_files(self):
		self.write_makefile()
		utils.write_file_if_not_exists(self.get_head_filename(),
			"/* This file is included at the beginning of the main source file,\n" + 
			"   so definitions from this file can be used in functions in\n" +
			"   transitions and places. */\n\n")

	def write_makefile(self):
		makefile = utils.Makefile()
		makefile.set_top_comment("This file is autogenerated.\nDo not edit directly this file.")
		makefile.set("CC", self.get_build_option("CC"))
		makefile.set("CFLAGS", self.get_build_option("CFLAGS"))
		makefile.set("LIBDIR", "-L" + paths.CAILIE_DIR)
		makefile.set("LIBS", "-lcailie -lpthread -lrt " + self.get_build_option("LIBS"))
		makefile.set("INCLUDE", "-I" + paths.CAILIE_DIR)
		makefile.set("MPICC", "mpicc")

		if self.get_build_option("OTHER_FILES"):
			other_deps = [ os.path.splitext(f)[0] + ".o" for f in self.get_build_option("OTHER_FILES").split("\n") ]
		else:
			other_deps = []

		name_o = self.get_name() + ".o"
		name_cpp = self.get_name() + ".cpp"
		name_debug = self.get_name() + "_debug"
		name_debug_o = self.get_name() + "_debug.o"

		makefile.rule("all", [self.get_name()])
		makefile.rule("debug", [name_debug])
		makefile.rule("mpi", [self.get_name() + "_mpi"])
		makefile.rule("mpidebug", [self.get_name() + "_mpidebug"])

		deps = [ name_o ] + other_deps
		deps_debug = [ name_debug_o ] + other_deps
		makefile.rule(self.get_name(), deps, "$(CC) " + " ".join(deps) + " -o $@ $(CFLAGS) $(INCLUDE) $(LIBDIR) $(LIBS) " )

		makefile.rule(name_debug, deps_debug, "$(CC) " + " ".join(deps_debug) + " -o $@ $(CFLAGS) $(INCLUDE) $(LIBDIR) $(LIBS) " )

		makefile.rule(self.get_name() + "_mpi", deps, "$(MPICC) -cc=${CC} " + " ".join(deps)
			+ " -o $@ $(CFLAGS) $(INCLUDE) $(LIBDIR) -lmpicailie" )

		makefile.rule(self.get_name() + "_mpidebug", deps_debug, "$(MPICC) -cc=${CC} " + " ".join(deps_debug)
			+ " -o $@ $(CFLAGS) $(INCLUDE) $(LIBDIR) -lmpicailie" )

		makefile.rule(name_o, [ name_cpp, "head.cpp" ], "$(CC) $(CFLAGS) $(INCLUDE) -c {0} -o {1}".format(name_cpp, name_o))
		makefile.rule(name_debug_o, [ name_cpp, "head.cpp" ], "$(CC) -DCA_LOG_ON $(CFLAGS) $(INCLUDE) -c {0} -o {1}".format(name_cpp, name_debug_o))
		makefile.rule("clean", [], "rm -f *.o {0} {0}_debug {0}_mpi {0}_mpidebug".format(self.get_name()))
		makefile.rule(".cpp.o", [], "$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@")
		makefile.rule(".cc.o", [], "$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@")
		makefile.rule(".c.o", [], "$(CC) $(CFLAGS) $(INCLUDE) -c $< -o $@")
		makefile.write_to_file(os.path.join(self.get_directory(), "makefile"))

	def _build_option_as_xml(self, name):
		element = xml.Element("build-option")
		element.set("name", name)
		element.text = self.build_options[name]
		return element


	def _configuration_element(self):
		e = xml.Element("configuration")
		for p in self.parameters:
			e.append(p.as_xml())
		for t in self.extern_types:
			e.append(t.as_xml())
		for t in self.events:
			if t.has_code():
				e.append(t.as_xml())
		for t in self.functions:
			e.append(t.as_xml())
		for t in self.build_options:
			e.append(self._build_option_as_xml(t))
		return e

class Parameter:

	project = None

	def __init__(self):
		self.name = ""
		self.type = "Int"
		self.description = ""
		self.default = "0"

	def set_name(self, name):
		self.name = name
		self.changed()

	def get_name(self):
		return self.name

	def set_type(self, type):
		self.type = type
		self.changed()

	def get_type(self):
		return self.type

	def get_description(self):
		return self.description

	def get_default(self):
		return self.default

	def set_description(self, description):
		self.description = description
		self.changed()

	def set_default(self, default):
		self.default = default
		self.changed()

	def changed(self):
		if self.project:
			self.project.reset_param_values_cache()

	def as_xml(self):
		e = xml.Element("parameter")
		e.set("name", self.name)
		e.set("type", self.type)
		e.set("description", self.description)
		e.set("default", self.default)
		return e

class ExternType:
	"""
		Transport modes: "Disabled", "Direct", "Custom"
	"""

	def __init__(self, project, name = "", raw_type = "", transport_mode = "Disabled"):
		self.project = project
		self.id = self.project.new_id()
		self.name = name
		self.raw_type = raw_type
		self.transport_mode = transport_mode
		self.functions = {
			"getstring": "",
			"getsize": "",
			"pack": "",
			"unpack": ""
		}

	def get_name(self):
		return self.name

	def get_raw_type(self):
		return self.raw_type

	def get_transport_mode(self):
		return self.transport_mode

	def set_name(self, value):
		self.name = value

	def set_raw_type(self, value):
		self.raw_type = value

	def set_transport_mode(self, value):
		self.transport_mode = value

	def set_function_code(self, name, value):
		self.functions[name] = value

	def has_function(self, name):
		return "" != self.functions[name].strip()

	def get_function_code(self, name):
		if self.has_function(name):
			return self.functions[name]
		elif name == "getstring":
			return "\treturn \"" + self.name + "\";\n";
		else:
			return "\t\n"

	def is_function_allowed(self, name):
		if self.transport_mode == "Custom":
			return True
		else:
			return name == "getstring"

	def get_function_list_string(self):
		names = [ name for name in self.functions if self.has_function(name) and self.is_function_allowed(name) ]
		return ", ".join(names)

	def get_function_declaration(self, name):
		if name == "getstring":
			return "std::string getstring(" + self.raw_type + " &obj)"
		elif name == "getsize":
			return "size_t getstring(" + self.raw_type + " &obj)"
		elif name == "pack":
			return "void pack(CaPacker &packer, " + self.raw_type + " &obj)"
		elif name == "unpack":
			return self.raw_type + " unpack(CaUnpacker &unpacker)"


	def as_xml(self):
		e = xml.Element("extern-type")
		e.set("id", str(self.id))
		e.set("name", self.name)
		e.set("raw-type", self.raw_type)
		e.set("transport-mode", self.transport_mode)

		for name in self.functions:
			if self.has_function(name):
				fe = xml.Element("code")
				fe.set("name", name)
				fe.text = self.functions[name]
				e.append(fe)

		return e

class FunctionBase:

	def __init__(self, name):
		self.name = name
		self.code = ""

	def get_name(self):
		return self.name

	def set_name(self, name):
		self.name = name

	def set_function_code(self, code):
		if code is None:
			code = ""
		self.code = code

	def get_function_code(self):
		if self.has_code():
			return self.code
		else:
			return "\t\n"

	def has_code(self):
		return self.code.strip() != ""

	def get_function_declaration(self):
		return self.get_c_return_type() + " " + self.name + "(" + self.get_c_parameters() + ")"

class Event(FunctionBase):

	def __init__(self, name, c_return_type, c_parameters):
		FunctionBase.__init__(self, name)
		self.c_return_type = c_return_type
		self.c_parameters = c_parameters

	def get_c_parameters(self):
		return self.c_parameters

	def get_c_return_type(self):
		return self.c_return_type


	def as_xml(self):
		e = xml.Element("event")
		e.set("name", self.name)
		e.text = self.code
		return e

class Function(FunctionBase):

	project = None

	def __init__(self, project):
		FunctionBase.__init__(self, "")
		self.project = project
		self.id = self.project.new_id()
		self.return_type = ""
		self.parameters = ""
		self.with_context = False

	def get_parameters(self):
		return self.parameters

	def set_parameters(self, parameters):
		self.parameters = parameters

	def get_return_type(self):
		return self.return_type

	def set_return_type(self, return_type):
		self.return_type = return_type

	def get_with_context(self):
		return self.with_context

	def set_with_context(self, value):
		self.with_context = value

	def get_c_parameters(self):
		p = self.split_parameters()
		if p is None:
			return "Invalid format of parameters"
		else:
			params_str =	[ self.project.type_to_ctype(t) + " &" + n for (t, n) in p ]
			if self.with_context:
				params_str.insert(0, "CaContext *ctx")
			return ", ".join(params_str)

	def get_c_return_type(self):
		return self.project.type_to_ctype(self.return_type)

	def check_definition(self):
		if self.project.type_to_ctype(self.return_type) is None:
			return False
		for p in self.split_parameters():
			if len(p) != 2:
				return False
			if self.project.type_to_ctype(p[0]) is None:
				return False
		return True

	def split_parameters(self):
		return [ x.split() for x in self.parameters.split(",") if x.strip() != ""]

	def as_xml(self):
		e = xml.Element("function")
		e.set("id", str(self.id))
		e.set("name", self.name)
		e.set("return-type", self.return_type)
		e.set("parameters", self.parameters)
		e.set("with-context", str(self.with_context))
		e.text = self.code
		return e


def load_project(filename):
	doc = xml.parse(filename)
	root = doc.getroot()
	project, idtable = load_project_from_xml(root, filename)
	return project

def load_project_from_xml(root, filename):
	project = Project(filename)
	if root.find("configuration"):
		load_configuration(root.find("configuration"), project)
	net, idtable = load_net(root.find("net"), project)
	project.set_net(net)
	return project, idtable

def load_parameter(element, project):
	p = Parameter()
	p.set_name(utils.xml_str(element, "name"))
	p.set_description(utils.xml_str(element, "description", ""))
	p.set_default(utils.xml_str(element, "default", "0"))
	p.set_type(utils.xml_str(element, "type"))
	project.add_parameter(p)

def load_extern_type(element, project):
	p = ExternType(project)
	p.set_name(utils.xml_str(element, "name"))
	p.set_raw_type(utils.xml_str(element, "raw-type"))
	p.set_transport_mode(utils.xml_str(element, "transport-mode"))

	for e in element.findall("code"):
		name = utils.xml_str(e, "name")
		p.set_function_code(name, e.text)
	project.add_extern_type(p)

def load_function(element, project):
	f = Function(project)
	f.set_name(utils.xml_str(element, "name"))
	f.set_return_type(utils.xml_str(element, "return-type"))
	f.set_parameters(utils.xml_str(element, "parameters"))
	f.set_with_context(utils.xml_bool(element, "with-context", False))
	f.set_function_code(element.text)
	project.add_function(f)

def load_event(element, project):
	name = utils.xml_str(element, "name")
	event = project.get_event(name)
	event.set_function_code(element.text)

def load_build_option(element, project):
	name = utils.xml_str(element, "name")
	value = element.text
	if value is None: # For backward compatability
		return
	project.set_build_option(name, value)

def load_configuration(element, project):
	for e in element.findall("parameter"):
		load_parameter(e, project)
	for e in element.findall("extern-type"):
		load_extern_type(e, project)
	for e in element.findall("event"):
		load_event(e, project)
	for e in element.findall("build-option"):
		load_build_option(e, project)
	for e in element.findall("function"):
		load_function(e, project)

def new_empty_project(directory):
	os.mkdir(directory)
	name = os.path.basename(directory)
	project_filename = os.path.join(directory,name + ".proj")
	project = Project(project_filename)
	net = Net(project)
	project.set_net(net)
	project.write_project_files()
	project.save()
	return project
