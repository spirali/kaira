from net import Net, load_net
import xml.etree.ElementTree as xml
import utils
import copy
import os
from events import EventSource

class Project(EventSource):
	""" 
		Events: changed, filename_changed
	"""
	
	def __init__(self, file_name):
		EventSource.__init__(self)
		self.id_counter = 100
		self.filename = file_name
		self.parameters = []

	def new_id(self):
		self.id_counter += 1
		return self.id_counter

	def set_net(self, net):
		self.net = net
		net.set_change_callback(self._net_changed)
		self.changed()

	def copy(self):
		return load_project_from_xml(self.as_xml(), self.filename)

	def get_name(self):
		if self.filename is None:
			return "<unnamed>"
		else:
			d, fname = os.path.split(self.filename)
			name, ext = os.path.splitext(fname)
			return name

	def get_filename(self):
		return self.filename

	def set_filename(self, filename):
		self.filename = filename
		self.emit_event("filename_changed")

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

		xml_nets = self.net.export_xml()
		for e in xml_nets:
			root.append(e)

		f = open(filename, "w")
		try:
			f.write(xml.tostring(root))
		finally:
			f.close()

	def new_parameter(self):
		p = Parameter()
		self.parameters.append(p)
		self.changed()
		return p

	def get_parameters(self):
		return self.parameters

	def remove_parameter(self, parameter):
		self.parameters.remove(parameter)
		self.changed()

	def _configuration_element(self):
		e = xml.Element("configuration")
		for p in self.parameters:
			e.append(p.as_xml())
		return e

class Parameter:

	def __init__(self):
		self.name = ""
		self.type = "Int"
		self.description = ""

	def set_name(self, name):
		self.name = name

	def get_name(self):
		return self.name

	def set_type(self, type):
		self.type = type

	def get_type(self):
		return self.type

	def get_description(self):
		return self.description

	def set_description(self, description):
		self.description = description

	def as_xml(self):
		e = xml.Element("parameter")
		e.set("name", self.name)
		e.set("type", self.type)
		e.set("description", self.description)
		return e

def load_project(filename):
	doc = xml.parse(filename)
	root = doc.getroot()
	return load_project_from_xml(root, filename)

def load_project_from_xml(root, filename):
	project = Project(filename)
	if root.find("configuration"):
		load_configuration(root.find("configuration"), project)
	project.set_net(load_net(root.find("net"), project))
	return project

def load_parameter(element, project):
	p = project.new_parameter()
	p.set_name(utils.xml_str(element, "name"))
	p.set_description(utils.xml_str(element, "description", ""))
	p.set_type(utils.xml_str(element, "type"))

def load_configuration(element, project):
	for e in element.findall("parameter"):
		load_parameter(e, project)

def new_empty_project():
	#p = load_project("../samples/pingpong.proj")
	#p.export("export.xml")
	#return p

	project = Project(None)
	net = Net(project)
	project.set_net(net)
	return project

