from net import Net, load_net
import xml.etree.ElementTree as xml
import utils
import copy

class Project:
	
	def __init__(self, project_name):
		self.id_counter = 100
		self.name = project_name
		self.parameters = []
		self.change_callback = lambda p: None

	def new_id(self):
		self.id_counter += 1
		return self.id_counter

	def set_net(self, net):
		self.net = net
		net.set_change_callback(self._net_changed)
		self.changed()

	def copy(self):
		return load_project_from_xml(self.as_xml())

	def get_name(self):
		return name

	def save(self, filename):
		pass

	def load(self, filename):
		pass

	def set_change_callback(self, callback):
		self.change_callback = callback

	def changed(self):
		self.change_callback(self)

	def _net_changed(self, net):
		self.changed()

	def as_xml(self):
		root = xml.Element("project")
		root.set("name", self.name)

		root.append(self._configuration_element())

		xml_net = self.net.as_xml()
		root.append(xml_net)
		return root

	def save(self, filename):
		f = open(filename, "w")
		try:
			f.write(xml.tostring(self.as_xml()))
		finally:
			f.close()

	def export(self, filename):
		root = xml.Element("project")
		root.set("name", self.name)

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
	return load_project_from_xml(root)

def load_project_from_xml(root):
	project = Project(utils.xml_str(root,"name"))
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
	#p = load_project("test1.proj")
	#p.export("export.xml")
	#return p

	project = Project("empty")
	net = Net(project)
	project.set_net(net)
	return project

