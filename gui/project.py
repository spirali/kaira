from net import Net, load_net
import xml.etree.ElementTree as xml
import utils

class Project:
	
	def __init__(self, project_name):
		self.id_counter = 100
		self.name = project_name
		self.change_callback = lambda p: None

	def new_id(self):
		self.id_counter += 1
		return self.id_counter

	def set_net(self, net):
		self.net = net
		net.set_change_callback(self._net_changed)
		self.changed()

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

	def save(self, filename):
		root = xml.Element("project")
		root.set("name", self.name)

		xml_net = self.net.as_xml()
		root.append(xml_net)

		f = open(filename, "w")
		try:
			f.write(xml.tostring(root))
		finally:
			f.close()

	def export(self, filename):
		root = xml.Element("project")
		root.set("name", self.name)

		xml_nets = self.net.export_xml()
		for e in xml_nets:
			root.append(e)

		f = open(filename, "w")
		try:
			f.write(xml.tostring(root))
		finally:
			f.close()


def load_project(filename):
	doc = xml.parse(filename)
	root = doc.getroot()
	project = Project(utils.xml_str(root,"name"))
	project.set_net(load_net(root.find("net"), project))
	return project

def new_empty_project():
	#p = load_project("test1.proj")
	#p.export("export.xml")
	#return p

	project = Project("empty")
	net = Net(project)
	project.set_net(net)
	"""from net import Place, Transition, Arc
	place = Place(net, (50, 70))
	transition = Transition(net, (120, 230))
	net.add_item(place)
	net.add_item(transition)
	net.add_arc(place, transition, [(240, 10),(180, 180)])
	project.save("tp.xml")"""
	return project

