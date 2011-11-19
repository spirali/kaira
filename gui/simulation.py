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

import xml.etree.ElementTree as xml
import process
import random
from loader import load_project_from_xml
from events import EventSource

import utils

class SimulationException(Exception):
	pass

class Simulation(EventSource):
	"""
		Events: changed, inited, error, shutdown
	"""

	controller = None
	project = None
	process_count = None
	quit_on_shutdown = False

	def __init__(self):
		EventSource.__init__(self)
		self.random = random.Random()

	def connect(self, host, port):
		def connected(stream):
			self.controller = controller
			self.read_header(stream)
			self.query_reports(lambda: self.emit_event("inited"))
		connection = process.Connection(host, port, exit_callback = self.controller_exit, connect_callback = connected)
		controller = process.CommandWrapper(connection)
		controller.start()

	def controller_exit(self, message):
		if message:
			self.emit_event("error", message + "\n")

		if self.controller:
			self.emit_event("error", "Traced process terminated\n")

		self.controller = None

	def shutdown(self):
		if self.controller:
			if self.quit_on_shutdown:
				self.controller.run_command("QUIT", None)
			else:
				self.controller.run_command("DETACH", None)
		self.controller = None
		self.emit_event("shutdown")

	def read_header(self, stream):
		header = xml.fromstring(stream.readline())
		lines_count = int(header.get("description-lines"))
		project_string = "\n".join((stream.readline() for i in xrange(lines_count)))
		self.project = load_project_from_xml(xml.fromstring(project_string), "")

	def query_reports(self, callback = None):
		def reports_callback(line):
			root = xml.fromstring(line)
			self.running = utils.xml_bool(root, "running")
			if not self.running:
				self.emit_event("error", "Network terminated\n")
			self.instances = []
			for e in root.findall("net"):
				id = utils.xml_int(e, "id")
				net = self.project.find_net(utils.xml_int(e, "net-id"))
				i = NetInstance(id, net, self)
				self.instances.append(i)
				nodes = {}
				units = [ Unit(e) for e in e.findall("unit") ]
				i.set_units(units)
				for unit in units:
					node = nodes.get(unit.path)
					if node is None:
						node = NetNode(i, unit.path)
						nodes[unit.path] = node
					node.add_unit(unit)
				i.set_nodes(nodes)
			if callback:
				callback()
			self.emit_event("changed")
		self.controller.run_command("REPORTS", reports_callback)

	def fire_transition(self, transition, instance, path):
		if not self.running:
			return
		if self.controller:
			command = "FIRE {0} {1} {2}".format(transition.get_id(), instance.get_id(), path)
			self.controller.run_command_expect_ok(command)
			self.query_reports()

class Path:
	def __init__(self, items, absolute = True):
		self.items = tuple(items)
		self.absolute = absolute

	def __eq__(self, path):
		if not isinstance(path, Path):
			return False
		return self.absolute == path.absolute and self.items == path.items

	def __hash__(self):
		return hash(self.items)

	def __str__(self):
		start = "/" if self.absolute else "./"
		return start + "/".join(map(str,self.items))

	def __cmp__(self, path):
		c = cmp(len(self.items), len(path.items))
		if c == 0:
			return cmp(self.items, path.items)
		else:
			return c

def path_from_string(string):
    assert string != ""
    if string[-1] == "/":
        string = string[:-1]
    items = string.split("/")
    if items[0] == "":
        return Path(map(int, items[1:]))
    elif items[0] == ".":
        return Path(map(int, items[1:]), False)
    else:
        return Path(map(int, items), False)

class Unit:

	def __init__(self, unit_element):
		self.places = {}
		self.transitions = {}
		self.path = path_from_string(unit_element.get("path"))

		for place in unit_element.findall("place"):
			tokens = [ e.get("value") for e in place.findall("token") ]
			self.places[int(place.get("id"))] = tokens

		for t in unit_element.findall("transition"):
			self.transitions[int(t.get("id"))] = utils.xml_bool(t, "enabled")

	def has_place(self, place):
		return self.places.has_key(place.get_id())

	def get_tokens(self, place):
		return self.places[place.get_id()]

	def has_transition(self, transition):
		return self.transitions.has_key(transition.get_id())

	def is_enabled(self, transition):
		return self.transitions[transition.get_id()]

class NetNode:

	def __init__(self, instance, path):
		self.path = path
		self.units = []
		self.instance = instance

	def add_unit(self, unit):
		self.units.append(unit)

	def get_tokens(self, place):
		for u in self.units:
			if u.has_place(place):
				return u.get_tokens(place)
		return []

	def is_enabled(self, transition):
		for u in self.units:
			if u.has_transition(transition):
				return u.is_enabled(transition)
		return False

	def fire_transition(self, transition):
		self.instance.simulation.fire_transition(transition, self.instance, self.path)

class OverviewNode:

	path = None

	def __init__(self, instance, units):
		self.instance = instance
		self.units = units

	def get_tokens(self, place):
		tokens = []
		for u in self.units:
			if u.has_place(place):
				tokens += [ t + "@" + str(u.path) for t in u.get_tokens(place) ]
		return tokens

	def is_enabled(self, transition):
		for u in self.units:
			if u.has_transition(transition) and u.is_enabled(transition):
				return True
		return False

	def fire_transition(self, transition):
		units = [ u for u in self.units if u.has_transition(transition) and u.is_enabled(transition) ]
		if units:
			u = self.instance.simulation.random.choice(units)
			self.instance.simulation.fire_transition(transition, self.instance, u.path)

class NetInstance:

	def __init__(self, id, net, simulation):
		self.id = id
		self.net = net
		self.simulation = simulation

	def get_id(self):
		return self.id

	def set_nodes(self, nodes):
		self.nodes = nodes

	def set_units(self, units):
		self.units = units

	def running_paths(self):
		return self.nodes.keys()

	def get_node(self, path):
		return self.nodes.get(path)

	def get_overview(self):
		return OverviewNode(self, self.units)

	def get_name(self):
		return self.net.get_name()
