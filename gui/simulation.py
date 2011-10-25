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
from project import load_project_from_xml
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
		self.process_count = utils.xml_int(header, "process-count")
		lines_count = utils.xml_int(header, "description-lines")
		project_string = "\n".join((stream.readline() for i in xrange(lines_count)))
		self.project = load_project_from_xml(xml.fromstring(project_string), "")

	def get_instances(self):
		return self.instances

	def query_reports(self, callback = None):
		def reports_callback(line):
			root = xml.fromstring(line)
			instances = {}
			for e in root.findall("process"):
				process_id = utils.xml_int(e, "id")
				process_id_str = str(process_id)
				self.running = utils.xml_bool(e, "running")
				if not self.running:
					self.emit_event("error", "Network terminated\n")
				for ne in e.findall("net-instance"):
					id = utils.xml_int(ne, "id")
					i = instances.get(id)
					if i is None:
						net = self.project.find_net(utils.xml_int(ne, "net-id"))
						i = NetInstance(id, net, self)
						instances[id] = i
					for pe in ne.findall("place"):
						place_id = utils.xml_int(pe, "id")
						p = i.get_place(place_id)
						for te in pe.findall("token"):
							p.append(Token(te.get("value"), process_id_str))
					for tre in ne.findall("enabled"):
						transition_id = utils.xml_int(tre, "id")
						i.add_enabled(process_id, transition_id)
			self.instances = instances.values()
			if callback:
				callback()
			self.emit_event("changed")
		self.controller.run_command("REPORTS", reports_callback)

	def fire_transition(self, transition, instance, process_id):
		if not self.running:
			return
		if self.controller:
			command = "FIRE {0} {1} {2}".format(transition.get_id(), instance.get_id(), process_id)
			self.controller.run_command_expect_ok(command)
			self.query_reports()

class Token:
	
	def __init__(self, value, addr):
		self.value = value
		self.addr = addr

	def __str__(self):
		return self.value + "@" + self.addr

class Perspective(utils.EqMixin):

	def __init__(self, name, instance):
		self.name = name
		self.instance = instance

	def get_name(self):
		return self.name

class PerspectiveProcess(Perspective):

	def __init__(self, name, instance, process_id):
		Perspective.__init__(self, name, instance)
		self.process_id = process_id

	def get_tokens(self, place):
		return [ t for t in self.instance.get_place(place.get_id()) if t.addr == str(self.process_id) ]

	def is_enabled(self, transition):
		return self.instance.is_enabled(self.process_id, transition.get_id())

	def fire_transition(self, transition):
		self.instance.simulation.fire_transition(transition, self.instance, self.process_id)

	def __eq__(self, other):
		return (isinstance(other, self.__class__)
			and self.process_id == other.process_id)

	def __ne__(self, other):
		return not self.__eq__(other)

class PerspectiveAll(Perspective):

	def get_tokens(self, place):
		return self.instance.get_place(place.get_id())

	def get_enabled(self, transition):
		return [ i for i in xrange(self.instance.simulation.process_count)
			if self.instance.is_enabled(i, transition.get_id()) ]

	def is_enabled(self, transition):
		return len(self.get_enabled(transition)) > 0

	def fire_transition(self, transition):
		enabled = self.get_enabled(transition)
		if len(enabled) > 0:
			process_id = self.instance.simulation.random.choice(enabled)
			self.instance.simulation.fire_transition(transition, self.instance, process_id)

class NetInstance:

	def __init__(self, id, net, simulation):
		self.id = id
		self.net = net
		self.simulation = simulation
		self.places = {}
		self.enabled = []

	def get_id(self):
		return self.id

	def get_place(self, place_id):
		p = self.places.get(place_id)
		if p is None:
			p = []
			self.places[place_id] = p
		return p

	def get_perspectives(self):
		perspectives = [ PerspectiveProcess(str(i), self, i) for i in xrange(self.simulation.process_count) ]
		return [ PerspectiveAll("All", self) ] + perspectives

	def add_enabled(self, process_id, transition_id):
		self.enabled.append((process_id, transition_id))

	def get_name(self):
		return "Net"

	def is_enabled(self, process_id, transition_id):
		return (process_id, transition_id) in self.enabled
