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
import utils
import random
from project import load_project_from_xml
from events import EventSource

class SimulationException(Exception):
	pass

class Simulation(EventSource):
	"""
		Events: changed, inited, error, shutdown
	"""

	controller = None
	project = None
	process_count = None
	idtable = None
	quit_on_shutdown = False

	def __init__(self):
		EventSource.__init__(self)
		self.areas_instances = {}
		self.places_content = {}
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

	def get_net(self):
		return self.project.net

	def get_area_instances_number(self, area):
		return self.areas_instances[area.get_id()].number_of_instances()

	def get_instance_node(self, area, iid):
		return self.areas_instances[area.get_id()].get_node(iid)

	def is_instance_running(self, area, iid):
		return self.areas_instances[area.get_id()].is_running(iid)

	def is_transition_enabled(self, transition, iid):
		return iid in self.enabled_transitions[transition.get_id()]

	def get_tokens_of_place(self, place):
		return self.places_content[place.get_id()]

	def read_header(self, stream):
		header = xml.fromstring(stream.readline())
		lines_count = int(header.get("description-lines"))
		self.process_count = int(header.get("process-count"))
		project_string = "\n".join((stream.readline() for i in xrange(lines_count)))
		self.project, self.idtable = load_project_from_xml(xml.fromstring(project_string), "")

	def query_reports(self, callback = None):
		def reports_callback(lines):
			self.places_content, areas_instances_data, self.enabled_transitions, self.node_to_process = \
				join_reports(lines, self.idtable)
			for network_id in areas_instances_data:
					self.areas_instances[network_id] = InstancedArea(areas_instances_data[network_id])
			if callback:
				callback()
			self.emit_event("changed")
		self.controller.run_command("REPORTS", reports_callback, self.process_count)

	def fire_transition(self, transition, iid):
		if self.controller:
			self.controller.run_command_expect_ok("FIRE " + str(transition.get_id()) + " " + str(iid))
			self.query_reports()

	def fire_transition_random_instance(self, transition):
		enabled_iids = self.enabled_instances_of_transition(transition)
		if len(enabled_iids) > 0:
			iid = self.random.choice(enabled_iids)
			self.fire_transition(transition, iid)

	def enabled_instances_of_transition(self, transition):
		return self.enabled_transitions[transition.get_id()]


class InstancedArea:

	def __init__(self, data):
		self.instances = {}	
		for iid, node, running in data:
			self.instances[iid] = [node, running]

	def number_of_instances(self):
		return len(self.instances)

	def set_running(self, iid, value):
		self.instances[iid][1] = value

	def is_running(self, iid):
		return self.instances[iid][1]

	def get_node(self, iid):
		return self.instances[iid][0]

def extract_report(root):
	places_content = {}
	transitions = {}
	areas_instances = {}
	for node_e in root.findall("node"):
		area_id = utils.xml_int(node_e, "network-id")
		iid = utils.xml_int(node_e,"iid")
		node = utils.xml_int(node_e,"node")
		running = utils.xml_bool(node_e, "running")
		areas_instances.setdefault(area_id,[])
		areas_instances[area_id].append( (iid, node, running) )
		for place_e in node_e.findall("place"):
			tokens = [ utils.xml_str(e,"value") for e in place_e.findall("token") ]
			place_id = utils.xml_int(place_e,"id")
			place_content = places_content.setdefault(place_id, {})
			place_content[iid] = tokens
		for transition_e in node_e.findall("transition"):
			transition_id = utils.xml_int(transition_e, "id")
			transitions.setdefault(transition_id,[])
			if utils.xml_bool(transition_e, "enable") and running:
				transitions[transition_id].append(iid)
	return (places_content, transitions, areas_instances)

def join_reports(lines, idtable):
	place_content = {}
	areas_instances = {}
	node_to_process = {}
	transitions = {}

	for process_id, line in enumerate(lines):
		report = xml.fromstring(line)
		pc, tr, ai = extract_report(report)
		pc = utils.translate(idtable, pc)
		tr = utils.translate(idtable, tr)
		place_content = utils.join_dicts(pc, place_content, utils.join_dicts)
		areas_instances = utils.join_dicts(ai, areas_instances, lambda x,y: x + y)
		transitions = utils.join_dicts(tr, transitions, lambda x,y: x + y)
		for area in ai.values():
			for iid, node, running in area:
				node_to_process[node] = process_id

	return place_content, areas_instances, transitions, node_to_process
