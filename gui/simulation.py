#
#    Copyright (C) 2010 Stanislav Bohm
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
from events import EventSource

class SimulationException(Exception):
	pass

class Simulation(EventSource):
	"""
		Events: changed, inited, output
	"""

	def __init__(self, project, param_values):
		EventSource.__init__(self)
		self.project = project
		self.enabled_transitions = {}
		self.areas_instances = {}
		self.places_content = {}
		self.random = random.Random()
		self.process = process.Process(project.get_executable_filename(),self._simulator_output)
		self.process.cwd = project.get_directory()
		# FIXME: Timeout

		other_params = [ "-p%s=%s" % (p,param_values[p]) for p in param_values ]
		first_line = self.process.start_and_get_first_line( ["-msim"] + other_params )
		try:
			port = int(first_line)
		except ValueError:
			raise SimulationException("Simulation failed: " + first_line)

		self.controller = process.CommandWrapper(process.Connection("localhost", port))
		self.controller.start()
		self.query_first_reports()

	def shutdown(self):
		self.process.shutdown()
		# Shutdown of the controller is not necessary because when the simulator is terminated then socket is closed

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

	def query_reports(self):
		def reports_callback(line):
			self._process_report(xml.fromstring(line))
		self.controller.run_command("REPORTS", reports_callback)

	def query_first_reports(self):
		def reports_callback(line):
			self._process_first_report(xml.fromstring(line))
		self.controller.run_command("REPORTS", reports_callback)

	def fire_transition(self, transition, iid):
		self.controller.run_command_expect_ok("FIRE " + str(transition.get_id()) + " " + str(iid))
		self.query_reports()

	def fire_transition_random_instance(self, transition):
		enabled_iids = self.enabled_instances_of_transition(transition)
		if len(enabled_iids) > 0:
			iid = self.random.choice(enabled_iids)
			self.fire_transition(transition, iid)

	def enabled_instances_of_transition(self, transition):
		return self.enabled_transitions[transition.get_id()]

	def _process_report(self, root):
		places_content, enabled_transitions, areas_instances_data = extract_report(root)
		self.enabled_transitions = enabled_transitions
		self.places_content = places_content
		for network_id in areas_instances_data:
			instanced_area = self.areas_instances[network_id]
			for iid, node, running in areas_instances_data[network_id]:
				instanced_area.set_running(iid, running)
		self.emit_event("changed")

	def _process_first_report(self, root):
		places_content, enabled_transitions, areas_instances_data = extract_report(root)
		self.enabled_transitions = enabled_transitions
		self.places_content = places_content
		self.areas_instances = {}
		for network_id in areas_instances_data:
			self.areas_instances[network_id] = InstancedArea(areas_instances_data[network_id])
		self.emit_event("inited")

	def _simulator_output(self, line):
		self.emit_event("output", "OUTPUT: " + line)
		return True

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
