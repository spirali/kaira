
import xml.etree.ElementTree as xml
import process
import utils
import random
from events import EventSource

class Simulation(EventSource):
	"""
		Events: changed
	"""

	def __init__(self, project):
		EventSource.__init__(self)
		self.project = project
		self.enabled_transitions = {}
		self.random = random.Random()
		self.process = process.Process("../out/project",self._simulator_output)
		# FIXME: Timeout
		port = int(self.process.start_and_get_first_line( ["-msim"] ))
		self.controller = process.CommandWrapper(process.Connection("localhost", port))
		self.controller.start()
		self.query_reports()

	def shutdown(self):
		self.process.shutdown()
		# Shutdown of the controller is not necessary because when the simulator is terminated then socket is closed

	def get_net(self):
		return self.project.net

	def query_reports(self):
		def reports_callback(line):
			self._process_report(xml.fromstring(line))
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
		places = {}
		transitions = {}
		for node_e in root.findall("node"):
			iid = utils.xml_int(node_e,"iid")
			running = utils.xml_bool(node_e, "running")
			for place_e in node_e.findall("place"):
				tokens = self._process_tokens(place_e, iid)
				place_id = utils.xml_int(place_e,"id")
				lst = places.setdefault(place_id, [])
				lst += tokens
			for transition_e in node_e.findall("transition"):
				transition_id = utils.xml_int(transition_e, "id")
				transitions.setdefault(transition_id,[])
				if utils.xml_bool(transition_e, "enable") and running:
					transitions[transition_id].append(iid)

		net = self.get_net()
		for p in places:
			net.set_tokens(p, places[p])

		for t in transitions:
			net.set_enable(t, len(transitions[t]) > 0)
		self.enabled_transitions = transitions
		self.emit_event("changed")

	def _process_tokens(self, place_e, iid):
		return [ e.get("value") + "@" + str(iid) for e in place_e.findall("token") ]

	def _simulator_output(self, line):
		self.emit_event("output", "OUTPUT: " + line)
		return True
