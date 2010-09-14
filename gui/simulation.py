
import xml.etree.ElementTree as xml
import process
import utils
from events import EventSource

class Simulation(EventSource):
	"""
		Events: changed
	"""

	def __init__(self, project):
		EventSource.__init__(self)
		self.project = project
		self.process = process.CommandProcess("../out/project")
		self.process.start( ["-msim"] )
		self.query_reports()

	def shutdown(self):
		self.process.shutdown()

	def get_net(self):
		return self.project.net

	def query_reports(self):
		def reports_callback(line):
			self._process_report(xml.fromstring(line))
		self.process.run_command("REPORTS", reports_callback)

	def _process_report(self, root):
		places = {}
		transitions = {}
		for node_e in root.findall("node"):
			iid = utils.xml_int(node_e,"iid")
			for place_e in node_e.findall("place"):
				tokens = self._process_tokens(place_e, iid)
				place_id = utils.xml_int(place_e,"id")
				lst = places.setdefault(place_id, [])
				lst += tokens
			for transition_e in node_e.findall("transition"):
				transition_id = utils.xml_int(transition_e, "id")
				transitions.setdefault(transition_id,[])
				if utils.xml_bool(transition_e, "enable"):
					transitions[transition_id].append(iid)

		for p in places:
			self.get_net().set_tokens(p, places[p])

		for t in transitions:
			self.get_net().set_enable(t, len(transitions[t]) > 0)

		self.emit_event("changed")

	def _process_tokens(self, place_e, iid):
		return [ e.get("value") + "@" + str(iid) for e in place_e.findall("token") ]