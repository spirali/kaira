import xml.etree.ElementTree as xml

import project
import simulation
import utils


class LogFrame:

	def __init__(self, time, place_content):
		self.time = time
		self.place_content = place_content

	def get_tokens(self, place, iid = None):
		if iid is None:
			return self.place_content[place.get_id()]
		else:
			return self.place_content[place.get_id()][iid]

	def get_time(self):
		return self.time


class DebugLog:

	def __init__(self, filename):
		self.load(filename)

	def load(self, filename):
		with open(filename,"r") as f:
			f.readline() # Skip first line
			settings = xml.fromstring(f.readline())
			lines_count = int(settings.get("description-lines"))
			process_count = int(settings.get("process-count"))
			proj = xml.fromstring("\n".join([ f.readline() for i in xrange(lines_count) ]))
			self.project, translatetable = project.load_project_from_xml(proj, "")

			place_content = {}
			areas_instances = {}

			for process_id in xrange(process_count):
				report = xml.fromstring(f.readline())
				pc, transitions, ai = simulation.extract_report(report)
				place_content = utils.join_dicts(pc, place_content, utils.join_dicts)
				areas_instances = utils.join_dicts(ai, areas_instances, lambda x,y: x + y)

			self.areas_instances = areas_instances
			self.frame = LogFrame(0,place_content)

	def get_area_instances_number(self, area):
		return len(self.areas_instances[area.get_id()])

	def get_instance_node(self, area, iid):
		for i, node, running in self.areas_instances[area.get_id()]:
			if iid == i:
				return node
