import xml.etree.ElementTree as xml

import project
import simulation
import utils
import copy

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

	def get_fullframe(self, idtable):
		return self


class LogFrameDiff:

	def __init__(self, time, prev, actions):
		self.time = time
		self.prev = prev
		self.actions = actions

	def get_fullframe(self, idtable):
		frame = copy.deepcopy(self.prev.get_fullframe(idtable))
		frame.time = self.time
		for action in self.actions.split("\n"):
			self.parse_action(frame, action, idtable)
		return frame

	def parse_action(self, frame, action, idtable):
		action_type = action[0]
		print action, self.time
		if action_type == "A":
			iid, place_id, token_name = action.split(" ", 3)
			iid = int(iid[1:])
			place_id = int(place_id)
			frame.place_content[idtable[place_id]][iid].append(token_name)
		if action_type == "R":
			iid, place_id, token_name = action.split(" ", 3)
			iid = int(iid[1:])
			place_id = int(place_id)
			frame.place_content[idtable[place_id]][iid].remove(token_name)


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
			self.project, idtable = project.load_project_from_xml(proj, "")

			place_content = {}
			areas_instances = {}

			for process_id in xrange(process_count):
				report = xml.fromstring(f.readline())
				pc, transitions, ai = simulation.extract_report(report)
				pc = utils.translate(idtable, pc)
				transitions = utils.translate(idtable, transitions)
				place_content = utils.join_dicts(pc, place_content, utils.join_dicts)
				areas_instances = utils.join_dicts(ai, areas_instances, lambda x,y: x + y)

			self.idtable = idtable
			self.areas_instances = areas_instances
			frame = LogFrame(0, place_content)
			self.frames = [ frame ]

			next_time = self.parse_time(f.readline())
			inf = float("inf")
			if next_time < inf:
				frame, next_time = self.load_frame_diff(f, frame, next_time)
				while next_time < inf:
					self.frames.append(frame)
					frame, next_time = self.load_frame_diff(f, frame, next_time)
				self.frames.append(frame)

	def frames_count(self):
		return len(self.frames)

	def parse_time(self, string):
		if string == "":
			return float("inf")
		else:
			return int(string)

	def load_frame_diff(self, f, prev, time):
		lines = []
		line = f.readline()
		while line and not line[0].isdigit():
			lines.append(line.strip())
			line = f.readline()
		return (LogFrameDiff(time, prev, "\n".join(lines)), self.parse_time(line))

	def get_area_instances_number(self, area):
		return len(self.areas_instances[area.get_id()])

	def get_instance_node(self, area, iid):
		for i, node, running in self.areas_instances[area.get_id()]:
			if iid == i:
				return node

	def get_frame(self, pos):
		return self.frames[pos].get_fullframe(self.idtable)
