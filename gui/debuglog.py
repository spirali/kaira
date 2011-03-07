#
#    Copyright (C) 2011 Stanislav Bohm
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

import project
import simulation
import utils
import copy

class LogFrame:

	def __init__(self, time, place_content, name):
		self.time = time
		self.place_content = place_content
		self.running = []
		self.started = []
		self.ended = []
		self.name = name

	def get_tokens(self, place, iid = None):
		if iid is None:
			return self.place_content[place.get_id()]
		else:
			return self.place_content[place.get_id()][iid]

	def get_time(self):
		return self.time

	def get_fullframe(self, project, idtable):
		return self


class LogFrameDiff:

	def __init__(self, time, prev, actions):
		self.time = time
		self.prev = prev
		self.actions = actions

	def get_fullframe(self, project, idtable):
		frame = copy.deepcopy(self.prev.get_fullframe(project, idtable))
		frame.time = self.time
		frame.started = []
		frame.ended = []
		for action in self.actions.split("\n"):
			self.parse_action(frame, action, idtable)

		for transition_id, iid in frame.started: # Reset input packing edges
			for edge in project.get_net().get_item(transition_id).edges_to(split_bidirectional = True):
				if edge.is_packing_edge():
					frame.place_content[edge.from_item.get_id()][iid] = []

		return frame

	def parse_action(self, frame, action, idtable):
		action_type = action[0]
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

		if action_type == "S":
			iid, transition_id = action.split(" ", 2)
			iid = int(iid[1:])
			transition_id = int(transition_id)
			item = (idtable[transition_id], iid)
			frame.running.append(item)
			frame.started.append(item)
			frame.name = "S"

		if action_type == "E":
			iid, transition_id = action.split(" ", 2)
			iid = int(iid[1:])
			transition_id = int(transition_id)
			item = (idtable[transition_id], iid)
			if item in frame.running:
				frame.running.remove(item)
			frame.ended.append(item)
			frame.name = "E"

		if action_type == "C":
			frame.name = "R"

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
			frame = LogFrame(0, place_content, "I")
			self.frames = [ frame ]

			next_time = self.parse_time(f.readline())
			inf = float("inf")
			if next_time < inf:
				frame, next_time = self.load_frame_diff(f, frame, next_time)
				while next_time < inf:
					self.frames.append(frame)
					frame, next_time = self.load_frame_diff(f, frame, next_time)
				self.frames.append(frame)
			self.maxtime = self.frames[-1].get_time()

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
		return self.frames[pos].get_fullframe(self.project, self.idtable)

	def get_time_string(self, frame):
		maxtime = time_to_string(self.maxtime)
		return "{0:0>{1}}".format(time_to_string(frame.get_time()), len(maxtime))

def time_to_string(nanosec):
	s = nanosec / 1000000000
	nsec = nanosec % 1000000000
	sec = s % 60
	minutes = (s / 60) % 60
	hours = s / 60 / 60
	return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)
