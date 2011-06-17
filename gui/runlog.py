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
import copy
import os

CACHE_FRAME_PERIOD = 200

class LogFrame:

	full_frame = True

	def __init__(self, time, units, name):
		self.time = time
		self.units = units
		self.running = []
		self.started = []
		self.ended = []
		self.name = name

	def get_units(self, path):
		if path is None:
			return self.units
		else:
			return (unit for unit in self.units if unit.path == path)

	def get_tokens(self, place, path):
		units = (unit for unit in self.get_units(path) if unit.has_place(place))
		return sum(( [ token + "@" + str(unit.path) for token in unit.get_tokens(place) ] for unit in units ), [])

	def get_time(self):
		return self.time

	def copy(self):
		return copy.deepcopy(self)


class LogFrameDiff:

	full_frame = False

	def __init__(self, time, actions):
		self.time = time
		self.actions = actions

	def apply_on_frame(self, frame, project):
		frame.time = self.time
		frame.started = []
		frame.ended = []
		frame.blocked = []
		for action in self.actions.split("\n"):
			self.parse_action(frame, action)

		for transition_id, iid in frame.started: # Reset input packing edges
			for edge in project.get_net().get_item(transition_id).edges_to(postprocess = True):
				if edge.is_packing_edge():
					frame.place_content[edge.from_item.get_id()][iid] = []

		return frame

	def parse_action(self, frame, action):
		action_type = action[0]
		if action_type == "A":
			iid, place_id, token_name = action.split(" ", 3)
			iid = int(iid[1:])
			place_id = int(place_id)
			frame.place_content[place_id][iid].append(token_name)
		if action_type == "R":
			iid, place_id, token_name = action.split(" ", 3)
			iid = int(iid[1:])
			place_id = int(place_id)
			frame.place_content[place_id][iid].remove(token_name)

		if action_type == "S":
			iid, transition_id = action.split(" ", 2)
			iid = int(iid[1:])
			transition_id = int(transition_id)
			item = (transition_id, iid)
			frame.running.append(item)
			frame.started.append(item)
			frame.name = "S"

		if action_type == "E":
			iid, transition_id = action.split(" ", 2)
			iid = int(iid[1:])
			transition_id = int(transition_id)
			item = (transition_id, iid)
			if item in frame.running:
				frame.running.remove(item)
			frame.ended.append(item)
			frame.name = "E"

		if action_type == "C":
			frame.name = "R"

		if action_type == "T":
			args = action.split(" ")
			node = int(args[0][1:])
			frame.blocked += [ (node, int(t)) for t in args[1:] ]

	def get_time(self):
		return self.time


class Log:

	def __init__(self, filename):
		basename = os.path.basename(filename)
		self.name, ext = os.path.splitext(basename)
		self.load(filename)

	def get_name(self):
		return self.name

	def load(self, filename):
		with open(filename,"r") as f:
			f.readline() # Skip first line
			settings = xml.fromstring(f.readline())
			lines_count = int(settings.get("description-lines"))
			self.process_count = int(settings.get("process-count"))
			self.threads_count = int(settings.get("process-count"))
			proj = xml.fromstring("\n".join([ f.readline() for i in xrange(lines_count) ]))
			self.project = project.load_project_from_xml(proj, "")

			self.node_to_process = {}

			reports = [ xml.fromstring(f.readline()) for i in xrange(self.process_count) ]
			units = sum([[ simulation.Unit(e) for e in report.findall("unit") ] for report in reports], [])
			frame = LogFrame(0, units, "I")
			self.frames = [ frame.copy() ]

			next_time = self.parse_time(f.readline())
			inf = float("inf")
			if next_time < inf:
				diff, next_time = self.load_frame_diff(f, next_time)
				frame = diff.apply_on_frame(frame, self.project)
				while next_time < inf:
					if len(self.frames) % CACHE_FRAME_PERIOD == 0:
						self.frames.append(frame.copy())
					else:
						self.frames.append(diff)
					diff, next_time = self.load_frame_diff(f, next_time)
					frame = diff.apply_on_frame(frame, self.project)
				self.frames.append(diff)
			self.maxtime = self.frames[-1].get_time()
			self.paths = [ unit.path for unit in self.frames[-1].units ]

	def get_paths(self):
		return self.paths

	def frames_count(self):
		return len(self.frames)

	def parse_time(self, string):
		if string == "":
			return float("inf")
		else:
			return int(string)

	def load_frame_diff(self, f, time):
		lines = []
		line = f.readline()
		while line and not line[0].isdigit():
			lines.append(line.strip())
			line = f.readline()
		return (LogFrameDiff(time, "\n".join(lines)), self.parse_time(line))

	def get_frame(self, pos):
		frame = self.frames[pos]
		if frame.full_frame:
			return frame.copy()
		else:
			return frame.apply_on_frame(self.get_frame(pos - 1), self.project)

	def get_time_string(self, frame):
		maxtime = time_to_string(self.maxtime)
		return "{0:0>{1}}".format(time_to_string(frame.get_time()), len(maxtime))

	def get_mapping(self):
		return []

	def get_statistics(self):
		return {}

def time_to_string(nanosec):
	s = nanosec / 1000000000
	nsec = nanosec % 1000000000
	sec = s % 60
	minutes = (s / 60) % 60
	hours = s / 60 / 60
	return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)
