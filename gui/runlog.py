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

class LogNetworkInstance:

	def __init__(self, path):
		self.path = path
		self.tokens = {}

	def get_tokens(self, place):
		place_id = place.get_id()
		if self.tokens.has_key(place_id):
			return self.tokens[place_id]
		else:
			return []

	def add_unit(self, unit):
		for place_id in unit.places:
			self.tokens[place_id] = unit.places[place_id]

	def add_token(self, place_id, token_name):
		if self.tokens.has_key(place_id):
			self.tokens[place_id].append(token_name)
		else:
			self.tokens[place_id] = [ token_name ]

	def remove_token(self, place_id, token_name):
		self.tokens[place_id].remove(token_name)

	def clear_tokens(self, place_id):
		self.tokens[place_id] = []


class LogFrame:

	full_frame = True

	def __init__(self, time, instances, name):
		self.time = time
		self.instances = instances
		self.running = []
		self.started = []
		self.ended = []
		self.name = name

	def get_instances(self, path):
		if path is None:
			return self.instances.values()
		else:
			return (i for p, i in self.instances.items() if p == path)

	def get_tokens(self, place, path):
		instances = self.get_instances(path)
		return sum(( [ token + "@" + str(i.path) for token in i.get_tokens(place) ] for i in instances ), [])

	def get_time(self):
		return self.time

	def copy(self):
		return copy.deepcopy(self)

	def remove_token(self, path, place_id, token_name):
		self.instances[path].remove_token(place_id, token_name)

	def add_token(self, path, place_id, token_name):
		if self.instances.has_key(path):
			i = self.instances[path]
		else:
			i = LogNetworkInstance(path)
			self.instances[path] = i
		i.add_token(place_id, token_name)


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

		for path, transition_id in frame.started: # Reset input packing edges
			for edge in project.get_net().get_item(transition_id).edges_to(postprocess = True):
				if edge.is_packing_edge():
					frame.instances[path].clear_tokens(edge.from_item.get_id())
		return frame

	def parse_action(self, frame, action):
		action_type = action[0]
		if action_type == "A":
			path, place_id, token_name = action[1:].split(" ", 3)
			path = simulation.path_from_string(path)
			place_id = int(place_id)
			frame.add_token(path, place_id, token_name)

		if action_type == "R":
			path, place_id, token_name = action[1:].split(" ", 3)
			path = simulation.path_from_string(path)
			place_id = int(place_id)
			frame.remove_token(path, place_id, token_name)

		if action_type == "S":
			path, transition_id = action[1:].split(" ", 2)
			item = (simulation.path_from_string(path), int(transition_id))
			frame.running.append(item)
			frame.started.append(item)
			frame.name = "S"

		if action_type == "E":
			path, transition_id = action[1:].split(" ", 2)
			item = (simulation.path_from_string(path), int(transition_id))
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
			instances = {}
			for unit in units:
				if instances.has_key(unit.path):
					i = instances[unit.path]
				else:
					i = LogNetworkInstance(unit.path)
					instances[unit.path] = i
				i.add_unit(unit)
			frame = LogFrame(0, instances, "I")
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
			self.paths = [ path for path in frame.instances ]

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
