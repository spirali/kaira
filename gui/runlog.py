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
import os

CACHE_FRAME_PERIOD = 200

class LogNetworkInstance:

    def __init__(self, path):
        self.path = path
        self.tokens = {}

    def get_tokens(self, place):
        place_id = place.get_id()
        tokens = self.tokens.get(place_id)
        if tokens:
            return tokens
        else:
            return ()

    def add_unit(self, unit):
        for place_id in unit.places:
            self.tokens[place_id] = unit.places[place_id]

    def add_token(self, place_id, token_name):
        t = self.tokens.get(place_id)
        if t:
            t.append(token_name)
        else:
            self.tokens[place_id] = [ token_name ]

    def remove_token(self, place_id, token_name):
        self.tokens[place_id].remove(token_name)

    def clear_tokens(self, place_id):
        self.tokens[place_id] = []


class LogFrame:

    full_frame = True

    def __init__(self, time, computing_node, instances, name):
        self.time = time
        self.computing_node = computing_node
        self.instances = instances
        self.running = []
        self.started = []
        self.ended = []
        self.name = name
        self.enabled = set()
        self.notenabled = set()

    def get_process_id(self, log):
        if self.computing_node is not None:
            return self.computing_node / log.threads_count

    def get_thread_id(self, log):
        if self.computing_node is not None:
            return self.computing_node % log.threads_count

    def get_instances(self, path):
        if path is None:
            return self.instances.values()
        else:
            return (i for p, i in self.instances.items() if p == path)

    def get_tokens(self, place, path):
        instances = self.get_instances(path)
        return sum(( [ token + "@" + str(i.path) for token in i.get_tokens(place) ] for i in instances ), [])

    def get_tokens_count(self, place, path):
        i = self.instances.get(path)
        if i is None:
            return 0
        else:
            return len(self.instances[path].get_tokens(place))

    def get_time(self):
        return self.time

    def copy(self):
        return copy.deepcopy(self)

    def remove_token(self, path, place_id, token_name):
        self.instances[path].remove_token(place_id, token_name)

    def add_token(self, path, place_id, token_name):
        i = self.instances.get(path)
        if i is None:
            i = LogNetworkInstance(path)
            self.instances[path] = i
        i.add_token(place_id, token_name)


class LogFrameDiff:

    full_frame = False

    """ computation_node = process_id * threads_count + thread_id """
    def __init__(self, time, computing_node, actions):
        self.time = time
        self.computing_node = computing_node
        self.actions = actions

    def apply_on_frame(self, frame, project):
        frame.time = self.time
        frame.computing_node = self.computing_node
        frame.started = []
        frame.ended = []
        frame.enabled = set()
        frame.notenabled = set()
        for action in self.actions.split("\n"):
            self.parse_action(frame, action)

        for path, transition_id in frame.started: # Reset input packing edges
            for edge in project.get_net().get_item(transition_id).edges_to(postprocess = True):
                if edge.is_packing_edge():
                    frame.instances[path].clear_tokens(edge.from_item.get_id())
        frame.notenabled = frame.notenabled.difference(frame.enabled)
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

        if action_type == "U":
            args = action[1:].split(" ")
            path = simulation.path_from_string(args[0])
            for i in args[1:]:
                transition_id = int(i[1:])
                p = (path, transition_id)
                if i[0] == "+":
                    frame.enabled.add(p)
                else:
                    frame.notenabled.add(p)

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
            self.threads_count = int(settings.get("threads-count"))
            proj = xml.fromstring("\n".join([ f.readline() for i in xrange(lines_count) ]))
            self.project = project.load_project_from_xml(proj, "")

            self.node_to_process = {}

            reports = [ xml.fromstring(f.readline()) for i in xrange(self.process_count) ]
            elements = sum([[ e for e in report.findall("unit") ] for report in reports ], [])
            units = [ simulation.Unit(e) for e in elements ]

            instances = {}
            for unit in units:
                i = instances.get(unit.path)
                if i is None:
                    i = LogNetworkInstance(unit.path)
                    instances[unit.path] = i
                i.add_unit(unit)
            frame = LogFrame(0, None, instances, "I")

            for e in elements:
                path = simulation.path_from_string(e.get("path"))
                for t in e.findall("transition"):
                    if utils.xml_bool(t, "enabled"):
                        frame.enabled.add((path, utils.xml_int(t, "id")))

            self.frames = [ frame.copy() ]

            next_time, computing_node = self.parse_timeblock(f.readline())
            inf = float("inf")
            if next_time < inf:
                diff, (next_time, comuping_node) = self.load_frame_diff(f, next_time, computing_node)
                frame = diff.apply_on_frame(frame, self.project)
                while next_time < inf:
                    if len(self.frames) % CACHE_FRAME_PERIOD == 0:
                        self.frames.append(frame.copy())
                    else:
                        self.frames.append(diff)
                    diff, (next_time, computing_node) = self.load_frame_diff(f, next_time, computing_node)
                    frame = diff.apply_on_frame(frame, self.project)
                self.frames.append(diff)
            self.maxtime = self.frames[-1].get_time()
            self.paths = [ path for path in frame.instances ]
            self.paths.sort()

    def get_paths(self):
        return self.paths

    def frames_count(self):
        return len(self.frames)

    def parse_timeblock(self, string):
        if string == "":
            return (float("inf"), -1)
        else:
            time, computing_node = string.split()
            return int(time), int(computing_node)

    def load_frame_diff(self, f, time, computing_node):
        lines = []
        line = f.readline()
        while line and not line[0].isdigit():
            lines.append(line.strip())
            line = f.readline()
        return (LogFrameDiff(time, computing_node, "\n".join(lines)), self.parse_timeblock(line))

    def get_frame(self, pos):
        frame = self.frames[pos]
        if frame.full_frame:
            return frame.copy()
        else:
            return frame.apply_on_frame(self.get_frame(pos - 1), self.project)

    def get_time_string(self, frame):
        maxtime = time_to_string(self.maxtime)
        return "{0:0>{1}}".format(time_to_string(frame.get_time()), len(maxtime))

    def process_transition_data(self, tdata):
        net = self.project.get_net()

        # Create utilization of transitions from tdata
        keys = tdata.keys()
        keys.sort(key=lambda x: x[0])
        transitions = []
        transitions_names = []
        for t in keys:
            working = []
            for time, value in tdata[t][0]:
                if value == 0:
                    new = None
                else:
                    new = value
                working.append((time, new))
            enabled = [ (time, 0 if value else None) for time, value in tdata[t][1] ]
            if len(working) > 1:
                transitions.append([enabled, working])
                transitions_names.append(net.get_item(t[1]).get_name() + "@" + str(t[0]))
        return {
            "transitions" : transitions,
            "transitions_names" : transitions_names,
        }

    def get_statistics(self):
        def transition_started(f, t):
            lst = tdata[t][0]
            lst.append((f.time, lst[-1][1] + 1))
        def transition_ended(f, t):
            lst = tdata[t][0]
            lst.append((f.time, lst[-1][1] - 1))
        def postprocess_pdata(v):
            time, value = v
            if value == 0:
                return (time, None)
            else:
                return (time, value - 1)

        net = self.project.get_net()
        path_places = [ (path, place) for path in self.paths for place in net.places() ]
        path_transitions = [ (path, transition) for path in self.paths for transition in net.transitions() ]
        tokens_names = [ "{0}@{1}".format(place.get_id(), path) for path, place in path_places ]
        tokens = [ [(0, self.frames[0].get_tokens_count(place, path))] for path, place in path_places ]
        tdata = {}
        pdata = [ [ [(0, None)] for j in xrange(self.threads_count) ] for i in xrange(self.process_count) ]
        for path, transition in path_transitions:
            t = (path, transition.get_id())
            tdata[t] = ([ (0, 0) ], [ (0, t in self.frames[0].enabled) ])

        for frame in self.frames:
            if frame.full_frame:
                f = frame.copy()
            else:
                f = frame.apply_on_frame(f, self.project)

            for i, (path, place) in enumerate(path_places):
                count = f.get_tokens_count(place, path)
                if count != tokens[i][-1][1]:
                    tokens[i].append((f.time, count))

            for t in f.started:
                transition_started(f, t)

            for t in f.ended:
                transition_ended(f, t)

            for t in f.notenabled:
                lst = tdata[t][1]
                if lst[-1][1]: # transition was enabled
                    lst.append((f.time, False))

            for t in f.enabled:
                lst = tdata[t][1]
                if not lst[-1][1]: # transition was not enabled
                    lst.append((f.time, True))

            n = f.computing_node
            if n is not None:
                lst = pdata[n / self.threads_count][n % self.threads_count]
                if f.started:
                    lst.append((f.time, 0))
                elif f.ended:
                    lst.append((f.time, None))


        # Remove the place from the table if there is only zero during all history
        for i, t in reversed(list(enumerate(tokens))):
            if t == [ (0,0) ]:
                del tokens[i]
                del tokens_names[i]

        statistics = {
            "tokens_names" : tokens_names,
            "tokens" : tokens,
            "processes_names" : [ "process " + str(i) for i in xrange(self.process_count) ],
            "processes" : pdata
        }

        return dict(statistics.items() + self.process_transition_data(tdata).items())

def time_to_string(nanosec):
    s = nanosec / 1000000000
    nsec = nanosec % 1000000000
    sec = s % 60
    minutes = (s / 60) % 60
    hours = s / 60 / 60
    return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)
