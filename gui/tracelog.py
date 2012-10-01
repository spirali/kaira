#
#    Copyright (C) 2012 Stanislav Bohm
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
import utils
import loader
import struct
import os

from runinstance import RunInstance

zero_char = chr(0)


class TraceLog:

    def __init__(self, filename):
        self.filename = filename

        self._read_header()

        self.traces = [None] * (self.process_count * self.threads_count)

        for process_id in xrange(self.process_count):
            for thread_id in xrange(self.threads_count):
                self._read_thread_trace(process_id, thread_id)

        self._preprocess()

    def get_filename_without_ext(self):
        name, ext = os.path.splitext(self.filename)
        return name

    def get_event_runinstance(self, index):
        ri = self.first_runinstance.copy()
        for i in xrange(index):
            event_pointer = self.timeline[i]
            trace = self.traces[event_pointer.trace_id]
            trace.pointer = event_pointer.pointer
            trace.process_event(ri)
        return ri

    def get_event_thread(self, index):
        if index == 0:
            return "X"
        index -= 1
        event_pointer = self.timeline[index]
        return event_pointer.trace_id % self.threads_count

    def get_event_process(self, index):
        if index == 0:
            return "X"
        index -= 1
        event_pointer = self.timeline[index]
        return event_pointer.trace_id / self.threads_count

    def get_event_time(self, index):
        if index == 0:
            return 0
        index -= 1
        event_pointer = self.timeline[index]
        trace = self.traces[event_pointer.trace_id]
        trace.pointer = event_pointer.pointer
        return trace.get_next_event_time()

    def get_event_name(self, index):
        if index == 0:
            return "Init "
        index -= 1
        event_pointer = self.timeline[index]
        trace = self.traces[event_pointer.trace_id]
        trace.pointer = event_pointer.pointer
        return trace.get_next_event_name()

    def get_runinstances_count(self):
        return len(self.timeline) + 1

    def get_max_time(self):
        return self.get_event_time(len(self.timeline))

    def _read_header(self):
        with open(self.filename, "r") as f:
            header = xml.fromstring(f.readline())
            self.pointer_size = utils.xml_int(header, "pointer-size")
            self.process_count = utils.xml_int(header, "process-count")
            self.threads_count = utils.xml_int(header, "threads-count")
            x = xml.fromstring(f.read())
            self.project = loader.load_project_from_xml(x, "")

    def _read_thread_trace(self, process_id, thread_id):
        filename = "{0}-{1}-{2}.ktt".format(
            self.get_filename_without_ext(),
            process_id,
            thread_id)
        with open(filename, "rb") as f:
            trace = Trace(f.read(), process_id, thread_id, self.pointer_size)
            self.traces[process_id * self.threads_count + thread_id] = trace

    def _preprocess(self):
        timeline = []
        trace_times = [ trace.get_next_event_time() for trace in self.traces ]

        ri = DataCollectingRunInstance(self.project, self.process_count, self.threads_count)

        self.first_runinstance = RunInstance(self.project, self.process_count, self.threads_count)
        timeline = []

        while True:

            # Searching for trace with minimal event time
            minimal_time_index = utils.index_of_minimal_value(trace_times)

            if minimal_time_index is None:
                break

            minimal_time = trace_times[minimal_time_index]
            trace = self.traces[minimal_time_index]

            # Timeline update
            timeline.append(EventPointer(minimal_time_index, trace.pointer))

            trace.process_event(ri)
            trace_times[minimal_time_index] = trace.get_next_event_time()

        self.timeline = timeline
        transition_names, transition_values = ri.get_transitions_utilization()
        tokens_names, tokens_values = ri.get_tokens_counts()
        self.statistics = {
            "threads" : ri.threads_data,
            "transition_names" : transition_names,
            "transition_values" : transition_values,
            "tokens_names" : tokens_names,
            "tokens_values" : tokens_values,

        }


class Trace:

    struct_basic = struct.Struct("<Q")
    struct_transition_fired = struct.Struct("<Qi")
    struct_spawn = struct.Struct("<Qi")
    struct_receive = struct_basic

    struct_token_4 = struct.Struct("<iL")
    struct_token_8 = struct.Struct("<iQ")

    def __init__(self, data, process_id, thread_id, pointer_size):
        self.data = data
        self.pointer = 0
        self.process_id = process_id
        self.thread_id = thread_id
        if pointer_size == 4:
            self.struct_token = self.struct_token_4
        elif pointer_size == 8:
            self.struct_token = self.struct_token_8
        else:
            Exception("Invalid pointer size")
        self.info = self._read_header()

    def get_next_event_name(self):
        t = self.data[self.pointer]
        if t == "T":
            return "Fired"
        elif t == "F":
            return "Fin  "
        elif t == "R":
            return "Recv "
        elif t == "S":
            return "Spawn"
        elif t == "Q":
            return "Quit "

    def process_event(self, runinstance):
        t = self.data[self.pointer]
        self.pointer += 1
        if t == "T":
            self._process_event_transition_fired(runinstance)
        elif t == "F":
            self._process_event_transition_finished(runinstance)
        elif t == "R":
            return self._process_event_receive(runinstance)
        elif t == "S":
            return self._process_event_spawn(runinstance)
        elif t == "Q":
            return self._process_event_quit(runinstance)
        else:
            raise Exception("Invalid event type '{0}/{1}'".format(t, ord(t)))

    def is_pointer_at_end(self):
        return self.pointer >= len(self.data)

    def process_tokens_add(self, runinstance):
        while not self.is_pointer_at_end():
            t = self.data[self.pointer]
            if t == "t":
                self.pointer += 1
                place_id, token_pointer = self._read_struct_token()
                token_value = self._read_cstring()
                runinstance.add_token(place_id, token_pointer, token_value)
            else:
                break

    def process_tokens_remove(self, runinstance):
        while not self.is_pointer_at_end():
            t = self.data[self.pointer]
            if t == "s":
                self.pointer += 1
                place_id, token_pointer = self._read_struct_token()
                runinstance.remove_token(place_id, token_pointer)
            else:
                break

    def get_next_event_time(self):
        if self.is_pointer_at_end():
            return None
        else:
            return self.struct_basic.unpack_from(self.data, self.pointer + 1)[0]

    def _read_header(self):
        info = {}
        while True:
            key = self._read_cstring()
            value = self._read_cstring()
            if key == "" and value == "":
                if "KairaThreadTrace" not in info or info["KairaThreadTrace"] != "1":
                    raise Exception("Invalid format or version of KairaThreadTrace")
                return info
            info[key] = value

    def _read_struct_transition_fired(self):
        values = self.struct_transition_fired.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_transition_fired.size
        return values

    def _read_struct_transition_finished(self):
        values = self.struct_basic.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_basic.size
        return values

    def _read_struct_receive(self):
        values = self.struct_receive.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_receive.size
        return values

    def _read_struct_spawn(self):
        values = self.struct_spawn.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_spawn.size
        return values

    def _read_struct_quit(self):
        values = self.struct_basic.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_basic.size
        return values

    def _process_event_transition_fired(self, runinstance):
        values = self._read_struct_transition_fired()
        runinstance.transition_fired(self.process_id, self.thread_id, *values)
        self.process_tokens_remove(runinstance)
        self.process_tokens_add(runinstance)

    def _process_event_transition_finished(self, runinstance):
        time = self._read_struct_transition_finished()[0]
        runinstance.transition_finished(self.process_id, self.thread_id, time)
        self.process_tokens_add(runinstance)

    def _process_event_spawn(self, runinstance):
        runinstance.event_spawn(self.process_id, self.thread_id, *self._read_struct_spawn())
        self.process_tokens_add(runinstance)

    def _process_event_quit(self, runinstance):
        time = self._read_struct_transition_finished()[0]
        runinstance.event_quit(self.process_id, self.thread_id, time)
        self.process_tokens_add(runinstance)

    def _process_event_receive(self, runinstance):
        time = self._read_struct_receive()[0]
        runinstance.event_receive(self.process_id, self.thread_id, time)
        self.process_tokens_add(runinstance)

    def _read_struct_token(self):
        values = self.struct_token.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_token.size
        return values

    def _read_cstring(self):
        start = self.pointer
        while self.data[self.pointer] != zero_char:
            self.pointer += 1
        s = self.data[start:self.pointer]
        self.pointer += 1
        return s


class EventPointer:

    def __init__(self, trace_id, pointer):
        self.trace_id = trace_id
        self.pointer = pointer

    def execute(self, runinstance):
        self.tr


class DataCollectingRunInstance(RunInstance):

    def __init__(self, project, process_count, threads_count):
        RunInstance.__init__(self, project, process_count, threads_count)
        begin = (0, None)
        self.threads_data = [ [ [ begin ] for t in xrange(self.threads_count) ]
                              for p in xrange(self.process_count) ]
        self.transitions_data = {} # [process_id][transition_id] -> [ (time, color) ]
        self.tokens_data = {} # [process_id][place_id] -> int
        self.group_nets = {}
        self.last_time = 0

    def add_transition_data(self, process_id, transition_id, value):
        process = self.transitions_data.get(process_id)
        if process is None:
            process = {}
            self.transitions_data[process_id] = process
        lst = process.get(transition_id)
        if lst is None:
            lst = []
            process[transition_id] = lst
        lst.append(value)

    def change_tokens_data(self, process_id, place_id, time, change):
        process = self.tokens_data.get(process_id)
        if process is None:
            process = {}
            self.tokens_data[process_id] = process
        lst = process.get(place_id)
        if lst is None:
            process[place_id] = [ (time, change) ]
        elif lst[-1][0] == time:
            lst[-1] = (time, lst[-1][1] + change)
        else:
            lst.append((time, lst[-1][1] + change))

    def transition_fired(self, process_id, thread_id, time, transition_id):
        RunInstance.transition_fired(self, process_id, thread_id, time, transition_id)
        self.last_time = time
        if self.last_event_activity.transition.has_code():
            value = (time, 0)
            self.threads_data[process_id][thread_id].append(value)
            self.add_transition_data(process_id, transition_id, value)

    def transition_finished(self, process_id, thread_id, time):
        RunInstance.transition_finished(self, process_id, thread_id, time)
        self.last_time = time
        activity = self.last_event_activity
        if activity.transition.has_code():
            value = (time, None)
            self.threads_data[activity.process_id][activity.thread_id].append(value)
            self.transitions_data[process_id][activity.transition.id].append(value)

    def event_spawn(self, process_id, thread_id, time, net_id):
        RunInstance.event_spawn(self, process_id, thread_id, time, net_id)
        self.last_time = time

    def event_quit(self, process_id, thread_id, time):
        RunInstance.event_quit(self, process_id, thread_id, time)
        self.last_time = time

    def event_receive(self, process_id, thread_id, time):
        RunInstance.event_receive(self, process_id, thread_id, time)
        self.last_time = time

    def add_token(self, place_id, token_pointer, token_value):
        RunInstance.add_token(self, place_id, token_pointer, token_value)
        net_instance = self.last_event_instance
        self.change_tokens_data(net_instance.process_id,
                                place_id, self.last_time, 1)

    def remove_token(self, place_id, token_pointer):
        RunInstance.remove_token(self, place_id, token_pointer)
        net_instance = self.last_event_instance
        self.change_tokens_data(net_instance.process_id,
                                place_id, self.last_time, -1)

    def get_transitions_utilization(self):
        names = []
        values = []
        for process_id, p in self.transitions_data.items():
            for transition_id, lst in p.items():
                net, item = self.project.get_net_and_item(transition_id)
                names.append("{0.name}({0.id}) {1.name}@{2}".format(
                    net,
                    item,
                    process_id))
                values.append([lst])
        return names, values

    def get_tokens_counts(self):
        names = []
        values = []
        for process_id, p in self.tokens_data.items():
            for place_id, lst in p.items():
                net, item = self.project.get_net_and_item(place_id)
                names.append("{0.name} {1}@{2}".format(
                    net,
                    place_id,
                    process_id))
                values.append(lst)
        return names, values
