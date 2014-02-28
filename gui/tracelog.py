#
#    Copyright (C) 2012-2013 Stanislav Bohm,
#                       Martin Surkovsky
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
import controlseq

from table import Table
from runinstance import RunInstance
from exportri import ExportRunInstance, place_counter_name

zero_char = chr(0)

class TraceLog:

    def __init__(self, filename, export_data=False):
        self.filename = filename
        self.export_data = export_data
        self._read_header()

        self.traces = [None] * self.process_count
        for process_id in xrange(self.process_count):
            self._read_trace(process_id)

        self.first_runinstance = RunInstance(self.project, self.process_count)

        self._preprocess()

    def execute_visible_events(self, ri, from_event=0, to_event=None):
        if to_event is None:
            to_event = len(self.timeline)
        for i in xrange(from_event, to_event):
            event_pointer = self.timeline[i]
            trace = self.traces[event_pointer["process"]]
            trace.pointer = event_pointer["pointer"]
            trace.process_event(ri)
        return ri

    def execute_all_events(self, ri,from_event=0, to_event=None):
        if to_event is None:
            to_event = len(self.full_timeline)
        for i in xrange(from_event, to_event):
            event_pointer = self.full_timeline[i]
            trace = self.traces[event_pointer["process"]]
            trace.pointer = event_pointer["pointer"]
            trace.process_event(ri)
        return ri

    def get_event_runinstance(self, index):
        return self.execute_visible_events(
            self.first_runinstance.copy(), 0, index)

    def get_event_process(self, index):
        if index == 0:
            return "X"
        index -= 1
        event_pointer = self.timeline[index]
        return event_pointer["process"]

    def get_event_time(self, index):
        if index == 0:
            return 0
        index -= 1
        event_pointer = self.timeline[index]
        trace = self.traces[event_pointer["process"]]
        trace.pointer = event_pointer["pointer"]
        return trace.get_next_event_time()

    def get_event_name(self, index):
        if index == 0:
            return "Init "
        index -= 1
        event_pointer = self.timeline[index]
        trace = self.traces[event_pointer["process"]]
        trace.pointer = event_pointer["pointer"]
        return trace.get_next_event_name()

    def get_runinstances_count(self):
        return len(self.timeline) + 1

    def get_max_time(self):
        return self.get_event_time(len(self.timeline))

    def export_sequence(self, index):
        time = utils.time_to_string(self.get_event_time(index))
        name = "Tracelog upto {0}".format(time)
        sequence = controlseq.ControlSequence(name)
        ri = self.first_runinstance.copy()
        for i in xrange(index):
            event_pointer = self.timeline[i]
            trace = self.traces[event_pointer["process"]]
            trace.pointer = event_pointer["pointer"]
            trace.process_event(ri)
            if ri.last_event == "fire":
                sequence.add_transition_start(ri.last_event_process,
                                              ri.last_event_activity.transition.get_name())
            elif ri.last_event == "finish":
                sequence.add_transition_finish(ri.last_event_process)
            elif ri.last_event == "receive":
                sequence.add_receive(ri.last_event_process,
                                     ri.last_event_activity.origin_id)
        return sequence

    def _read_header(self):
        with open(self.filename, "r") as f:
            header = xml.fromstring(f.readline())
            self.pointer_size = utils.xml_int(header, "pointer-size")
            self.process_count = utils.xml_int(header, "process-count")
            x = xml.fromstring(f.read())
            self.project = loader.load_project_from_xml(x, "")

    def _read_trace(self, process_id):
        filename = "{0}-{1}-0.ktt".format(
            utils.trim_filename_suffix(self.filename),
            process_id)
        with open(filename, "rb") as f:
            trace = Trace(f.read(), process_id, self.pointer_size)
            self.traces[process_id] = trace

    def _preprocess(self):
        # Set time offsets
        starttime = min([ trace.get_init_time() for trace in self.traces ])
        for trace in self.traces:
            trace.time_offset = trace.get_init_time() - starttime
        trace_times = [ trace.get_next_event_time() for trace in self.traces ]

        if self.export_data:
            place_counters = [place_counter_name(p)
                              for p in self.project.nets[0].places()
                              if p.trace_tokens]

            ri = ExportRunInstance(
                self,
                [ t for t in self.project.nets[0].transitions() if t.trace_fire ],
                [ (p, i) for p in self.project.nets[0].places()
                         for i, tracing in enumerate(p.trace_tokens_functions)
                         if tracing.return_numpy_type != 'O' ],
                ExportRunInstance.basic_header + place_counters)
        else:
            ri = RunInstance(
                self.project, self.process_count)

        index = 0
        timeline = Table([("process", "<i4"), ("pointer", "<i4")], 100)
        full_timeline = Table([("process", "<i4"), ("pointer", "<i4")], 100)
        while True:

            # Searching for trace with minimal event time
            minimal_time_index = utils.index_of_minimal_value(trace_times)
            if minimal_time_index is None:
                break

            trace = self.traces[minimal_time_index]

            full_timeline.add_row((minimal_time_index, trace.pointer))

            # Timeline update
            if trace.is_next_event_visible():
                timeline.add_row(full_timeline[index])

            trace.process_event(ri)
            trace_times[minimal_time_index] = trace.get_next_event_time()

            index += 1

        self.data = Table([], 0)
        if self.export_data:
            self.data = ri.get_table()

        timeline.trim()
        full_timeline.trim()
        self.timeline, self.full_timeline = timeline, full_timeline


class Trace:

    struct_basic = struct.Struct("<Q")
    struct_transition_fired = struct.Struct("<Qi")
    struct_spawn = struct.Struct("<Qi")
    struct_send = struct.Struct("<QQii")
    struct_receive = struct.Struct("<Qi")

    struct_token_4 = struct.Struct("<Li")
    struct_token_8 = struct.Struct("<Qi")

    struct_int = struct.Struct("<i")
    struct_double = struct.Struct("<d")

    def __init__(self, data, process_id, pointer_size):
        self.data = data
        self.pointer = 0
        self.process_id = process_id
        self.time_offset = 0
        if pointer_size == 4:
            self.struct_token = self.struct_token_4
        elif pointer_size == 8:
            self.struct_token = self.struct_token_8
        else:
            Exception("Invalid pointer size")
        self.info = self._read_header()

    def get_init_time(self):
        s = self.info.get("inittime")
        if s is not None:
            return int(s)
        else:
            return 0

    def is_next_event_visible(self):
        t = self.data[self.pointer]
        return t != "I" and t != "M" and t != "N"

    def get_next_event_name(self):
        """ Return name of event as 5-character string """
        t = self.data[self.pointer]
        if t == "T":
            return "Fire "
        elif t == "F":
            return "Fin  "
        elif t == "M":
            return "Send "
        elif t == "N":
            return "MSend"
        elif t == "R":
            return "Recv "
        elif t == "S":
            return "Spawn"
        elif t == "I":
            return "Idle "
        elif t == "H" or t == "Q": # "H" for backward compatability
            return "Quit "

    def process_event(self, runinstance):
        t = self.data[self.pointer]
        self.pointer += 1
        runinstance.pre_event()
        if t == "T":
            self._process_event_transition_fired(runinstance)
        elif t == "F":
            self._process_event_transition_finished(runinstance)
        elif t == "R":
            return self._process_event_receive(runinstance)
        elif t == "S":
            return self._process_event_spawn(runinstance)
        elif t == "I":
            return self._process_event_idle(runinstance)
        elif t == "Q":
            # This is called only when transition that call ctx.quit is not traced
            self.pointer -= 1 # _process_event_quit expect the pointer at "Q"
            self._process_event_quit(runinstance)
        else:
            raise Exception("Invalid event type '{0}/{1}' (pointer={2}, process={3})"
                                .format(t, ord(t), hex(self.pointer), self.process_id))

    def is_pointer_at_end(self):
        return self.pointer >= len(self.data)

    def process_tokens_add(self, runinstance, send_time=0):
        place_id = None
        token_pointer = None
        values = []
        while not self.is_pointer_at_end():
            t = self.data[self.pointer]
            if t == "t":
                if place_id is not None:
                    runinstance.add_token(place_id, token_pointer, values, send_time)
                values = []
                self.pointer += 1
                token_pointer, place_id = self._read_struct_token()
            elif t == "i":
                self.pointer += 1
                value = self._read_struct_int()
                values.append(value)
            elif t == "d":
                self.pointer += 1
                value = self._read_struct_double()
                values.append(value)
            elif t == "s":
                self.pointer += 1
                value = self._read_cstring()
                values.append(value)
            elif t == "M":
                self.pointer += 1
                self._process_event_send(runinstance)
            else:
                if place_id is not None and token_pointer is not None:
                    runinstance.add_token(place_id, token_pointer, values, send_time)
                break

        if self.is_pointer_at_end() and place_id is not None:
            runinstance.add_token(place_id, token_pointer, values, send_time)

    def process_tokens_remove(self, runinstance):
        while not self.is_pointer_at_end():
            t = self.data[self.pointer]
            if t == "r":
                self.pointer += 1
                token_pointer, place_id = self._read_struct_token()
                runinstance.remove_token(place_id, token_pointer)
            elif t == "M":
                self.pointer += 1
                self._process_event_send(runinstance)
            else:
                break

    def get_next_event_time(self):
        if self.is_pointer_at_end():
            return None
        else:
            return self.struct_basic.unpack_from(self.data, self.pointer + 1)[0] + \
                   self.time_offset

    def _read_header(self):
        info = {}
        while True:
            key = self._read_cstring()
            value = self._read_cstring()
            if key == "" and value == "":
                break
            info[key] = value

        if "KairaThreadTrace" not in info or info["KairaThreadTrace"] != "1":
            raise Exception("Invalid format or version of KairaThreadTrace")
        return info

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

    def _read_struct_send(self):
        time, size, edge_id, count = self.struct_send.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_send.size
        values = [ self._read_struct_int() for i in xrange(count) ]
        return (time, size, edge_id, values)

    def _read_struct_spawn(self):
        values = self.struct_spawn.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_spawn.size
        return values

    def _read_struct_quit(self):
        values = self.struct_basic.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_basic.size
        return values

    def _process_end(self, runinstance):
        t = self.data[self.pointer]
        if t != "X":
            return
        self.pointer += 1
        values = self.struct_basic.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_basic.size
        runinstance.event_end(self.process_id, values[0] + self.time_offset)

    def _process_event_transition_fired(self, runinstance):
        time, transition_id = self._read_struct_transition_fired()
        pointer1 = self.pointer
        values = self._read_transition_trace_function_data()
        pointer2 = self.pointer
        self.pointer = pointer1
        runinstance.transition_fired(self.process_id,
                                     time + self.time_offset,
                                     transition_id,
                                     values)
        self.process_tokens_remove(runinstance)
        self.pointer = pointer2
        self._process_event_quit(runinstance)
        self.process_tokens_add(runinstance)
        self._process_end(runinstance)

    def _process_event_transition_finished(self, runinstance):
        time = self._read_struct_transition_finished()[0]
        runinstance.transition_finished(self.process_id,
                                        time + self.time_offset)
        self._process_event_quit(runinstance)
        self.process_tokens_add(runinstance)
        self._process_end(runinstance)

    def _process_event_send(self, runinstance):
        time, size, edge_id, target_ids = self._read_struct_send()
        for target_id in target_ids:
            runinstance.event_send(self.process_id,
                                   time + self.time_offset,
                                   target_id,
                                   size,
                                   edge_id)

    def _process_event_spawn(self, runinstance):
        time, net_id = self._read_struct_spawn()
        runinstance.event_spawn(self.process_id,
                                time + self.time_offset,
                                net_id)
        self.process_tokens_add(runinstance)

    def _process_event_quit(self, runinstance):
        t = self.data[self.pointer]
        if t != "Q":
            return
        self.pointer += 1
        time = self._read_struct_quit()[0]
        runinstance.event_quit(self.process_id,
                               time + self.time_offset)

    def _process_event_receive(self, runinstance):
        time, origin_id = self._read_struct_receive()
        send_time = runinstance.event_receive(self.process_id,
                                              time + self.time_offset,
                                              origin_id)
        self.process_tokens_add(runinstance, send_time)
        self._process_end(runinstance)

    def _process_event_idle(self, runinstance):
        time = self._read_struct_quit()[0]
        runinstance.event_idle(self.process_id,
                               time + self.time_offset)

    def _read_struct_token(self):
        values = self.struct_token.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_token.size
        return values

    def _read_struct_int(self):
        value = self.struct_int.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_int.size
        return value[0]

    def _read_struct_double(self):
        value = self.struct_double.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_double.size
        return value[0]

    def _read_cstring(self):
        start = self.pointer
        while self.data[self.pointer] != zero_char:
            self.pointer += 1
        s = self.data[start:self.pointer]
        self.pointer += 1
        return s

    def _read_transition_trace_function_data(self):
        values = []
        while not self.is_pointer_at_end():
            t = self.data[self.pointer]
            if t == "r":
                self.pointer += 1
                self._read_struct_token()
            elif t == "i":
                self.pointer += 1
                value = self._read_struct_int()
                values.append(value)
            elif t == "d":
                self.pointer += 1
                value = self._read_struct_double()
                values.append(value)
            elif t == "s":
                self.pointer += 1
                value = self._read_cstring()
                values.append(value)
            else:
                break
        return values
