#
#    Copyright (C) 2012 Stanislav Bohm,
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

    def  get_index_from_time(self,  time):
        last  =  len(self.timeline)  -  1
        first  =  0
        while  1:
            if  last  <  first:
                return last
            i  =  (last  +  first)  /  2
            t = self.get_event_time(i)
            if time  >  t:
                first  =  i  +  1
            elif  time  <  t:
                last  =  i  -  1

    def _preprocess(self):
        # Set time offsets
        starttime = min([ trace.get_init_time() for trace in self.traces ])
        for trace in self.traces:
            trace.time_offset = trace.get_init_time() - starttime

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
            if trace.is_next_event_visible():
                timeline.append(EventPointer(minimal_time_index, trace.pointer))

            trace.process_event(ri)
            trace_times[minimal_time_index] = trace.get_next_event_time()

        self.timeline = timeline
        self.bigtable = ri
        transition_names, transition_values = ri.get_transitions_utilization()
        tokens_names, tokens_values = ri.get_tokens_counts()
#        tr_tsum_names, tr_tsum_values = ri.get_transitions_time_sum()
#        proc_tsum_names, proc_tsum_values = self._make_processes_time_sum(ri.threads_data)
        proc_hist_names, proc_hist_values = self._make_processes_his(ri.threads_data)
        trans_gthreads_names, trans_gthreads_values = ri.get_transitions_utilization_gthreads()
        self.statistics = {
            "threads" : ri.threads_data,
            "transition_names" : transition_names,
            "transition_values" : transition_values,
            "tokens_names" : tokens_names,
            "tokens_values" : tokens_values,
            "proc_hist_names" : proc_hist_names,
            "proc_hist_values" : proc_hist_values,
#            "proc_tsum_names" : proc_tsum_names,
#            "proc_tsum_values" : proc_tsum_values,
#            "tr_tsum_names" : tr_tsum_names,
#            "tr_tsum_values" : tr_tsum_values,
            "trans_gthreads_names" : trans_gthreads_names,
            "trans_gthreads_values" : trans_gthreads_values
        }

    def _make_processes_his(self, data):
        names = []
        for p in range(self.process_count):
            for t in range(self.threads_count):
                names.append("process {0}`{1}".format(p, t))

        values = []
        for thread in data:
            hist = dict()
            for times in thread:
                t = times[1]
                if t in hist:
                    hist[t] += 1
                else:
                    hist[t] = 1
            values.append(hist)

        return names, values

#    def _make_processes_time_sum(self, data):
#        names =  ["process {0}".format(p) for p in xrange(self.process_count)]
#        values = []
#        for process in data:
#            sum = 0
#            for thread in process:
#                for times in thread:
#                    sum += (times[1] - times[0])
#            values.append(sum)
#
#        return names, values

class Trace:

    struct_basic = struct.Struct("<Q")
    struct_transition_fired = struct.Struct("<Qi")
    struct_spawn = struct.Struct("<Qi")
    struct_send = struct.Struct("<Qi")
    struct_receive = struct.Struct("<Qi")

    struct_token_4 = struct.Struct("<Li")
    struct_token_8 = struct.Struct("<Qi")

    struct_int = struct.Struct("<i")
    struct_double = struct.Struct("<d")

    def __init__(self, data, process_id, thread_id, pointer_size):
        self.data = data
        self.pointer = 0
        self.process_id = process_id
        self.thread_id = thread_id
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
        """ Return name of event as 5-character string """
        t = self.data[self.pointer]
        return t != "I" and t != "M"

    def get_next_event_name(self):
        """ Return name of event as 5-character string """
        t = self.data[self.pointer]
        if t == "T":
            return "Fired"
        elif t == "F":
            return "Fin  "
        elif t == "M":
            return "Send "
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
        elif t == "H" or t == "Q": # "H" for backward compatability
            return self._process_event_quit(runinstance)
        else:
            raise Exception("Invalid event type '{0}/{1}'".format(t, ord(t)))

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
            elif t == "M": # We want to skip SEND event in tracelog
                self.pointer += 1
                self._process_event_send_msg(runinstance)
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
            elif t == "M": # We want to skip SEND event in tracelog
                self.pointer += 1
                self._process_event_send_msg(runinstance)
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

    def _read_struct_send_msg(self):
        values = self.struct_send.unpack_from(self.data, self.pointer)
        self.pointer += self.struct_send.size
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
        time, transition_id = self._read_struct_transition_fired()
        pointer1 = self.pointer
        values = self._read_transition_trace_function_data()
        pointer2 = self.pointer
        self.pointer = pointer1
        runinstance.transition_fired(self.process_id,
                                     self.thread_id,
                                     time + self.time_offset,
                                     transition_id,
                                     values)
        self.process_tokens_remove(runinstance)
        self.pointer = pointer2
        self.process_tokens_add(runinstance)

    def _process_event_transition_finished(self, runinstance):
        time = self._read_struct_transition_finished()[0]
        runinstance.transition_finished(self.process_id,
                                        self.thread_id,
                                        time + self.time_offset)
        self.process_tokens_add(runinstance)

    def _process_event_send_msg(self, runinstance):
        time, msg_id = self._read_struct_send_msg()
        runinstance.event_send(self.process_id,
                               self.thread_id,
                               time + self.time_offset,
                               msg_id)

    def _process_event_spawn(self, runinstance):
        time, net_id = self._read_struct_spawn()
        runinstance.event_spawn(self.process_id,
                                self.thread_id,
                                time + self.time_offset,
                                net_id)
        self.process_tokens_add(runinstance)

    def _process_event_quit(self, runinstance):
        time = self._read_struct_quit()[0]
        runinstance.event_quit(self.process_id,
                               self.thread_id,
                               time + self.time_offset)
        self.process_tokens_add(runinstance)

    def _process_event_receive(self, runinstance):
        time, msg_id = self._read_struct_receive()
        send_time = runinstance.event_receive(self.process_id,
                                              self.thread_id,
                                              time + self.time_offset,
                                              msg_id)
        self.process_tokens_add(runinstance, send_time)

    def _process_event_idle(self, runinstance):
        time = self._read_struct_quit()[0]
        runinstance.event_idle(self.process_id,
                               self.thread_id,
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


class EventPointer:

    def __init__(self, trace_id, pointer):
        self.trace_id = trace_id
        self.pointer = pointer

    def execute(self, runinstance):
        self.tr

class EventInformation:

    def __init__(self, name, time, process, thread, transition, time_length):
        self.name = name
        self.time = time
        self.process = process
        self.thread =  thread
        self.transition = transition
        self.time_length = time_length

    def get_name(self):
        return self.name

    def get_time(self):
        return self.time

    def get_process(self):
        return self.process

    def get_thread(self):
        return self.thread

    def get_transition(self):
        return self.transition

    def get_time_length(self):
        return self.time_length

class DataCollectingRunInstance(RunInstance):

    def __init__(self, project, process_count, threads_count):
        RunInstance.__init__(self, project, process_count, threads_count)

        self.threads_data = [ [] for t in range(self.process_count * self.threads_count)]
        self.transitions_data = {} # [transition_id] -> [ (start_time, lenght) ]
        self.tokens_data = {} # [process_id][place_id] -> int
        self.group_nets = {}
        self.last_time = 0
        self.last_event_info = None
        self.tokens_info = dict()
        self.sender_info = dict()

        self.bigtable = {'action'          : [],
                         'previous_action' : [],
                         'time_start'      : [],
                         'snd_process'     : [],
                         'snd_thread'      : [],
                         'process'         : [],
                         'thread'          : [],
                         'transition_id'   : [],
                         'time_send'       : [],
                         'place_id'        : [],
                         'token_value'     : []}

    def add_entry(self, action, previous_action, time, snd_process, snd_thread, process, thread, transition, time_length, place_id, token_value):
        self.bigtable['action'].append(action)
        self.bigtable['previous_action'].append(previous_action)
        self.bigtable['time_start'].append(time)
        self.bigtable['snd_process'].append(snd_process)
        self.bigtable['snd_thread'].append(snd_thread)
        self.bigtable['process'].append(process)
        self.bigtable['thread'].append(thread)
        self.bigtable['transition_id'].append(transition)
        self.bigtable['time_send'].append(time_length)
        self.bigtable['place_id'].append(place_id)
        self.bigtable['token_value'].append(token_value)

    def get_entry(self, idx):
        action      = self.bigtable['action'][idx]
        previous_action = self.bigtable['previous_action'][idx]
        time        = self.bigtable['time_start'][idx]
        snd_process = self.bigtable['snd_process'][idx]
        snd_thread  = self.bigtable['snd_thread'][idx]
        process     = self.bigtable['process'][idx]
        thread      = self.bigtable['thread'][idx]
        transition  = self.bigtable['transition_id'][idx]
        time_length = self.bigtable['time_send'][idx]
        place_id    = self.bigtable['place_id'][idx]
        token_value = self.bigtable['token_value'][idx]

        return (action, previous_action, time, snd_process, snd_thread, process, thread, transition, time_length, place_id, token_value)

    def add_transition_data(self, process_id, thread_id, transition_id, value):
        if transition_id not in self.transitions_data:
            transition_data = [[] for t in range(self.process_count * self.threads_count)]
            self.transitions_data[transition_id] = transition_data

        transition_data = self.transitions_data[transition_id] # [[],[],[], ...]
        lst = transition_data[process_id * self.threads_count + thread_id]
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

    def transition_fired(self, process_id, thread_id, time, transition_id, values):
        RunInstance.transition_fired(self, process_id, thread_id, time, transition_id, values)
        self.last_time = time
        self.last_event_info = EventInformation(
            "_transition_fired", time, process_id, thread_id, transition_id, "")

    def transition_finished(self, process_id, thread_id, time):
        time_start = self.activites[process_id * self.threads_count + thread_id].time
        RunInstance.transition_finished(self, process_id, thread_id, time)
        self.last_time = time
        activity = self.last_event_activity
        if activity.transition.has_code():
            value = (time_start, time - time_start)
            self.threads_data[activity.process_id * self.threads_count + activity.thread_id].append(value)
            self.add_transition_data(process_id, thread_id, activity.transition.id, value)

            self.add_entry(
                "transition_executed", self.last_event_info.get_name(), time_start, process_id, thread_id, process_id, thread_id,
                activity.transition.id, (time - time_start), "", "")
            self.last_event_info = EventInformation(
                "_transition_finished", time, process_id, thread_id,
                activity.transition.id, (time-time_start))

    def event_spawn(self, process_id, thread_id, time, net_id):
        RunInstance.event_spawn(self, process_id, thread_id, time, net_id)
        self.last_time = time
        self.last_event_info = EventInformation(
            "_event_spawn", time, process_id, thread_id, "", "")

    def event_quit(self, process_id, thread_id, time):
        RunInstance.event_quit(self, process_id, thread_id, time)
        self.last_time = time
        self.last_event_info = EventInformation(
            "_event_quit", time, process_id, thread_id, "", "")

    def event_send(self, process_id, thread_id, time, msg_id):
        RunInstance.event_send(self, process_id, thread_id, time, msg_id)
        self.last_time = time
        self.sender_info[msg_id] = (process_id, thread_id)
        self.last_event_info = EventInformation(
            "_event_send", time, process_id, thread_id, "", "")

    def event_receive(self, process_id, thread_id, time, msg_id):
        send_time = RunInstance.event_receive(self, process_id, thread_id, time, msg_id)
        self.last_time = time
        (snd_process, snd_thread) = self.sender_info.pop(msg_id, (-1, -1))
        self.add_entry(
            "send_receive", self.last_event_info.get_name(),
            time - send_time,
            snd_process, snd_thread,
            process_id, thread_id,
            "", send_time, "", "")

        self.last_event_info = EventInformation(
            "_event_send", time, process_id, thread_id, "", send_time)
        return send_time

    def add_token(self, place_id, token_pointer, token_value, send_time):
        RunInstance.add_token(self, place_id, token_pointer, token_value, send_time)
        net_instance = self.last_event_instance
        self.change_tokens_data(net_instance.process_id,
                                place_id, self.last_time, 1)

        previous_action = self.last_event_info.get_name()
        time = self.last_event_info.get_time()
        process = self.last_event_info.get_process()
        thread = self.last_event_info.get_thread()
        transition = self.last_event_info.get_transition()

        val = token_value[0] if token_value is not None else ""
        self.add_entry(
            "add_token", previous_action, time, process, thread, process, thread, transition,
            send_time, place_id, val)

    def remove_token(self, place_id, token_pointer):
        RunInstance.remove_token(self, place_id, token_pointer)
        net_instance = self.last_event_instance
        self.change_tokens_data(net_instance.process_id,
                                place_id, self.last_time, -1)

        previous_action = self.last_event_info.get_name()
        time = self.last_event_info.get_time()
        process = self.last_event_info.get_process()
        thread = self.last_event_info.get_thread()
        transition = self.last_event_info.get_transition()
        time_length = self.last_event_info.get_time_length()

        self.add_entry(
            "remove_token", previous_action, time, process, thread, process, thread, transition,
            time_length, place_id, "")

    def get_transitions_utilization_gthreads(self):
        dnames = {}
        dvalues = {}
        transition_ids = self.transitions_data.keys()

        for transition_id in transition_ids:
            transition_data = self.transitions_data[transition_id]
            net, item = self.project.get_net_and_item(transition_id)

            for process_id in range(self.process_count):
                process_data = []
                if process_id not in dvalues:
                    dvalues[process_id] = []
                if process_id not in dnames:
                    dnames[process_id] = []
                for thread_id in range(self.threads_count):
                    process_data.append(transition_data[process_id * self.threads_count + thread_id])

                threads_in_process = []
                for threads in process_data:
                    for thread in threads:
                        if thread is not None:
                            threads_in_process.append(thread)
                dvalues[process_id].append(threads_in_process)

                dnames[process_id].append("{0.name} {1.name}@{2}".format(
                    net,
                    item,
                    process_id))

        values = []         
        names = []
        for process_id in range(self.process_count):
            process_data = dvalues[process_id]
            names_data = dnames[process_id]
            for data, name in zip(process_data, names_data):
                values.append(data)
                names.append(name)

        return names, values

    def get_transitions_utilization(self):
        transition_ids = self.transitions_data.keys()
        values = []
        names = []

        for transition_id, transition_data in self.transitions_data.items():
           net, item = self.project.get_net_and_item(transition_id)
           for process_id in xrange(self.process_count):
               for thread_id in xrange(self.threads_count):
                   index = process_id * self.threads_count + thread_id
                   values.append(transition_data[index])
                   names.append("{0.name} {1.name}@{2}`{3}".format(net,
                                                                   item,
                                                                   process_id,
                                                                   thread_id))
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

    def get_transitions_time_sum(self):
        names = []
        values = []
        for process_id, p in self.transitions_data.items():
            for transition_id, lst in p.items():
                net, item = self.project.get_net_and_item(transition_id)
                names.append("{0.name}({0.id}) {1.name}@{2}".format(
                    net,
                    item,
                    process_id))
                sum = 0
                for times in lst:
                    sum += (times[1] - times[0])
                values.append(sum)
        return names, values

    def export(self, filename):
        f = open(filename, 'w')
        length = len(self.bigtable['action'])
        for i in range(length):
            ent = self.get_entry(i)
            if ent is not None:
                action, previous_action, time, snd_process, snd_thread, process, thread, transition, time_length, place_id, token_value = ent

                f.write("\"{0}\",\"{1}\",\"{2}\",\"{3}\",\"{4}\",\"{5}\",\"{6}\",\"{7}\",\"{8}\",\"{9}\",\"{10}\"\n".format(
                    action, previous_action, time, snd_process, snd_thread, process, thread, transition, time_length, place_id, token_value))
        f.close()
