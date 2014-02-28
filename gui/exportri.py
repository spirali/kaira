#
#    Copyright (C) 2013 Stanislav Bohm
#                  2014 Martin Surkovsky
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

import settingswindow
from runinstance import RunInstance
from table import  Table
from gtk import RESPONSE_APPLY

class ExportRunInstance(RunInstance):

    basic_header = ["Event", "Time", "Duration", "Process", "ID"]

    def __init__(self, tracelog, transitions, place_functions, columns):
        RunInstance.__init__(self,
                             tracelog.project,
                             tracelog.process_count)

        self.transitions = dict((transition.id, transition)
                                for transition in transitions)

        self.place_functions = dict((place.id, (place, f_index))
                                    for place, f_index in place_functions)

        self.traced_places = dict((p.id, p)
                                  for p in tracelog.project.nets[0].places()
                                  if p.trace_tokens and
                                     place_counter_name(p) in columns)

        self.column_event = "Event" in columns
        self.column_time = "Time" in columns
        self.column_duration = "Duration" in columns
        self.column_process = "Process" in columns
        self.column_id = "ID" in columns
        self.column_value = bool(place_functions)
        self.column_tokens = bool(self.traced_places)

        self.table = self._create_table()

        self.idles = [None] * self.process_count
        self.tokens_counters = [[0] * len(self.traced_places)
                                for p in range(tracelog.process_count)]

    def _create_table(self):
        header = []
        types = [];
        if self.column_event:
            header.append("Event")
            types.append("|S1") # char
        if self.column_time:
            header.append("Time")
            types.append("<u8") # 64 bit unsigned integer
        if self.column_duration:
            header.append("Duration")
            types.append("<u8")
        if self.column_process:
            header.append("Process")
            types.append("<i4") # 32 bit integer
        if self.column_id:
            header.append("ID")
            types.append("<i4") # 32 bit integer
        if self.column_value:
            self.values_indexes = {}
            for index, place_id in enumerate(self.place_functions.keys()):
                place, f_index = self.place_functions[place_id]
                col_name = place_value_name(place, f_index)
                self.values_indexes[col_name] = index

                header.append(col_name)
                types.append(place.trace_tokens_functions[f_index].return_numpy_type)
        if self.column_tokens:
            self.tokens_indexes = {}
            for index, (id, place) in enumerate(self.traced_places.items()):
                col_name = place_counter_name(place)
                self.tokens_indexes[col_name] = index

                header.append(col_name)
                types.append('<i4')

        return Table(zip(header, types), 100)

    def add_row(self, event, time, duration, process, id, (col_name, value)):
        row = []
        if self.column_event:
            row.append(event)
        if self.column_time:
            row.append(time)
        if self.column_duration:
            row.append(duration)
        if self.column_process:
            row.append(process)
        if self.column_id:
            row.append(id)
        if self.column_value:
            values = [None] * len(self.place_functions)
            if event == 'A':
                values[self.values_indexes[col_name]] = value
            row += values
        if self.column_tokens:
            if event == 'C':
                couter_index = self.tokens_indexes[col_name]
                self.tokens_counters[process][couter_index] += value
            row += self.tokens_counters[process]

        self.table.add_row(row)

    def get_table(self):
        self.table.trim()
        return self.table

    # Collected events
    def transition_finished(self, process_id, time):
        activity = self.activites[process_id]
        if activity.transition.id in self.transitions:
            start_time = activity.time
            self.add_row('T',
                         start_time,
                         time - start_time,
                         process_id,
                         activity.transition.id,
                         (None, None))
        RunInstance.transition_finished(self, process_id, time)

    def event_receive(self, process_id, time, origin_id):
        start_time = self.idles[process_id]
        if start_time is not None:
            self.add_row('I',
                         start_time,
                         time - start_time,
                         process_id,
                         None,
                         (None, None))
            self.idles[process_id] = None
        RunInstance.event_receive(self, process_id, time, origin_id)

    def event_idle(self, process_id, time):
        self.idles[process_id] = time
        RunInstance.event_idle(self, process_id, time)

    def add_token(self, place_id, token_pointer, token_value, send_time):
        self._change_place_counter(place_id, +1)

        if place_id in self.place_functions:
            place, f_index = self.place_functions[place_id]
            self.add_row('A',
                         self.last_event_time,
                         None,
                         self.last_event_process,
                         place_id,
                         (place_value_name(place, f_index),
                          token_value[f_index]))
        RunInstance.add_token(
            self, place_id, token_pointer, token_value, send_time)

    def remove_token(self, place_id, token_pointer):
        self._change_place_counter(place_id, -1)
        RunInstance.remove_token(self, place_id, token_pointer)

    def _change_place_counter(self, place_id, change):
        if place_id in self.traced_places:
            place = self.traced_places[place_id]
            self.add_row('C',
                         self.last_event_time,
                         None,
                         self.last_event_process,
                         place_id,
                         (place_counter_name(place), change))


def place_value_name(place, f_index):
    return "V: ({0}/{1})".format(place.get_name_or_id(),
                                 place.trace_tokens_functions[f_index].name)

def place_counter_name(place):
    return "C: {0}".format(place.get_name_or_id())

def run_assistant(app, tracelog):
    assistant = settingswindow.BasicSettingAssistant(2,
                                                     "Export settings",
                                                     app.window)
    assistant.set_size_request(600, 600)
    def page_1(setting):
        w = settingswindow.SettingWidget()
        data = [ (t.get_name_or_id(), t, False)
                 for t in tracelog.project.nets[0].transitions() ]
        w.add_checkbuttons_list("transitions",
                                "Transitions",
                                data,
                                [ "Transition", "Export fire?" ])

        data = [ ("{0}/{1}".format(p.get_name_or_id(), f.name), (p, i), False)
                     for p in tracelog.project.nets[0].places()
                     for i, f in enumerate(p.trace_tokens_functions)
                     # only function that does not return PyObject values
                     if p.trace_tokens and f.return_numpy_type != 'O']

        w.add_checkbuttons_list("place_functions",
                                "Place\nFunctions",
                                data,
                                [ "Place/Function", "Export add token?" ])
        return w
    def page_2(setting):
        w = settingswindow.SettingWidget()
        data = [ ("Event", "Event", True),
                 ("Time", "Time", True),
                 ("Duration", "Duration", True),
                 ("Process", "Process", True),
                 ("ID", "ID", True) ]

        counters = [(place_counter_name(p), place_counter_name(p), False)
                    for p in tracelog.project.nets[0].places() if p.trace_tokens]

        data += counters
        w.add_checkbuttons("columns", "Columns", data)
        return w

    assistant.append_setting_widget("Events", page_1)
    assistant.append_setting_widget("Columns", page_2)

    if assistant.run() != RESPONSE_APPLY:
        return

    return (assistant.get_setting("transitions"),
            assistant.get_setting("place_functions"),
            assistant.get_setting("columns"))
