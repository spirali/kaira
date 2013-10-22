#
#    Copyright (C) 2013 Stanislav Bohm
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


import extensions
import datatypes
import settingswindow
import utils
from runinstance import RunInstance

class ExportRunInstance(RunInstance):

    def __init__(self, tracelog, transitions, place_functions, columns):
        RunInstance.__init__(self, tracelog.project, tracelog.process_count, tracelog.threads_count)
        self.transitions = transitions
        self.place_functions = place_functions
        self.column_event = "Event" in columns
        self.column_time = "Time" in columns
        self.column_id = "ID" in columns
        self.column_process = "Process" in columns
        self.column_tet = "TET" in columns
        self.column_value = bool(place_functions)
        self.rows = []
        self.tet = None

    def pre_event(self):
        RunInstance.pre_event(self)
        self.tet = None

    def transition_finished(self, process_id, thread_id, time):
        activity = self.activites[process_id * self.threads_count + thread_id]
        self.tet = time - activity.time
        if activity.transition in self.transitions:
            self.add_row("T", time, activity.transition.id, process_id, "")
        RunInstance.transition_finished(self, process_id, thread_id, time)

    def add_token(self, place_id, token_pointer, token_value, send_time):
        RunInstance.add_token(self, place_id, token_pointer, token_value, send_time)
        for pf in self.place_functions:
            if pf[0].id == place_id:
                self.add_row("A",
                             self.last_event_time,
                             place_id,
                             self.last_event_process,
                             token_value[pf[1]])

    def add_row(self, event, time, id, process, value):
        row = []
        if self.column_event:
            row.append(event)
        if self.column_time:
            row.append(time)
        if self.column_id:
            row.append(id)
        if self.column_process:
            row.append(process)
        if self.column_tet:
            row.append(self.tet)
        if self.column_value:
            row.append(value)
        self.rows.append(row)

    def create_table(self):
        header = []
        if self.column_event:
            header.append("Event")
        if self.column_time:
            header.append("Time")
        if self.column_id:
            header.append("ID")
        if self.column_process:
            header.append("Process")
        if self.column_tet:
            header.append("TET")
        if self.column_value:
            header.append("Value")
        return (header, self.rows)


class TracelogExport(extensions.Operation):

    name = "Tracelog export"
    description = "Export data from tracelog into a table"

    parameters = [ extensions.Parameter("Tracelog", datatypes.t_tracelog) ]

    def run(self, app, tracelog):
        assistant = settingswindow.BasicSettingAssistant(2,
                                                         "Export settings",
                                                         app.window)
        assistant.set_size_request(600, 600)
        def page_1(setting):
            w = settingswindow.SettingWidget()
            data = [ (t.get_name_or_id(), t, False)
                     for t in tracelog.project.nets[0].transitions() ]
            w.add_checkbuttons_list("transitions", "Transitions", data, [ "Transition", "Export fire?" ] )
            data = [ ("{0}/{1}".format(p.get_name_or_id(), tracing.name), (p, i), False)
                     for p in tracelog.project.nets[0].places() for i, tracing in enumerate(p.tracing) ]
            w.add_checkbuttons_list("place_functions", "Place\nFunctions", data, [ "Place/Function", "Export add token?" ] )
            return w
        def page_2(setting):
            w = settingswindow.SettingWidget()
            data = [ ("Event", "Event", False),
                     ("Time", "Time", True),
                     ("ID", "ID", False),
                     ("Process", "Process", False),
                     ("Transition Execution Time", "TET", False),
                   ]
            w.add_checkbuttons("columns", "Columns", data)
            return w

        assistant.append_setting_widget("Events", page_1)
        assistant.append_setting_widget("Columns", page_2)

        if not assistant.run():
            return

        ri = ExportRunInstance(tracelog,
                               assistant.collected_setting[0]["transitions"],
                               assistant.collected_setting[0]["place_functions"],
                               assistant.collected_setting[1]["columns"])
        tracelog.execute_events(ri)
        return extensions.Source("Tracelog Table " + utils.get_timestamp_string(),
                                 datatypes.t_table,
                                 ri.create_table())

extensions.add_operation(TracelogExport)
