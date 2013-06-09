#
#    Copyright (C) 2010-2013 Stanislav Bohm
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
import process
import random
from loader import load_project_from_xml
from events import EventSource
from runinstance import RunInstance
import controlseq

import utils

class SimulationException(Exception):
    pass

class Simulation(EventSource):
    """
        Events: changed, inited, error, shutdown
    """

    controller = None
    project = None
    process_count = None
    quit_on_shutdown = False
    init_control_sequence = None

    def __init__(self):
        EventSource.__init__(self)
        self.random = random.Random()
        self.running = True
        self.runinstance = None
        self.sequence = controlseq.ControlSequence()

    def connect(self, host, port):
        def inited():
            self.emit_event("inited")
            if self.init_control_sequence:
                self.run_sequence(self.init_control_sequence)

        def connected(stream):
            self.controller = controller
            self.read_header(stream)
            self.query_reports(inited)
        connection = process.Connection(
            host,
            port,
            exit_callback=self.controller_exit,
            connect_callback=connected)
        controller = process.CommandWrapper(connection)
        controller.start()

    def controller_exit(self, message):
        if message:
            self.emit_event("error", message + "\n")
        if self.controller:
            self.emit_event("error", "Traced process terminated\n")
        self.controller = None

    def shutdown(self):
        if self.controller:
            if self.quit_on_shutdown:
                self.controller.run_command("QUIT", None)
            else:
                self.controller.run_command("DETACH", None)
        self.controller = None
        self.emit_event("shutdown")

    def read_header(self, stream):
        header = xml.fromstring(stream.readline())
        self.process_count = utils.xml_int(header, "process-count")
        self.threads_count = utils.xml_int(header, "threads-count")
        lines_count = utils.xml_int(header, "description-lines")
        project_string = "\n".join((stream.readline() for i in xrange(lines_count)))
        self.project = load_project_from_xml(xml.fromstring(project_string), "")

    def get_instances(self):
        return self.instances

    def query_reports(self, callback=None):
        def reports_callback(line):
            root = xml.fromstring(line)
            net_id = utils.xml_int(root, "net-id")
            runinstance = RunInstance(self.project, self.process_count, self.threads_count)
            for process_id, e in enumerate(root.findall("process")):
                runinstance.event_spawn(process_id, None, 0, net_id)
                for pe in e.findall("place"):
                    place_id = utils.xml_int(pe, "id")
                    for te in pe.findall("token"):
                        name = te.get("value")
                        source = te.get("source")
                        if source is not None:
                            name = "{{{0}}} {1}".format(source, name)
                        runinstance.add_token(place_id, 0, name)
                    runinstance.clear_removed_and_new_tokens()

                for tre in e.findall("enabled"):
                    runinstance.add_enabled_transition(utils.xml_int(tre, "id"))

            for e in root.findall("activation"):
                process_id = utils.xml_int(e, "process-id")
                thread_id = utils.xml_int(e, "thread-id")
                transition_id = utils.xml_int(e, "transition-id")
                runinstance.transition_fired(process_id,
                                             thread_id,
                                             0,
                                             transition_id, [])

            for e in root.findall("packet"):
                origin_id = utils.xml_int(e, "origin-id")
                target_id = utils.xml_int(e, "target-id")
                size = utils.xml_int(e, "size")
                edge_id = utils.xml_int(e, "edge-id")
                runinstance.event_send(origin_id, 0, 0, target_id, size, edge_id)

            runinstance.reset_last_event_info()
            self.runinstance = runinstance
            if self.running and utils.xml_bool(root, "quit"):
                self.running = False
                self.emit_event("error", "Program finished\n")
            if callback:
                callback()
            self.emit_event("changed")

        self.controller.run_command("REPORTS", reports_callback)

    def check_running(self):
        if not self.running:
            self.emit_event("error", "Program finished\n")
            return False
        else:
            return True

    def run_sequence(self, sequence):
        transitions = {}
        for t in self.runinstance.net.transitions():
            transitions[t.get_name()] = t

        def fire(process_id, thread_id, transition):
            t = transitions.get(transition)
            if t is None:
                 raise SimulationException("Transition not found")
            self.fire_transition(t.id, process_id, 1, query_reports=False)

        def finish(process_id, thread_id):
            self.finish_transition(process_id, thread_id, query_reports=False)

        def receive(process_id, thread_id, from_process):
            self.receive(process_id, from_process, query_reports=False)

        sequence.execute(fire, finish, receive)
        self.query_reports()

    def receive(self, process_id, origin_id, query_reports=True):
        if self.controller:
            command = "RECEIVE {0} {1}".format(process_id, origin_id)
            self.controller.run_command_expect_ok(
                command,
                lambda: self.sequence.add_receive(process_id, 0, origin_id))
            if query_reports:
                self.query_reports()

    def receive_all(self, process_ids=None):
        if process_ids is None:
            ids = xrange(self.process_count)
        else:
            ids = process_ids
        for i in ids:
            for j in xrange(self.process_count):
                for p in xrange(self.runinstance.get_packets_count(j, i)):
                    self.receive(i, j, query_reports=False)
        self.query_reports()

    def fire_transition(self, transition_id, process_id, phases, callback=None, query_reports=True):
        transition = self.project.get_item(transition_id)

        def fire_callback():
            self.sequence.add_fire(process_id, 0, transition.get_name())
            if transition.has_code() and phases == 2:
                self.sequence.add_finish(process_id, 0)

        if self.controller and self.check_running():
            command = "FIRE {0} {1} {2}".format(transition_id, process_id, phases)
            self.controller.run_command_expect_ok(command, fire_callback)
            if query_reports:
                self.query_reports(callback)

    def finish_transition(self, process_id, thread_id, callback=None, query_reports=True):
        if self.controller and self.check_running():
            command = "FINISH {0} {1}".format(process_id, thread_id)
            self.controller.run_command_expect_ok(
                command,
                lambda: self.sequence.add_finish(process_id, thread_id))
            if query_reports:
                self.query_reports(callback)
