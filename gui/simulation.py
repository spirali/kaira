#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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

    def __init__(self):
        EventSource.__init__(self)
        self.random = random.Random()
        self.runinstance = None

    def connect(self, host, port):
        def connected(stream):
            self.controller = controller
            self.read_header(stream)
            self.query_reports(lambda: self.emit_event("inited"))
        connection = process.Connection(host, port, exit_callback = self.controller_exit, connect_callback = connected)
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
        self.process_running = [True] * self.process_count
        lines_count = utils.xml_int(header, "description-lines")
        project_string = "\n".join((stream.readline() for i in xrange(lines_count)))
        self.project = load_project_from_xml(xml.fromstring(project_string), "")

    def get_instances(self):
        return self.instances

    def is_running(self):
        return any(self.process_running)

    def query_reports(self, callback = None):
        def reports_callback(line):
            run_state = self.is_running()
            root = xml.fromstring(line)
            net_id = utils.xml_int(root, "net-id")
            runinstance = RunInstance(self.project, self.process_count, self.threads_count)
            for e in root.findall("process"):
                process_id = utils.xml_int(e, "id")
                self.process_running[process_id] = utils.xml_bool(e, "running")
                runinstance.event_spawn(process_id, None, 0, net_id)
                for pe in e.findall("place"):
                    place_id = utils.xml_int(pe, "id")
                    for te in pe.findall("token"):
                        name = te.get("value")
                        origin = te.get("origin")
                        if origin is not None:
                            name = "{{{0}}} {1}".format(origin, name)
                        runinstance.add_token(place_id, 0, name)
                    runinstance.clear_removed_and_new_tokens()
                for tre in e.findall("enabled"):
                    runinstance.add_enabled_transition(utils.xml_int(tre, "id"))
            self.runinstance = runinstance
            if not self.is_running() and run_state != self.is_running():
                self.emit_event("error", "Simulation finished\n")
            if callback:
                callback()
            self.emit_event("changed")
        self.controller.run_command("REPORTS", reports_callback)

    def fire_transition(self, transition_id, process_id):
        if not self.process_running[process_id]:
            return
        if self.controller:
            command = "FIRE {0} {1}".format(transition_id, process_id)
            self.controller.run_command_expect_ok(command)
            self.query_reports()
