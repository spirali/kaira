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

import gtk
import os.path
import process

class StatespaceConfig(gtk.VBox):

    def __init__(self, app):
        gtk.VBox.__init__(self)
        self.app = app
        self.process = None

        frame = gtk.Frame("Statespace settings")
        frame.set_border_width(5)
        self.pack_start(frame, False, False, 5)

        table = gtk.Table()
        frame.add(table)

        hbox = gtk.HBox()
        label = gtk.Label("Processes:")
        hbox.pack_start(label, False, False)
        self.processes = gtk.SpinButton()
        self.processes.set_adjustment(gtk.Adjustment(2, 1, 1024, 1))
        hbox.pack_start(self.processes, False)
        table.attach(hbox, 0, 1, 0, 1)

        hbox = gtk.HBox()
        label = gtk.Label("Threads:")
        hbox.pack_start(label, False, False)
        self.threads = gtk.SpinButton()
        self.threads.set_adjustment(gtk.Adjustment(1, 1, 1024, 1))
        hbox.pack_start(self.threads, False)
        table.attach(hbox, 1, 2, 0, 1)

        self.indistinguishable_threads = gtk.CheckButton("Indistinguishable threads")
        self.indistinguishable_threads.set_active(True)
        table.attach(self.indistinguishable_threads, 1, 2, 1, 2)

        frame = gtk.Frame("Analyses")
        self.pack_start(frame, False, False, 5)
        frame.set_border_width(5)
        vbox = gtk.VBox()
        frame.add(vbox)

        self.analyze_deadlock = gtk.CheckButton("Deadlock analysis")
        vbox.pack_start(self.analyze_deadlock, False, False)
        self.analyze_deadlock.set_active(True)

        self.analyze_cycles = gtk.CheckButton("Detect computation cycles")
        vbox.pack_start(self.analyze_cycles, False, False)
        self.analyze_cycles.set_active(True)

        frame = gtk.Frame("Other options")
        self.pack_start(frame, False, False, 5)
        frame.set_border_width(5)
        self.create_dot = gtk.CheckButton("Create 'statespace.dot'")
        frame.add(self.create_dot)

        vbox = gtk.HBox(homogeneous=True)
        self.pack_start(vbox, False, False)
        self.start_button = gtk.Button("Build & Run analysis")
        self.start_button.connect("clicked", lambda w: self.start())
        vbox.pack_start(self.start_button, True, True, 5)
        self.stop_button = gtk.Button("Terminate computation")
        self.stop_button.connect("clicked", lambda w: self.stop())
        self.stop_button.set_sensitive(False)
        vbox.pack_start(self.stop_button, True, True, 5)
        self.info_label = gtk.Label()
        self.pack_start(self.info_label, False, False)
        self.show_all()

    def start(self):
        def build_ok():
            self.info_label.set_text("Running computation ...")

            def on_line(line, stream):
                self.info_label.set_text("Running computation ... " + line)
                return True

            def on_exit(code):
                self.process = None
                self.stop_button.set_sensitive(False)
                self.start_button.set_sensitive(True)
                if code == 0:
                    report_name = os.path.join(self.app.project.get_directory(),
                                               self.app.project.get_name() + ".kreport")
                    self.app.load_report(report_name)
                    self.info_label.set_text("Computation finished, report opened")
                else:
                    self.info_label.set_text("Computation failed")

            p = process.Process(build_config.get_executable_filename(),
                                on_line,
                                on_exit)
            p.cwd = self.app.project.get_directory()

            parameters = [ "-r{0}".format(self.processes.get_value_as_int()),
                           "--threads={0}".format(self.threads.get_value_as_int()) ]

            simconfig = self.app.project.get_simconfig()
            if simconfig.parameters_values is None:
                if not self.app.open_simconfig_dialog():
                    return

            parameters += [ "-p{0}={1}".format(k, v)
                            for (k, v) in simconfig.parameters_values.items() ]

            if self.create_dot.get_active():
                parameters.append("-Vdot")

            if self.analyze_deadlock.get_active():
                parameters.append("-Vdeadlock")
            p.start(parameters)
            self.process = p
            self.stop_button.set_sensitive(True)

        def build_fail():
            self.info_label.set_text("Building failed")
            self.start_button.set_sensitive(True)

        self.start_button.set_sensitive(False)
        self.info_label.set_text("Building ...")
        build_config = self.app.project.get_build_config("statespace")
        self.app.start_build(self.app.project, build_config, build_ok, build_fail)

    def stop(self):
        if self.process is not None:
            self.process.shutdown()
            self.process = None
        self.stop_button.set_sensitive(False)
        self.start_button.set_sensitive(True)
