#
#    Copyright (C) 2010-2013 Stanislav Bohm
#                  2011       Ondrej Garncarz
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

import os
import re
import sys
import paths
sys.path.append(paths.PTP_DIR)
import ConfigParser

import gtkutils
from mainwindow import MainWindow, Tab, SaveTab
import neteditor
from simconfig import SimConfigDialog
from projectconfig import ProjectConfig
from simulation import Simulation
import simview
import codeedit
import process
import settings
import loader
import ptp
import codetests
import report
import statespace
import utils
import controlseq
import simrun
import extensions


class App:
    """
        The class represents Kaira's gui, the callbacks from mainwindow
        (mainly from menu) calls methods of this class
    """
    def __init__(self, args):
        self.window = MainWindow(self)
        self.window.set_size_request(950,660)
        self.neteditor = None
        self.project = None
        self.sources_repository = extensions.SourcesRepository()
        self._open_welcome_tab()
        self.grid_size = 1
        self.settings = self.load_settings()
        self.message_parser = re.compile(
            "\*(?P<location>((?P<id_int>\d+)/(?P<section>[^:]*)|(?P<id_string>[^:]+))"
            "(:(?P<line>\d+))?):(?P<message>.*)")

        operation = None
        arg_index = 0

        for arg in args:
            if arg.startswith("OP:"):
                operation_class = extensions.operations.get(arg[3:])
                if operation_class is None:
                    self.console_write("Invalid operation".format(arg), "error")
                else:
                    operation = operation_class()
                continue

            filename = os.path.abspath(arg)
            if os.path.isfile(filename):
                if arg.endswith(".kreport"):
                    self.load_report(arg)
                elif arg.endswith(".proj"):
                    self.set_project(loader.load_project(filename))
                else:
                    source = self.sources_repository.load_source(filename, self)
                    if source is None:
                        operation = None
                        self.console_write("Do no know how to load '{0}' \n".format(filename), "error")
                    if operation:
                        operation.parameters[arg_index].attach_source(source, 0)
                        arg_index += 1
                    if len(self.sources_repository) > 0:
                        self.run_tool_window()
            else:
                self.console_write("File '{0}' not found\n".format(filename), "error")

        if operation:
            operation.execute(self)
            self.console_write("Operation '{0}' executed\n".format(operation.name), "info")

    def get_settings_filename(self):
        return os.path.expanduser("~/.config/kaira/kaira.conf")

    def load_settings(self):
        settings = ConfigParser.ConfigParser()

        # Fill defaults
        settings.add_section("main")
        settings.set("main", "save-before-build", "True")
        settings.set("main", "ptp-debug", "False")

        filename = self.get_settings_filename()
        if os.path.isfile(filename):
            with open(filename, "r") as f:
                settings.readfp(f)
        return settings

    def save_settings(self):
        filename = self.get_settings_filename()
        with utils.mkdir_if_needed_and_open(filename) as f:
            self.settings.write(f)

    def run(self):
        try:
            gtk.gdk.threads_init()
            self.window.show()
            gtk.main()
        finally:
            self.shutdown()

    def shutdown(self):
        self.window.close_all_tabs()

    def set_project(self, project):
        self.project = project
        self.project.set_callback("filename_changed", self._project_filename_changed)
        self.init_tabs()
        self.window.console.reset()
        self._project_filename_changed()

    def init_tabs(self):
        self.window.close_all_tabs(("project", "welcome"))
        self.neteditor = neteditor.NetEditor(self, self.project)
        self.neteditor.transition_edit_callback = self.transition_edit
        self.neteditor.place_edit_callback = self.place_edit
        self.window.add_tab(Tab("Nets",
                                self.neteditor,
                                "nets",
                                mainmenu_groups=("project", "undo", "screenshot"),
                                has_close_button=False))

    def switch_to_net(self, net):
        self.window.switch_to_tab_by_key("nets")
        self.neteditor.switch_to_net(net)

    def new_project(self):
        def project_name_changed(w = None):
            name = builder.get_object("newproject-name").get_text()
            ok = all(c.isalnum() or c == "-" or c == "_" for c in name) and name != ""
            builder.get_object("newproject-dir").set_text(os.path.join(directory[0], name))
            builder.get_object("newproject-ok").set_sensitive(ok)
        def change_directory(w):
            d = self.run_file_dialog("Project directory", "open-directory")
            if d is not None:
                directory[0] = d
                project_name_changed()
        builder = gtkutils.load_ui("newproject-dialog")
        for project_class in loader.projects:
            builder.get_object("newproject-target-env").append_text(project_class.get_target_env_name())
        builder.get_object("newproject-target-env").set_active(0)
        dlg = builder.get_object("newproject-dialog")
        dlg.set_transient_for(self.window)
        builder.get_object("newproject-name").connect("changed", project_name_changed)
        directory = [os.getcwd()]
        project_name_changed()
        builder.get_object("newproject-dirbutton").connect("clicked", change_directory)
        try:
            if dlg.run() == gtk.RESPONSE_OK:
                dirname = builder.get_object("newproject-dir").get_text()
                if os.path.exists(dirname):
                    self.show_error_dialog("Path '%s' already exists" % dirname)
                    return
                target_env_name = builder.get_object("newproject-target-env").get_active_text()
                p = self._catch_io_error(lambda: loader.new_empty_project(dirname, target_env_name))
                if p is not None:
                    self.set_project(p)
        finally:
            dlg.hide()

    def save_sequence_into_project(self, sequence):
        if self.project is None:
            self.show_error_dialog("No project is opened.")
            return
        if sequence.name is None:
            sequence.name = "Sequence"
        if controlseq.sequence_dialog(sequence, self.window):
            self.project.add_sequence(sequence)
        self.edit_control_sequences()

    def export_tracelog_sequence(self):
        tab = self.window.get_current_tab()
        sequence = tab.widget.export_sequence()
        self.save_sequence_into_project(sequence)

    def run_file_dialog(self, title, mode, filter_name=None, pattern=None):
        if mode == "open":
            action = gtk.FILE_CHOOSER_ACTION_OPEN
            stock = gtk.STOCK_OPEN
        elif mode == "save":
            action = gtk.FILE_CHOOSER_ACTION_SAVE
            stock = gtk.STOCK_SAVE
        elif mode == "open-directory":
            action = gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER
            stock = gtk.STOCK_OPEN
        dialog = gtk.FileChooserDialog(title,
                                       self.window,
                                       action,
                                       (gtk.STOCK_CANCEL,
                                        gtk.RESPONSE_CANCEL,
                                        stock,
                                        gtk.RESPONSE_OK))
        dialog.set_current_folder(os.getcwd())
        dialog.set_default_response(gtk.RESPONSE_OK)
        if filter_name is not None:
            self._add_file_filters(dialog, ((filter_name, pattern),), all_files=True)

        try:
            response = dialog.run()
            filename = dialog.get_filename()
        finally:
            dialog.destroy()

        if response == gtk.RESPONSE_OK:
            if mode == "save" and \
               os.path.isfile(filename) and \
               not self.show_yesno_dialog(
                   "File '{0}' already exists. Do you want to overwrite it?"
                        .format(filename)):
               return None
            return filename
        else:
            return None

    def load_project(self):
        filename = self.run_file_dialog("Open project", "open", "Project", "*.proj")
        if filename is None:
            return

        if filename[-5:] != ".proj":
            filename = filename + ".proj"

        p = self._catch_io_error(lambda: loader.load_project(filename))
        if p:
            self.set_project(p)

    def load_report(self, filename=None):
        if filename is None:
            filename = self.run_file_dialog("Open report", "open", "Report", "*.kreport")
            if filename is None:
                return
        r = self._catch_io_error(lambda: report.Report(filename))
        self.window.add_tab(Tab("Report", report.ReportWidget(self, r)))

    def save_project(self):
        if self.project.get_filename() is None:
            self.save_project_as()
        else:
            self._save_project()

    def close_current_tab(self, force=False):
        tab = self.window.get_current_tab()
        if force or tab.has_close_button():
            tab.close()

    def close_simulation_tabs(self):
        self.window.close_all_tabs(predicate_fn=lambda tab: isinstance(tab, simview.SimViewTab))

    def save_project_as(self):
        filename = self.run_file_dialog("Save project", "save", "Project", "*.proj")
        if filename is None:
            return
        if filename[-5:] != ".proj":
            filename = filename + ".proj"
        self.project.set_filename(filename)
        self._save_project()

    def _save_project(self, silent = False):
        self.window.foreach_tab(lambda tab: tab.project_save())
        if self._catch_io_error(self.project.save, True, False) and not silent:
            self.console_write("Project saved as '{0}'\n".format(self.project.get_filename()),
                               "success")

    def build_project(self, target):
        self.console_write("Building '{0}' ...\n".format(target), "info")
        build_config = self.project.get_build_config(target)
        self.start_build(self.project,
                         build_config,
                         lambda: self.console_write("Build finished ({0})\n".format(target), "success"))

    def run_tool_window(self):
        tab = Tab("Tools",
                  extensions.OperationManager(self),
                  call_close=True)
        self.window.add_tab(tab)

    def run_statespace_analysis(self):
        self.window.add_tab(Tab("Statespace",
                                statespace.StatespaceConfig(self),
                                mainmenu_groups=("project",)))

    def get_grid_size(self):
        return self.grid_size

    def set_grid_size(self, grid_size):
        self.grid_size = grid_size

    def undo(self):
        tab = self.window.get_current_tab()
        tab.widget.undo()

    def redo(self):
        tab = self.window.get_current_tab()
        tab.widget.redo()

    def _catch_io_error(self, fcn, return_on_ok=None, return_on_err=None):
        try:
            result = fcn()
            if return_on_ok == None:
                return result
            else:
                return return_on_ok
        except IOError as e:
            self.show_error_dialog(str(e))
            return return_on_err
        except OSError as e:
            self.show_error_dialog(str(e))
            return return_on_err

    def _add_file_filters(self, dialog, filters, all_files):
        if all_files:
            filters += (("All files", "*"),)
        for f in filters:
            ffilter = gtk.FileFilter()
            ffilter.set_name(f[0])
            ffilter.add_pattern(f[1])
            dialog.add_filter(ffilter)

    def edit_code_tests(self):
        if self.window.switch_to_tab_by_key("codetests"):
            return
        widget = codetests.CodeTestList(self)
        self.window.add_tab(SaveTab("Tests",
                                    widget,
                                    "codetests",
                                    mainmenu_groups=("project",)))

    def edit_control_sequences(self):
        if self.window.switch_to_tab_by_key("sequences"):
            return
        widget = controlseq.SequenceListWidget(self.project)
        self.window.add_tab(Tab(
            "Sequences", widget, "sequences",
            mainmenu_groups=("project",), call_close=True))

    def edit_simrun(self, lineno=None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key("simrun"):
            return
        widget = simrun.SimRunConfig(self, self.project)
        self.window.add_tab(Tab(
            "SimRun", widget, "simrun",
            mainmenu_groups=("project",)))
        widget.editor.jump_to_position(position)

    def transition_edit(self, transition, lineno=None):
        if transition.collective:
            self.console_write("A collective transition cannot contain an inner code.\n", "warn")
            return
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
                transition,
                lambda tab: tab.widget.jump_to_position(position)):
            return

        name = "T: {0}".format(transition.get_name_or_id())
        generator = self.get_safe_generator()
        if generator is None:
            return
        header = generator.get_transition_user_fn_header(transition.id)
        editor = codeedit.TransitionCodeEditor(self.project, transition, header)
        self.window.add_tab(Tab(name,
                                editor,
                                transition,
                                mainmenu_groups=("project",)))
        editor.jump_to_position(position)

    def place_edit(self, place, lineno=None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
            place,
            lambda tab: tab.widget.jump_to_position(position)):
            return

        generator = self.get_safe_generator()
        if generator is None:
            return
        header = generator.get_place_user_fn_header(place.id)

        name = "P: {0}".format(place.get_name_or_id())
        editor = codeedit.PlaceCodeEditor(self.project, place, header)
        self.window.add_tab(Tab(name, editor, place, mainmenu_groups=("project",)))
        editor.jump_to_position(position)

    def catch_ptp_exception(self, fn, show_errors=True):
        try:
            return (True, fn())
        except ptp.PtpException, e:
            if show_errors:
                error_messages = {}
                self._process_error_line(str(e), error_messages)
                self.project.set_error_messages(error_messages)
            return (False, None)

    def project_config(self):
        if self.window.switch_to_tab_by_key("project-config"):
            return
        widget = ProjectConfig(self)
        self.window.add_tab(Tab("Project",
                                widget,
                                "project-config",
                                mainmenu_groups=("project",)))

    def edit_settings(self):
        if self.window.switch_to_tab_by_key("settings"):
            return
        widget = settings.SettingsWidget(self)
        self.window.add_tab(Tab("Settings", widget, "settings"))

    def edit_head(self, lineno=None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
                "Head",
                lambda tab: tab.widget.jump_to_position(position)):
            return

        editor = codeedit.HeadCodeEditor(self.project)
        tab = codeedit.TabCodeEditor("Head", editor, "Head")
        tab.mainmenu_groups = ("project",)
        self.window.add_tab(tab)
        editor.jump_to_position(position)

    def run_simulated_program(self, name, directory, simconfig, valgrind):
        def output(line, stream):
            self.console_write_output(line)
            return True

        if valgrind:
            program_name = "valgrind"
            parameters = [ "-q", name ]
        else:
            program_name = name
            parameters = []

        parameters += [ "-s", "auto", "-b", "-r", str(simconfig.process_count) ]
        sprocess = process.Process(program_name, output)
        sprocess.cwd = directory
        # FIXME: Timeout
        other_params = [ "-p{0}={1}".format(k, v)
                         for (k, v) in simconfig.parameters_values.items() ]
        first_line = sprocess.start_and_get_first_line(parameters + other_params)
        try:
            port = int(first_line)
        except ValueError:
            self.console_write("Simulated program return invalid first line: "
                + first_line, "error")
            return None, None
        return sprocess, port

    def simulation_start(self, valgrind=False):
        def project_builded():
            sprocess, port = self.run_simulated_program(
                build_config.get_executable_filename(),
                self.project.get_directory(),
                simconfig,
                valgrind)
            if sprocess is None:
                return
            simulation = self.new_simulation()
            simulation.init_control_sequence = simconfig.sequence
            simulation.quit_on_shutdown = True
            simulation.set_callback(
                "inited",
                lambda: self.window.add_tab(simview.SimViewTab(self,
                                                               simulation,
                                                               mainmenu_groups=("project", "screenshot"))))
            simulation.set_callback("shutdown", lambda: sprocess.shutdown())
            simulation.connect("localhost", port)

        simconfig = self.project.get_simconfig()
        if simconfig.parameters_values is None:
            if not self.open_simconfig_dialog():
                return

        build_config = self.project.get_build_config("simulation")
        self.console_write("Preparing simulation ...\n", "info")
        self.start_build(self.project, build_config, project_builded)

    def open_simconfig_dialog(self):
        dialog = SimConfigDialog(self.window, self.project)
        try:
            if dialog.run() == gtk.RESPONSE_OK:
                dialog.set_simconfig(self.project)
                return True
            else:
                return False
        finally:
            dialog.destroy()

    def show_message_dialog(self, text, type):
        error_dlg = gtk.MessageDialog(parent=self.window,
                                      type=type,
                                      message_format=text,
                                      buttons=gtk.BUTTONS_OK)
        try:
            error_dlg.run()
        finally:
            error_dlg.destroy()

    def show_yesno_dialog(self, text):
        error_dlg = gtk.MessageDialog(parent=self.window,
                                      type=gtk.MESSAGE_QUESTION,
                                      message_format=text,
                                      buttons=gtk.BUTTONS_YES_NO)
        try:
            return error_dlg.run() == gtk.RESPONSE_YES
        finally:
            error_dlg.destroy()

    def show_error_dialog(self, text):
        self.show_message_dialog(text, gtk.MESSAGE_ERROR)

    def show_info_dialog(self, text):
        self.show_message_dialog(text, gtk.MESSAGE_INFO)

    def console_write(self, text, tag_name = "normal"):
        self.window.console.write(text, tag_name)

    def console_write_output(self, text):
        self.console_write("OUTPUT: " + text, "output")

    def console_write_link(self, text, callback):
        self.window.console.write_link(text, callback)

    def export_project(self, proj, build_config):
        self.window.foreach_tab(lambda tab: tab.project_export())
        proj.export(build_config)
        return True

    def hide_error_messages(self):
        self.project.set_error_messages({})

    def new_simulation(self):
        simulation = Simulation()
        simulation.set_callback("error", lambda line: self.console_write(line, "error"))
        simulation.set_callback("command-failed",
            lambda sequence, command:
                self.console_write("Command [{0}/{1}] {2} cannot be executed".format(
                                        command,
                                        sequence.get_commands_size(),
                                        sequence.commands[command]),
                                   "error"))
        return simulation

    def connect_to_application(self):
        def inited():
            self.console_write("Connected\n", "success")
            self.window.add_tab(simview.SimViewTab(self,
                                                   simulation,
                                                   "{0}:{1}".format(host, port)))

        address = simview.connect_dialog(self.window);
        if address is None:
            return

        host = address[0]
        port = address[1]
        simulation = self.new_simulation()
        simulation.set_callback("inited", inited)
        self.console_write("Connecting to {0}:{1} ...\n".format(host, port))
        simulation.connect(host, port)

    def get_safe_generator(self):
        """ Calls self.project,get_generator(), if errors occur shows them and returns None"""
        try:
            return self.project.get_generator()
        except ptp.PtpException, e:
            error_messages = {}
            self._process_error_line(e.message, error_messages)
            self.project.set_error_messages(error_messages)

    def _project_filename_changed(self):
        self.window.set_title("Kaira - {0}".format(self.project.get_name()))

    def _run_build_program(self, name, args, directory, ok_callback, fail_callback):
        def on_exit(code):
            if code == 0:
                if ok_callback:
                    ok_callback()
            else:
                if fail_callback:
                    fail_callback()

        def on_line(line, stream):
            self._process_error_line(line, None)
            return True

        p = process.Process(name, on_line, on_exit)
        p.cwd = directory
        p.start(args)

    def _run_makefile(self,
                      project,
                      build_directory,
                      ok_callback=None,
                      fail_callback=None,
                      target=None):
        args = []
        if target is not None:
            args.append(target)
        self._run_build_program("make",
                                args,
                                build_directory,
                                ok_callback,
                                fail_callback)

    def start_build(self, proj, build_config, ok_callback, fail_callback=None):
        if self.settings.getboolean("main", "save-before-build"):
            self._save_project(silent=True)

        self._start_ptp(proj,
                        build_config,
                        lambda lines: self._run_makefile(proj,
                                                         build_config.directory,
                                                         ok_callback,
                                                         fail_callback),
                        fail_callback)

    def _start_ptp(self, proj, build_config, ok_callback=None, fail_callback=None):
        stdout = []
        def on_exit(code):
            error_messages = {}
            if code == 0:
                self.project.set_error_messages(error_messages)
                if ok_callback:
                    ok_callback(stdout)
            else:
                for line in stdout:
                    self._process_error_line(line, error_messages)
                self.project.set_error_messages(error_messages)
                self.console_write("Building failed\n", "error")
                if fail_callback:
                    fail_callback()

        def on_line(line, stream):
            if debug:
                self.console_write(line)
            stdout.append(line)
            return True
        if not self.export_project(proj, build_config):
            return


        debug = self.settings.getboolean("main", "ptp-debug")
        p = process.Process(paths.PTP_BIN, on_line, on_exit)
        p.cwd = proj.get_directory()

        args = []

        if debug:
            args.append("--debug")

        if build_config.directory is not None:
            args += [ "--output", build_config.directory ]

        args.append(build_config.operation)
        args.append(build_config.get_export_filename())
        p.start(args)

    def _process_error_line(self, line, error_messages):
        match = self.message_parser.match(line)
        if match is None:
            self.console_write(line)
            return

        section = match.group("section")
        location = match.group("location")
        message = match.group("message")

        net = None
        if match.group("id_int") is not None:
            net, item = self.project.get_net_and_item(int(match.group("id_int")))
            if item is None:
                self.console_write(line)
                return
            if net is not None:
                location = net.name + ":" + location
            if error_messages is not None:
                d = error_messages.setdefault(item.id, {})
                lines = d.setdefault(section, [])
                lines.append(message)
        else:
            item = match.group("id_string")

        if match.group("line") is not None:
            line_no = int(match.group("line"))
        else:
            line_no = 0

        # In case of generated template:
        # 2 is subtracted because #LINE directive is place
        # before function definition, but
        # we jump at the line counted from the beginnging of user defined text
        # 1 for function definition, 1 for line with {
        if item == "head":
            callback = lambda: self.edit_head(line_no)
        elif item == "communication-model":
            callback = lambda: self.edit_simrun(line_no - 2)
        elif section == "function":
            callback = lambda: self.transition_edit(item, line_no - 2)
        elif section == "init_function":
            callback = lambda: self.place_edit(item, line_no - 2)
        elif net is not None:
            callback = lambda: self.switch_to_net(net)
        else:
            self.console_write(line)
            return
        self.console_write_link(location, callback)
        self.console_write(":" + message + "\n")

    def _open_welcome_tab(self):
        label = gtk.Label()
        line = "<span size='xx-large'>Kaira</span>\nv{0}\n\n" \
                "<a href='http://verif.cs.vsb.cz/kaira'>http://verif.cs.vsb.cz/kaira</a>" \
                    .format(ptp.get_config("Main", "VERSION"))
        label.set_markup(line)
        label.set_justify(gtk.JUSTIFY_CENTER)
        self.window.add_tab(Tab("Welcome",
                                label,
                                has_close_button=False,
                                mainmenu_groups=("welcome",)))

    def import_project(self):
        filename = self.run_file_dialog("Import project", "open", "Project", "*.proj")
        if filename is None:
            return

        if filename[-5:] != ".proj":
            filename = filename + ".proj"
        loader.import_project(self.project, filename)

    def save_as_svg(self):
        tab = self.window.get_current_tab()
        filename = "net.svg"
        tab.widget.save_as_svg(filename)
        self.console_write("Net saved as '{0}'.\n".format(filename), "success")

if __name__ == "__main__":
    args = sys.argv[1:] # Remove "app.py"
    extensions.load_extensions()
    app = App(args)
    app.run()

