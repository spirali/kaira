#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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
import gtkutils
from mainwindow import MainWindow, Tab, SaveTab
from netview import NetView
from simconfig import SimConfigDialog
from projectconfig import ProjectConfig
from simulation import Simulation
from tracelog import TraceLog
import simview
import codeedit
import process
import settings
import externtypes
import functions
import loader
import ptp
import runview
import codetests

VERSION_STRING = '0.5'

class App:
    """
        The class represents Kaira's gui, the callbacks from mainwindow
        (mainly from menu) calls methods of this class
    """
    def __init__(self, args):
        self.window = MainWindow(self)
        self.window.set_size_request(500,450)
        self.window.project_is_active(False)
        self.nv = None
        self._open_welcome_tab()
        self.grid_size = 1
        self.settings = {
            "save-before-build" : True,
            "ptp-debug" : False
        }

        if args:
            if os.path.isfile(args[0]):
                if args[0].endswith(".kth"):
                    self.open_tracelog_tab(args[0])
                else:
                    self.set_project(loader.load_project(args[0]))
            else:
                self.console_write("File '%s' not found\n" % args[0], "error")

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
        self.project.set_callback("changed", self._project_changed)
        self.project.set_callback("filename_changed", self._project_filename_changed)
        self.init_tabs()
        self.window.console.reset()
        self._project_changed()
        self._project_filename_changed()
        self.window.project_is_active(True)

    def init_tabs(self):
        self.window.close_all_tabs()
        self.nv = NetView(self, self.project)
        self.nv.transition_edit_callback = self.transition_edit
        self.nv.place_edit_callback = self.place_edit
        self.window.add_tab(Tab("Nets", self.nv, has_close_button = False))

    def new_project(self):
        def project_name_changed(w = None):
            name = builder.get_object("newproject-name").get_text().strip()
            builder.get_object("newproject-dir").set_text(os.path.join(directory[0], name))
            builder.get_object("newproject-ok").set_sensitive(name != "")
        def change_directory(w):
            d = self._directory_choose_dialog("Select project directory")
            if d is not None:
                directory[0] = d
                project_name_changed()
        builder = gtkutils.load_ui("newproject-dialog")
        for project_class in loader.projects:
            builder.get_object("newproject-extenv").append_text(project_class.get_extenv_name())
        builder.get_object("newproject-extenv").set_active(0)
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
                extenv_name = builder.get_object("newproject-extenv").get_active_text()
                p = self._catch_io_error(lambda: loader.new_empty_project(dirname, extenv_name))
                if p is not None:
                    self.set_project(p)
        finally:
            dlg.hide()

    def load_project(self):
        dialog = gtk.FileChooserDialog("Open project", self.window, gtk.FILE_CHOOSER_ACTION_OPEN,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)
        try:
            self._add_project_file_filters(dialog)
            response = dialog.run()
            if response == gtk.RESPONSE_OK:
                filename = dialog.get_filename()
                if filename[-5:] != ".proj":
                    filename = filename + ".proj"

                p = self._catch_io_error(lambda: loader.load_project(filename))
                if p:
                    # TODO: set statusbar
                    self.set_project(p)
        finally:
            dialog.destroy()

    def load_tracelog(self):
        dialog = gtk.FileChooserDialog("Open Log", self.window, gtk.FILE_CHOOSER_ACTION_OPEN,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        try:
            dialog.set_default_response(gtk.RESPONSE_OK)
            self._add_file_filters(dialog, (("Kaira Tracelog Header", "*.kth"),), all_files = True)

            response = dialog.run()
            if response == gtk.RESPONSE_OK:
                self.open_tracelog_tab(dialog.get_filename())
        finally:
            dialog.destroy()

    def open_tracelog_tab(self, filename):
        t = self._catch_io_error(lambda: TraceLog(filename))
        rv = runview.RunView(self, t)
        self.window.add_tab(Tab("Tracelog", rv))

    def save_project(self):
        if self.project.get_filename() is None:
            self.save_project_as()
        else:
            self._save_project()

    def close_current_tab(self, force=False):
        tab = self.window.current_tab()
        if force or tab.has_close_button():
            tab.close()

    def save_project_as(self):
        dialog = gtk.FileChooserDialog("Save net", self.window, gtk.FILE_CHOOSER_ACTION_SAVE,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_SAVE, gtk.RESPONSE_OK))
        try:
            dialog.set_default_response(gtk.RESPONSE_OK)
            self._add_project_file_filters(dialog)

            response = dialog.run()
            if response == gtk.RESPONSE_OK:
                filename = dialog.get_filename()
                if filename[-5:] != ".proj":
                    filename = filename + ".proj"
                self.project.set_filename(filename)
                self._save_project()
        finally:
            dialog.destroy()

    def _save_project(self, silent = False):
        self.window.foreach_tab(lambda tab: tab.project_save())
        if self._catch_io_error(self.project.save, True, False) and not silent:
            self.console_write("Project saved as '%s'\n" % self.project.get_filename(), "success")

    def build_project(self, target):
        self.console_write("Building '{0}' ...\n".format(target))
        build_config = self.project.get_build_config(target)
        self.start_build(self.project, build_config,
                          lambda p: self.console_write("Build finished\n", "success"))

    def get_grid_size(self):
        return self.grid_size

    def set_grid_size(self, grid_size):
        self.grid_size = grid_size

    def _catch_io_error(self, fcn, return_on_ok = None, return_on_err = None):
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

    def _add_project_file_filters(self, dialog):
        self._add_file_filters(dialog, (("Projects", "*.proj"),), all_files = True)

    def edit_code_tests(self):
        if not self.project.is_library():
            self.show_info_dialog("Tests are available only for project type 'Library'.")
            return
        if self.window.switch_to_tab_by_key("codetests"):
            return
        widget = codetests.CodeTestList(self)
        self.window.add_tab(SaveTab("Tests", widget, "codetests"))

    def transition_edit(self, transition, lineno = None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
                transition,
                lambda tab: tab.widget.jump_to_position(position)):
            return

        def open_tab(stdout):
            if transition.get_name() != "":
                name = "T:" + transition.get_name()
            else:
                name = "T: <unnamed" + str(transition.get_id()) + ">"
            editor = codeedit.TransitionCodeEditor(self.project, transition, "".join(stdout))
            self.window.add_tab(Tab(name, editor, transition))
            editor.jump_to_position(position)

        build_config = self.project.get_build_config("simulation")
        self._start_ptp(self.project, build_config, open_tab,
            extra_args = [ "--transition-user-fn", str(transition.get_id()) ])

    def place_edit(self, place, lineno = None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
            place,
            lambda tab: tab.widget.jump_to_position(position)):
            return

        def open_tab(stdout):
            name = "P: " + str(place.get_id())
            editor = codeedit.PlaceCodeEditor(self.project, place, "".join(stdout))
            self.window.add_tab(Tab(name, editor, place))
            editor.jump_to_position(position)

        build_config = self.project.get_build_config("simulation")
        self._start_ptp(self.project, build_config, open_tab,
            extra_args = [ "--place-user-fn", str(place.get_id())])

    def extern_type_functions_edit(self, externtype, position = None):
        if self.window.switch_to_tab_by_key(
            externtype,
            lambda tab: tab.widget.jump_to_position(position)):
            return

        editor = externtypes.ExternTypeEditor(self.project, externtype)
        self.window.add_tab(Tab(externtype.get_name(), editor, externtype))
        editor.jump_to_position(position)

    def function_edit(self, function, lineno = None):
        position = ("", lineno) if lineno is not None else None
        if self.window.switch_to_tab_by_key(
                function,
                lambda tab: tab.widget.jump_to_position(position)):
            return
        try:
            editor = functions.FunctionEditor(self.project, function)
            self.window.add_tab(Tab(function.get_name(), editor, function))
            editor.jump_to_position(position)
        except ptp.PtpException, e:
            self.console_write("Cannot open function '{0}'\n".format(function.get_name()), "error")
            self.console_write(str(e) + "\n", "error")

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
        w = ProjectConfig(self)
        self.window.add_tab(Tab("Project", w, "project-config"))

    def edit_settings(self):
        if self.window.switch_to_tab_by_key("settings"):
            return
        w = settings.SettingsWidget(self)
        self.window.add_tab(Tab("Settings", w, "settings"))

    def edit_head(self, lineno = None):
        position = ("", lineno) if lineno is not None else None

        if self.window.switch_to_tab_by_key(
                "Head",
                lambda tab: tab.widget.jump_to_position(position)):
            return

        editor = codeedit.HeadCodeEditor(self.project)
        self.window.add_tab(codeedit.TabCodeEditor("Head", editor, "Head"))
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


    def simulation_start(self, valgrind = False):
        def project_builded(project):
            sprocess, port = self.run_simulated_program(
                build_config.get_executable_filename(),
                project.get_directory(),
                simconfig,
                valgrind)
            if sprocess is None:
                return
            simulation = self.new_simulation()
            simulation.quit_on_shutdown = True
            simulation.set_callback(
                "inited",
                lambda: self.window.add_tab(simview.SimViewTab(self, simulation)))
            simulation.set_callback("shutdown", lambda: sprocess.shutdown())
            simulation.connect("localhost", port)

        if self.project.get_simulator_net() is None:
            self.console_write("No net is selected for simulations\n", "error")
            return

        simconfig = self.project.get_simconfig()
        if simconfig.parameters_values is None:
            if not self.open_simconfig_dialog():
                return

        build_config = self.project.get_build_config("simulation")
        self.console_write("Preparing simulation ...\n")
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
        return simulation

    def connect_to_application(self):
        def inited():
            self.console_write("Connected\n", "success")
            self.window.add_tab(simview.SimViewTab(self, simulation, "{0}:{1}".format(host, port)))

        address = simview.connect_dialog(self.window);
        if address is None:
            return

        host = address[0]
        port = address[1]
        simulation = self.new_simulation()
        simulation.set_callback("inited", inited)
        self.console_write("Connecting to {0}:{1} ...\n".format(host, port))
        simulation.connect(host, port)

    def get_settings(self, name):
        return self.settings[name]

    def set_settings(self, name, value):
        self.settings[name] = value

    def _project_changed(self):
        self.nv.net_changed()

    def _project_filename_changed(self):
        self.window.set_title("Kaira - {0} ({1})" \
            .format(self.project.get_name(), self.project.get_extenv_name()))

    def _run_build_program(self, name, args, directory, build_ok_callback):
        def on_exit(code):
            if build_ok_callback and code == 0:
                build_ok_callback()
        def on_line(line, stream):
            self._process_error_line(line, None)
            return True
        p = process.Process(name, on_line, on_exit)
        p.cwd = directory
        p.start(args)

    def _run_makefile(self, project, build_directory, build_ok_callback=None, target=None):
        args = []
        if target is not None:
            args.append(target)
        self._run_build_program("make", args, build_directory, lambda: build_ok_callback(project))

    def start_build(self, proj, build_config, build_ok_callback):
        if self.get_settings("save-before-build"):
            self._save_project(silent = True)

        extra_args = [ "--build", build_config.directory ]
        self._start_ptp(proj,
                        build_config,
                        lambda lines: self._run_makefile(proj,
                                                         build_config.directory,
                                                         build_ok_callback),
                        extra_args = extra_args)

    def _start_ptp(self, proj, build_config, build_ok_callback=None, extra_args=[]):
        stdout = []
        def on_exit(code):
            error_messages = {}
            if build_ok_callback and code == 0:
                self.project.set_error_messages(error_messages)
                build_ok_callback(stdout)
            else:
                for line in stdout:
                    self._process_error_line(line, error_messages)
                self.project.set_error_messages(error_messages)
                self.console_write("Building failed\n", "error")
        def on_line(line, stream):
            #self.console_write(line)
            stdout.append(line)
            return True
        if not self.export_project(proj, build_config):
            return
        p = process.Process(paths.PTP_BIN, on_line, on_exit)
        p.cwd = proj.get_directory()
        args = [ build_config.get_export_filename() ]

        if self.get_settings("ptp-debug"):
            args.insert(0, "--debug")

        p.start(args + extra_args)

    def _try_make_error_with_link(self, id_string, item_id, pos, message):
        search = re.search("^\d+:", message)
        line_no = int(search.group(0)[:-1]) if search else 0

        if id_string == "head":
            self.console_write_link(id_string + (":" + str(line_no) if line_no else ""),
                lambda: self.edit_head(line_no))
            self.console_write(message[message.find(":"):] if line_no else ":" + message)
            return True

        # 2 is subtracted because #LINE directive is place before function definition, but
        # we jump at the line counted from the beginnging of user defined text
        # 1 for function definition, 1 for line with {

        if search:
            line_no -= 2

        if pos in ["getstring", "pack", "unpack"] and item_id is None:
            item = self.project.find_extern_type(id_string)
            position = (pos, line_no)
            self.console_write_link(id_string + "/" + pos + (":" + str(line_no) if line_no else ""),
                lambda: self.extern_type_functions_edit(item, position))
            self.console_write(message[message.find(":"):] if line_no else ":" + message)
            return True

        item = self.project.get_item(item_id)
        if pos == "function" and item.is_transition():
            self.console_write_link(str(item_id) + "/" + pos +
                                        (":" + str(line_no) if line_no else ""),
                                    lambda: self.transition_edit(item, line_no))
            self.console_write(message[message.find(":"):] if line_no else ":" + message)
            return True
        if pos == "init_function" and item.is_place():
            self.console_write_link(str(item_id) + "/" + pos +
                                       (":" + str(line_no) if line_no else ""),
                                    lambda: self.place_edit(item, line_no))
            self.console_write(message[message.find(":"):] if line_no else ":" + message)
            return True
        if pos == "user_function":
            self.console_write_link(item.get_name() + (":" + str(line_no) if line_no else ""),
                lambda: self.function_edit(item, line_no))
            self.console_write(message[message.find(":"):] if line_no else ":" + message)
            return True
        return False

    def _process_error_line(self, line, error_messages):
        if line.startswith("*"):
            sections = line[1:].split(":",1)
            if "/" in sections[0]:
                id_string, pos = sections[0].split("/")
            else:
                id_string = sections[0]
                pos = None
            try:
                item_id = int(id_string)
            except ValueError:
                item_id = None
            if self._try_make_error_with_link(id_string, item_id, pos, sections[1]):
                return
            if error_messages is None:
                self.console_write(line)
            else:
                d = error_messages.setdefault(item_id, {})
                lines = d.setdefault(pos, [])
                lines.append(sections[1].strip())
        else:
            self.console_write(line)

    def _directory_choose_dialog(self, title):
        dialog = gtk.FileChooserDialog(title, self.window, gtk.FILE_CHOOSER_ACTION_SELECT_FOLDER,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)
        try:
            if dialog.run() == gtk.RESPONSE_OK:
                return dialog.get_filename()
            else:
                return None
        finally:
            dialog.destroy()

    def _open_welcome_tab(self):
        label = gtk.Label()
        line1 = "<span size='xx-large'>Kaira</span>\nv{0}\n\n".format(VERSION_STRING)
        line2 = "News &amp; documentation can be found at\n"
        line3 = "<a href='http://verif.cs.vsb.cz/kaira'>http://verif.cs.vsb.cz/kaira</a>"
        label.set_markup(line1 + line2 + line3)
        label.set_justify(gtk.JUSTIFY_CENTER)
        self.window.add_tab(Tab("Welcome", label, has_close_button = False))

    def import_project(self):
        dialog = gtk.FileChooserDialog("Import project",
                                       self.window,
                                       gtk.FILE_CHOOSER_ACTION_OPEN,
                                        (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                            gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)
        try:
            self._add_project_file_filters(dialog)
            response = dialog.run()
            if response == gtk.RESPONSE_OK:
                filename = dialog.get_filename()
                if filename[-5:] != ".proj":
                    filename = filename + ".proj"
                loader.import_project(self.project, filename)
        finally:
            dialog.destroy()

if __name__ == "__main__":
    args = sys.argv[1:] # Remove "app.py"
    app = App(args)
    app.run()

