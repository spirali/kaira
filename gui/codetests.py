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
import os
import shutil
import utils
import gtkutils
import process
import simview
from objectlist import ObjectList
from codeedit import CodeFileEditor


class CodeTest(utils.EqMixin):

    def __init__(self, project, name):
        self.project = project
        self.name = name

    def get_directory(self):
        return os.path.join(self.project.get_directory(),
                            "tests",
                            self.name)

    def get_filename(self):
        return os.path.join(self.get_directory(), self.name + ".cpp")

    def write_template(self):
        with open(self.get_filename(), "w") as f:
            f.write("#include \"{0}.h\"\n\n"
                       "int main(int argc, char **argv)\n"
                       "{{\n"
                       "\t// calib_init HAS TO be always the first\n"
                       "\tcalib_init(argc, argv);\n\n"
                       "\t// Put your code here\n\n"
                       "\treturn 0;\n}}\n".format(self.project.get_name()))

    def write_makefile(self):
        filename = os.path.join(self.get_directory(), "makefile")
        with open(filename, "w") as f:
            f.write("BUILD_DIR=../../lib\n\n"
                    "{0}: {0}.cpp\n"
                    "\t$(CXX) -o {0} {0}.cpp -I$(BUILD_DIR) -L$(BUILD_DIR)"
                    " $(CFLAGS) $(INCLUDE) $(LIBDIR) -l{1} $(LIBS)\n\n"
                    "include $(BUILD_DIR)/makefile"
                        .format(self.name, self.project.get_name()))

    def remove(self):
        shutil.rmtree(self.get_directory())

    def build(self, app, callback):
        app._run_build_program("make", [], self.get_directory(), callback, None)

    def get_executable_filename(self):
        return os.path.join(self.get_directory(), self.name)

    def run(self, app):
        def on_exit(code):
            app.console_write("Test '{0}' returned '{1}'.\n".format(self.name, code),
                              "success" if code == 0 else "error")
        def on_line(line, stream):
            app.console_write_output(line)
            return True

        app.console_write("Test '{0}' started.\n".format(self.name), "success")
        p = process.Process(self.get_executable_filename(), on_line, on_exit)
        p.cwd = self.get_directory()
        p.start()

    def simulation(self, app, valgrind):
        simconfig = app.project.get_simconfig()
        if simconfig.parameters_values is None:
            if not app.open_simconfig_dialog():
                return
        sprocess, port = app.run_simulated_program(
            self.get_executable_filename(),
            self.get_directory(),
            simconfig,
            valgrind)
        if sprocess is None:
            return
        simulation = app.new_simulation()
        simulation.quit_on_shutdown = True
        simulation.set_callback(
            "inited",
            lambda: app.window.add_tab(simview.SimViewTab(self, simulation)))
        simulation.set_callback("shutdown", lambda: sprocess.shutdown())
        simulation.connect("localhost", port)


def add_test(project, codetest):
    directory = project.get_directory()
    tests_directory = os.path.join(directory, "tests")
    utils.makedir_if_not_exists(tests_directory)

    if os.path.exists(codetest.get_directory()):
        return False
    os.makedirs(codetest.get_directory())
    codetest.write_template()
    codetest.write_makefile()
    return True

def collect_tests(project):
    directory = project.get_directory()
    tests_directory = os.path.join(directory, "tests")
    tests = []

    if not os.path.isdir(tests_directory):
        return tests

    for name in os.listdir(tests_directory):
        d = os.path.join(tests_directory, name)
        if os.path.isdir(d) and os.path.exists(os.path.join(d, name + ".cpp")):
            tests.append(CodeTest(project, name))
    return tests

def testname_dialog(mainwindow, codetest):
    builder = gtkutils.load_ui("testname-dialog")
    dlg = builder.get_object("testname-dialog")
    try:
        name = builder.get_object("name")
        name.set_text(codetest.name)
        name.select_region(0, -1)
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            codetest.name = name.get_text()
            return True
        else:
            return False
    finally:
        dlg.destroy()


class CodeTestList(gtk.VBox):

    def __init__(self, app):
        gtk.VBox.__init__(self)
        self.app = app

        box = gtk.HBox()
        hbox = gtk.HButtonBox()
        hbox.set_layout(gtk.BUTTONBOX_START)
        button = gtk.Button(stock = gtk.STOCK_ADD)
        button.connect("clicked", lambda w: self.add_test())
        hbox.add(button)
        button = gtk.Button(stock = gtk.STOCK_REMOVE)
        button.connect("clicked",
                       lambda w: self.remove_test(self.objlist.selected_object()))
        hbox.add(button)
        button = gtk.Button(stock = gtk.STOCK_EXECUTE)
        button.connect("clicked",
                       lambda w: self.execute(self.objlist.selected_object()))
        hbox.add(button)
        box.pack_start(hbox, False, False)

        self.switch = gtk.combo_box_new_text()
        self.switch.append_text("Build")
        self.switch.append_text("Build & Run")
        self.switch.append_text("Build & Traced Run")
        self.switch.append_text("Build & Run + Valgrind")
        self.switch.append_text("Simulation")
        self.switch.append_text("Simulation + Valgrind")
        self.switch.set_active(4) # Select 'Simulation' as default
        box.pack_start(self.switch, False, False)
        self.pack_start(box, False, False)

        hbox = gtk.HBox()

        self.objlist = ObjectList([("_", object), ("Tests", str) ])
        self.objlist.set_size_request(100, 100)
        self.objlist.object_as_row = lambda obj: [ obj, obj.name ]
        self.objlist.cursor_changed = self.cursor_changed
        hbox.pack_start(self.objlist, False, False)
        self.editor = CodeFileEditor(self.app.project.get_syntax_highlight_key())
        hbox.pack_start(self.editor)

        self.pack_start(hbox)

        tests = collect_tests(app.project)
        self.objlist.fill(tests)
        if tests:
            self.cursor_changed(tests[0])
            self.objlist.select_first()
        else:
            self.cursor_changed(None)

        self.show_all()

    def save(self):
        if self.editor.get_sensitive():
            self.editor.save()

    def cursor_changed(self, obj):
        if self.editor.get_sensitive():
            self.editor.save()
        if obj is None:
            self.editor.set_text("")
            self.editor.set_sensitive(False)
            return
        self.editor.set_sensitive(True)
        self.editor.load(obj.get_filename())

    def refresh_tests(self):
        tests = collect_tests(self.app.project)
        self.objlist.refresh(tests)
        self.cursor_changed(self.objlist.selected_object())

    def add_test(self):
        codetest = CodeTest(self.app.project, "")
        if testname_dialog(self.app.window, codetest):
            if not add_test(self.app.project, codetest):
                self.app.show_error_dialog(
                    "Test {0.name} already exists".format(codetest))
        else:
            return
        self.refresh_tests()
        self.objlist.select_object(codetest)
        self.cursor_changed(codetest)

    def remove_test(self, obj):
        if obj is not None:
            self.cursor_changed(None)
            obj.remove()
            tests = collect_tests(self.app.project)
            self.objlist.clear()
            self.objlist.fill(tests)
            self.objlist.select_first()

    def execute(self, obj):
        def build(callback):
            build_config = self.app.project.get_build_config("lib")
            self.app.start_build(self.app.project,
                                 build_config,
                                 lambda: obj.build(self.app, callback))

        target = self.switch.get_active_text()

        if target == "Build":
            build(lambda: self.app.console_write("Build finished\n", "success"))
        elif target == "Build & Run":
            build(lambda: obj.run(self.app))
        elif target == "Simulation":
            build(lambda: obj.simulation(self.app, False))
        elif target == "Simulation + Valgrind":
            build(lambda: obj.simulation(self.app, True))
        else:
            self.app.console_write(
                "Target '{0}' is not implemented.\n".format(target), "error")
