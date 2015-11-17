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

import re
import gtk
import gtkutils
import objectlist
import xml.etree.ElementTree as xml

import utils

command_parser = re.compile(
   "(?P<process>\d+) (?P<action>[SFTR])( ((?P<arg_int>\d+)|(?P<arg_str>.*)))?"
)

def sequence_dialog(sequence, mainwindow):
    builder = gtkutils.load_ui("sequence-dialog")
    dlg = builder.get_object("sequence-dialog")
    try:
        name = builder.get_object("name")
        name.set_text(sequence.name)
        name.select_region(0, -1)
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            sequence.name = name.get_text()
            return True
        return False
    finally:
        dlg.destroy()


class ControlSequenceException(Exception):
    pass

class ControlSequence:

    view = None

    def __init__(self, name=None, commands=None, element=None):
        if element is not None:
            self.name = element.get("name")
            text = element.text
            if text is None:
                self.commands = []
            else:
                self.commands = [ command for command in text.split("\n")
                                  if command ]
        else:
            self.name = name
            if commands is None:
                self.commands = []
            else:
                self.commands = commands

    def copy(self):
        return ControlSequence(self.name, self.commands)

    def as_xml(self):
        element = xml.Element("sequence")
        element.set("name", self.name)
        element.text = "\n".join(self.commands)
        return element

    def execute(self, on_fire, on_transition_start, on_transition_finish, on_receive):
        for i in xrange(len(self.commands)):
            self.execute_command(i, on_fire, on_transition_start, on_transition_finish, on_receive)

    def execute_command(self, i, on_fire, on_transition_start, on_transition_finish, on_receive):
        line = self.commands[i]
        match = command_parser.match(line)
        if match is None:
            raise ControlSequenceException("Invalid format: ", line)

        process = int(match.group("process"))
        action = match.group("action")

        if action == "T":
            if match.group("arg_int"):
                arg = match.group("arg_int")
            else:
                arg = match.group("arg_str")
            return on_fire(process, arg)
        elif action == "R":
            arg_int = match.group("arg_int")
            if arg_int is None:
                raise ControlSequenceException("Invalid format of receive")
            return on_receive(process, int(arg_int))
        elif action == "S":
            if match.group("arg_int"):
                arg = match.group("arg_int")
            else:
                arg = match.group("arg_str")
            return on_transition_start(process, arg)
        else:
            assert action == "F"
            return on_transition_finish(process)

    def get_commands_size(self):
        return len(self.commands)

    def add_fire(self, process, transition):
        self.commands.append("{0} T {1}".format(process, transition))
        if self.view:
            self.view.add_fire(process, transition)

    def add_transition_start(self, process, transition):
        self.commands.append("{0} S {1}".format(process, transition))
        if self.view:
            self.view.add_transition_start(process, transition)

    def add_transition_finish(self, process):
        self.commands.append("{0} F".format(process))
        if self.view:
            self.view.add_transition_finish(process)

    def add_receive(self, process, from_process):
        self.commands.append("{0} R {1}".format(process, from_process))
        if self.view:
            self.view.add_receive(process, from_process)


class SequenceView(gtkutils.SimpleList):

    def __init__(self, sequence=None, show_init_state=None):
        gtkutils.SimpleList.__init__(
            self, (("P", str), ("Action|markup", str), ("Arg", str)))
        if show_init_state:
            self.append(("", "<span background='grey'>Init</span>", ""))
        if sequence:
            self.load_sequence(sequence)

    def load_sequence(self, sequence):
        self.clear()
        sequence.execute(self.add_fire,
                         self.add_transition_start,
                         self.add_transition_finish,
                         self.add_receive)

    def add_fire(self, process_id, transition):
        self.append((str(process_id),
                     "<span background='green'>Fire</span>",
                     transition))

    def add_transition_start(self, process_id, transition):
        self.append((str(process_id),
                     "<span background='lightgreen'>StartT</span>",
                     transition))

    def add_transition_finish(self, process_id):
        self.append((str(process_id),
                     "<span background='#FF7070'>FinishT</span>",
                     ""))

    def add_receive(self, process_id, from_process):
        self.append((str(process_id),
                     "<span background='lightblue'>Receive</span>",
                     str(from_process)))


class SequenceListWidget(gtk.HPaned):

    def __init__(self, app, project):
        gtk.HPaned.__init__(self)
        self.app = app
        self.project = project
        self.export_transition_id = True

        vbox = gtk.VBox()
        button = gtk.Button(stock=gtk.STOCK_REMOVE)
        button.connect("clicked", lambda w: self._remove_sequence(
                self.objlist.selected_object()))
        vbox.pack_start(button, False, True)

        btn_box = gtk.HButtonBox()
        btn_box.set_layout(gtk.BUTTONBOX_START)
        button = gtk.CheckButton("Export transition IDs")
        button.set_active(self.export_transition_id)
        button.connect("toggled", self._cb_export_transition_id)
        btn_box.add(button)

        button = gtk.Button("Export")
        button.connect("clicked", lambda w: self._export_control_sequence(
                self.objlist.selected_object()))
        btn_box.add(button)
        vbox.pack_start(btn_box, False, False)

        self.objlist = objectlist.ObjectList([("_", object), ("Sequences", str) ])
        self.objlist.object_as_row = lambda obj: [ obj, obj.name ]
        self.objlist.cursor_changed = self.on_cursor_changed
        self.objlist.set_size_request(175, 0)
        self.event = self.project.set_callback(
            "sequences_changed",
            lambda: self.objlist.refresh(project.sequences))
        vbox.pack_start(self.objlist, True, True)
        self.pack1(vbox, False)

        self.view = SequenceView()
        self.pack2(self.view, True)
        self.show_all()

        self.objlist.fill(project.sequences)

    def close(self):
        self.event.remove()

    def on_cursor_changed(self, obj):
        if obj is None:
            self.view.clear()
        else:
            self.view.load_sequence(obj)

    def _remove_sequence(self, obj):
        if obj:
            self.project.remove_sequence(obj)

    def _export_control_sequence(self, sequence):
        VERSION = "1.0"
        TYPE = "transition_id" if self.export_transition_id else "transition_name"

        if sequence:
            dialog = gtk.FileChooserDialog("Export Control Sequence",
                                           self.app.window,
                                           gtk.FILE_CHOOSER_ACTION_SAVE,
                                           (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                           gtk.STOCK_SAVE, gtk.RESPONSE_OK))
            dialog.set_default_response(gtk.RESPONSE_OK)
            dialog.set_current_name("{0}.kcs.xml".format(sequence.name))

            skcs_filter = gtk.FileFilter() # Kaira Control Sequence
            skcs_filter.set_name("Control Sequence (.kcs.xml)")
            dialog.add_filter(skcs_filter)

            try:
                response = dialog.run()
                filename = dialog.get_filename()
            finally:
                dialog.destroy()

            net = self.project.build_net

            transitions = {}
            for t in net.transitions():
                transitions["#{0}".format(t.id)] = t
            for t in net.transitions():
                transitions[utils.sanitize_name(t.get_name())] = t

            running_transitions = {}
            if response == gtk.RESPONSE_OK:
                cmdlines = "\n"
                for command in sequence.commands:
                    match = command_parser.match(command)
                    if match is None:
                        raise ControlSequenceException("Invalid format: ", command)

                    process = int(match.group("process"))
                    action = match.group("action")

                    if action == "T" or action == "S":
                        arg = match.group("arg_int")
                        if arg is None:
                            arg = match.group("arg_str")
                        if not transitions.has_key(arg):
                            raise ControlSequenceException(
                                    "Transition '{0}' not found.".format(arg))

                        t = transitions[arg]
                        if self.export_transition_id:
                            tid = t.id
                        else:
                            tid = utils.sanitize_name(t.get_name_or_id())
                        cmdlines += "{0} {1} {2}\n".format(process, action, tid)
                        if action == "S":
                            if running_transitions.has_key(process):
                                running_transitions[process].push(t.id)
                            else:
                                running_transitions[process] = [t.id]
                    elif action == "R":
                        arg_int = match.group("arg_int")
                        if arg_int is None:
                            raise ControlSequenceException("Invalid format of receive.")
                        cmdlines += "{0}\n".format(command)
                    else:
                        assert action == "F"
                        if not running_transitions.has_key(process) or \
                                not running_transitions[process]:
                            raise ControlSequenceException(
                                "Invalid sequence. Transition fire action is missing.")

                        tid = running_transitions[process].pop()
                        cmdlines += "{0} {1} {2}\n".format(process, action, tid)

                element = xml.Element("sequence")
                element.set("name", sequence.name)
                element.set("type", TYPE)
                element.set("version", VERSION)
                element.text = cmdlines

                tree = xml.ElementTree(element)
                tree.write(filename)

    def _cb_export_transition_id (self, widget):
        self.export_transition_id = widget.get_active()
