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
import gtkutils
import mainwindow
import controlseq
from netview import NetView, NetViewCanvasConfig

class SimViewTab(mainwindow.Tab):

    def __init__(self, app, simulation, tabname="Simulation", mainmenu_groups=()):
        self.simulation = simulation
        simview = SimView(app, simulation)
        mainwindow.Tab.__init__(self, tabname, simview, mainmenu_groups=mainmenu_groups)

    def close(self):
        mainwindow.Tab.close(self)
        self.simulation.shutdown()


class SimCanvasConfig(NetViewCanvasConfig):

    simulation = None
    simview = None

    def on_item_click(self, item, position):
        if item.kind == "box" and item.owner.is_transition():
            self.fire_transition(item.owner)
        elif item.kind == "activation":
            if not self.check_last_active():
                return
            process_id, transition = item.owner
            if self.simview.button_auto_receive.get_active():
                callback = lambda: self.simulation.receive_all()
            else:
                callback = None
            self.simulation.finish_transition(process_id, callback)
        elif item.kind == "packet" and item.packet_data is not None:
            if not self.check_last_active():
                return
            process_id, origin_id = item.packet_data
            self.simulation.receive(process_id, origin_id)
        else:
            NetViewCanvasConfig.on_item_click(self, item, position)

    def check_last_active(self):
        if not self.simulation.is_last_instance_active():
            self.simview.app.console_write("A history of simulation is displayed, it cannot be changed\n", "error")
            return False
        else:
            return True

    def fire_transition(self, transition):
        if not self.check_last_active():
            return
        perspective = self.view.get_perspective()
        ids = [ i.process_id for i in perspective.net_instances.values()
                if i.enabled_transitions is not None and transition.id
                in i.enabled_transitions ]
        if not ids:
            return
        process_id = self.simulation.random.choice(ids)
        if self.simview.button_auto_receive.get_active():
            callback = lambda: self.simulation.receive_all()
        else:
            callback = None
        self.simulation.fire_transition(transition.id, process_id,
                                        self.simview.get_fire_phases(),
                                        callback)

    def set_highlight(self):
        NetViewCanvasConfig.set_highlight(self)
        enabled = self.perspective.get_enabled_transitions()
        for transition in self.net.transitions():
            if transition.id in enabled:
               transition.box.highlight = (0, 255, 0, 0.85)


class SimView(gtk.VBox):

    def __init__(self, app, simulation):
        gtk.VBox.__init__(self)
        self.app = app
        self.simulation = simulation

        self.pack_start(self._toolbar(), False, False)
        self.netview = NetView(app, None, other_tabs=(("History", self._history()),))
        self.config = SimCanvasConfig(self.netview)
        self.config.simulation = simulation
        self.config.simview = self
        self.netview.set_config(self.config)
        self.pack_start(self.netview, True, True)

        self.netview.set_runinstance(self.simulation.runinstance)
        simulation.set_callback("changed", self._simulation_changed)

        self.button_run_phase12.set_active(True)
        self.button_auto_receive.set_active(True)
        self.show_all()

    def get_fire_phases(self):
        if self.button_run_phase1.get_active():
            return 1
        else:
            return 2

    def save_as_svg(self, filename):
        self.netview.save_as_svg(filename)

    def on_cursor_changed(self):
        path = self.sequence_view.get_selection_path()
        if path is None:
            return
        self.simulation.set_runinstance_from_history(path[0])

    def _history(self):
        box = gtk.VBox()

        self.sequence_view = controlseq.SequenceView()
        self.sequence_view.set_size_request(130, 100)
        self.simulation.sequence.view = self.sequence_view
        self.sequence_view.connect_view("cursor-changed",
                                        lambda w: self.on_cursor_changed())
        box.pack_start(self.sequence_view, True, True)

        button = gtk.Button("Show current")
        button.connect("clicked",
                       lambda w: self.simulation.set_runinstance_from_history(-1))
        self.show_current_button = button

        box.pack_start(button, False, False)
        button = gtk.Button("Save sequence")
        button.connect("clicked",
                       lambda w: self.app.save_sequence_into_project(
                            self.simulation.sequence.copy()))
        box.pack_start(button, False, False)
        return box

    def _simulation_changed(self, new_state):
        self.show_current_button.set_sensitive(not self.simulation.is_last_instance_active())
        if new_state:
            self.sequence_view.unselect_all()
        self.netview.set_runinstance(self.simulation.runinstance)

    def _toolbar(self):
        toolbar = gtk.Toolbar()

        button = gtk.RadioToolButton(None)
        button.set_tooltip_text("Start transition")
        button.set_stock_id(gtk.STOCK_GO_FORWARD)
        toolbar.add(button)
        self.button_run_phase1 = button

        button = gtk.RadioToolButton(self.button_run_phase1, None)
        button.set_tooltip_text("Start & finish transition")
        button.set_stock_id(gtk.STOCK_GOTO_LAST)
        toolbar.add(button)
        self.button_run_phase12 = button

        toolbar.add(gtk.SeparatorToolItem())

        button = gtk.ToolButton(None)
        button.set_tooltip_text("Receive all packets")
        button.set_stock_id(gtk.STOCK_GOTO_BOTTOM)
        button.connect("clicked",
                        lambda w: self.simulation.receive_all(
                            self.netview.get_perspective().get_process_ids()))
        toolbar.add(button)

        button = gtk.ToggleToolButton(None)
        button.set_tooltip_text(
            "Automatically call 'Receive all packets' after any transition action")
        button.set_stock_id(gtk.STOCK_EXECUTE)
        toolbar.add(button)
        self.button_auto_receive = button
        return toolbar


def connect_dialog(mainwindow):
    builder = gtkutils.load_ui("connect-dialog")
    dlg = builder.get_object("connect-dialog")
    try:

        host = builder.get_object("host")
        port = builder.get_object("port")
        port.set_value(10000)

        dlg.set_title("Connect")
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            return (host.get_text(), int(port.get_value()))
        return None
    finally:
        dlg.destroy()
