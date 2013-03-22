#
#    Copyright (C) 2010, 2011, 2012 Stanislav Bohm
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
from runview import NetInstanceView, NetInstanceCanvasConfig

class SimViewTab(mainwindow.Tab):
    def __init__(self, app, simulation, tabname="Simulation", mainmenu_groups=()):
        self.simulation = simulation
        simview = SimView(app, simulation)
        mainwindow.Tab.__init__(self, tabname, simview, mainmenu_groups=mainmenu_groups)

    def close(self):
        mainwindow.Tab.close(self)
        self.simulation.shutdown()


class SimCanvasConfig(NetInstanceCanvasConfig):

    def on_item_click(self, item, position):
        if item.kind == "box" and item.owner.is_transition():
               self.view.fire_transition(item.owner)
        else:
            NetInstanceCanvasConfig.on_item_click(self, item, position)

    def set_highlight(self):
        NetInstanceCanvasConfig.set_highlight(self)
        enabled = self.perspective.get_enabled_transitions()
        for transition in self.net.transitions():
            if transition.id in enabled:
               transition.box.highlight = (0, 255, 0, 0.85)



class SimView(NetInstanceView):

    config_class = SimCanvasConfig

    def __init__(self, app, simulation):
        NetInstanceView.__init__(self, app, SimCanvasConfig(self))
        self.simulation = simulation
        self.set_runinstance(self.simulation.runinstance)
        simulation.set_callback("changed", self._simulation_changed)

    def _simulation_changed(self):
        self.set_runinstance(self.simulation.runinstance)

    def on_item_click(self, item):
        NetInstanceView.on_item_click(self, item)
        if item.is_transition():
            self.fire_transition(item)

    def fire_transition(self, transition):
        perspective = self.get_perspective()
        ids = [ i.process_id for i in perspective.net_instances.values()
                if i.enabled_transitions is not None and transition.id in i.enabled_transitions ]
        if not ids:
            return
        process_id = self.simulation.random.choice(ids)
        self.simulation.fire_transition(transition.id, process_id)

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
