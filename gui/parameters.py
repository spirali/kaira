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

import gtk
import gtkutils
from objectlist import ObjectList
from project import Parameter

def parameters_dialog(parameter, mainwindow):
    builder = gtkutils.load_ui("parameter-dialog")
    dlg = builder.get_object("parameter-dialog")

    try:
        name = builder.get_object("name")
        name.set_text(parameter.name)

        desc = builder.get_object("description")
        desc.set_text(parameter.description)

        default = builder.get_object("default")
        default.set_text(parameter.default)

        typename = builder.get_object("type")
        typename.set_text(parameter.type)

        policy_mandatory = builder.get_object("policy_mandatory")
        policy_optional = builder.get_object("policy_optional")
        policy_constant = builder.get_object("policy_constant")

        if parameter.policy == "mandatory":
            policy_mandatory.set_active(True)
        elif parameter.policy == "optional":
            policy_optional.set_active(True)
        elif parameter.policy == "constant":
            policy_constant.set_active(True)
        else:
            raise Exception("Invalid parameter policy")

        default.set_text(parameter.default)

        dlg.set_title("Parameter")
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            parameter.name = name.get_text()
            parameter.description = desc.get_text()
            parameter.default = default.get_text()
            parameter.type = typename.get_text()

            if policy_mandatory.get_active():
                parameter.policy = "mandatory"
            if policy_optional.get_active():
                parameter.policy = "optional"
            if policy_constant.get_active():
                parameter.policy = "constant"
            parameter.changed()
            return True
        return False
    finally:
        dlg.destroy()

class ParametersWidget(ObjectList):

    def __init__(self, project, mainwindow):
        defs = [("_", object),
                ("Name", str),
                ("Policy", str),
                ("Type", str),
                ("Default", str),
                ("Description", str)]
        buttons = [
            (None, gtk.STOCK_ADD, self._add_parameter),
            (None, gtk.STOCK_REMOVE, self._remove_parameter),
            (None, gtk.STOCK_EDIT, self._edit_parameter)]

        ObjectList.__init__(self, defs, buttons)
        self.project = project
        self.mainwindow = mainwindow

        self.fill(project.get_parameters())

    def object_as_row(self, parameter):
        return [ parameter, parameter.name,
                parameter.policy, parameter.type, parameter.default,
                parameter.description]

    def row_activated(self, selected):
        self._edit_parameter(selected)

    def _add_parameter(self, selected):
        param = Parameter()
        if parameters_dialog(param, self.mainwindow):
            self.add_object(param)
            self.project.add_parameter(param)

    def _edit_parameter(self, selected):
        if selected and parameters_dialog(selected, self.mainwindow):
            self.update_selected(selected)

    def _remove_parameter(self, selected):
        if selected:
            param = self.get_and_remove_selected()
            self.project.remove_parameter(param)


