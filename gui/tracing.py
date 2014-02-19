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


import gtkutils
import gtk
import utils

class TraceFunction(utils.EqMixin):

    def __init__(self, name, return_type):
        self.name = name
        self.return_type = return_type

    @property
    def return_numpy_type(self):
        return utils.ctype_to_numpy_type(self.return_type)


def tracefn_dialog(mainwindow, trace_function):
    if trace_function is None:
        mainwindow.app.show_message_dialog(
            "No tracing function selected.",
            gtk.MESSAGE_WARNING)
        return False

    builder = gtkutils.load_ui("tracefn-dialog")
    dlg = builder.get_object("tracefn-dialog")

    try:
        name = builder.get_object("name")
        name.set_text(trace_function.name)

        return_int = builder.get_object("return_int")
        return_double = builder.get_object("return_double")
        return_string = builder.get_object("return_string")

        return_type = trace_function.return_type
        if return_type == "int":
            return_int.set_active(True)
        elif return_type == "double":
            return_double.set_active(True)
        else:
            return_string.set_active(True)

        dlg.vbox.show_all()

        dlg.set_title("Trace function")
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            if return_int.get_active():
                return_type = "int"
            elif return_double.get_active():
                return_type = "double"
            else:
                return_type = "std::string"
            trace_function.name = name.get_text()
            trace_function.return_type = return_type
            return True
        return False
    finally:
        dlg.destroy()
