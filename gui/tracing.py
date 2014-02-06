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

    def __init__(self, name, return_type, type_description):
        self.name = name
        self.return_type = return_type
        self.type_description = type_description # for NumPy

supported = {'int': '<i4', 'double': '<f8', 'std::string': 'O'}

def get_type_descrition(return_type):
    if return_type in supported:
        return supported[return_type]
    else:
        return None

def tracefn_dialog(mainwindow, trace_function):
    if trace_function is None:
        mainwindow.app.show_message_dialog(
            "No tracing function selected.",
            gtk.MESSAGE_WARNING)
        return False

    def expanded(expander, param_spec):
        if expander.get_expanded():
            expander.child.show_all()
        else:
            expander.child.hide_all()

    def change_description(widget, return_type, expander, td_entry):
        if widget is None or widget.get_active():
            if expander.child is not None:
                expander.remove(expander.child)

            hbox = gtk.HBox(None)
            label = gtk.Label("{0}: ".format(return_type))
            label.set_alignment(xalign=1.0, yalign=0.5)
            hbox.pack_start(label, True, True)

            if trace_function.return_type == return_type:
                td_entry.set_text(trace_function.type_description)
            else:
                td_entry.set_text(supported[return_type])

            hbox.pack_start(td_entry, False, False)
            hbox.show_all()
            expander.add(hbox)

    builder = gtkutils.load_ui("tracefn-dialog")
    dlg = builder.get_object("tracefn-dialog")

    try:
        name = builder.get_object("name")
        name.set_text(trace_function.name)

        expander = gtk.Expander("Specify type description")
        expander.connect("notify::expanded", expanded)
        td_entry = gtk.Entry()

        return_int = builder.get_object("return_int")
        return_int.connect(
            "toggled", change_description, "int", expander, td_entry)

        return_double = builder.get_object("return_double")
        return_double.connect(
            "toggled", change_description, "double", expander, td_entry)

        return_string = builder.get_object("return_string")
        return_string.connect(
            "toggled", change_description, "std::string", expander, td_entry)

        return_type = trace_function.return_type
        if return_type == "int":
            return_int.set_active(True)
        elif return_type == "double":
            return_double.set_active(True)
        else:
            return_string.set_active(True)
        change_description(None, return_type, expander, td_entry)

        dlg.vbox.pack_start(expander)
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
            trace_function.type_description = td_entry.get_text()
            return True
        return False
    finally:
        dlg.destroy()
