#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                        2011 Ondrej Garncarz
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

## @file functions.py
#  The file containing classes and a function for a widget from
#  Project details > Functions.

import gtk
from objectlist import ObjectList
from codeedit import CodeEditor
import gtkutils

## The class granting the add/remove/edit functionality under
#  Project details > Functions.
class FunctionsWidget(ObjectList):

    ## The constructor of a widget.
    # @param project A working project.
    # @param app The running Kaira application.
    def __init__(self, project, app):
        defs = [("_", object), ("Name", str), ("Return type", str), ("Context", bool), ("Parameters", str) ]
        buttons = [
            (None, gtk.STOCK_ADD, self._add_function),
            (None, gtk.STOCK_REMOVE, self._remove_function),
            (None, gtk.STOCK_EDIT, self._edit_function),
            ("Edit _code", None, self._edit_function_code)
        ]

        ObjectList.__init__(self, defs, buttons)
        self.project = project
        self.app = app

        self.fill(project.functions)

    ## Returns a list of information about a function.
    #  @param obj An object encapsulating the function.
    #  @return The list containg the object itself, the function's name,
    #    return type, boolean about using context, and parameters.
    def object_as_row(self, obj):
        return [obj, obj.get_name(), obj.get_return_type(), obj.get_with_context(), obj.get_parameters()]

    ## Triggers editing of a definition of a function.
    #  @param selected The function.
    def row_activated(self, selected):
        self._edit_function_code(selected)

    ## Adds a function to the project.
    #  @param selected Not used.
    def _add_function(self, selected):
        obj = self.project.get_function_class()()
        if function_dialog(obj, self.app.window):
            self.add_object(obj)
            self.project.add_function(obj)

    ## Edits a declaration of a function.
    #  @param selected The function.
    def _edit_function(self, selected):
        if selected and function_dialog(selected, self.app.window):
            self.update_selected(selected)

    ## Removes a function from the project.
    #  @param selected The function.
    def _remove_function(self, selected):
        if selected:
            obj = self.get_and_remove_selected()
            self.project.remove_function(obj)

    ## Edits a definition of a function.
    #  @param obj The function.
    def _edit_function_code(self, obj):
        if obj is not None:
            if obj.check_definition():
                self.app.function_edit(obj)
            else:
                self.app.console_write("Invalid definition of function '%s'\n"
                    % obj.get_name(), "error")

## The class for editing a definition of a function. 
class FunctionEditor(CodeEditor):

    ## The constructor, starts editing a definition of a function.
    #  @param project A working project.
    #  @param function The function to edit.
    def __init__(self, project, function):
        self.function = function
        declaration = function.get_function_declaration()
        code = function.get_function_code()
        start = "\n{\n"
        end = "}\n"
        CodeEditor.__init__(self, project.get_syntax_highlight_key(), declaration + start, code, end, (2, 0))

    ## The event handler updating the function's definition.
    #  @param buffer Not used.
    def buffer_changed(self, buffer):
        self.function.set_function_code(self.get_text())

## Runs a dialog for editing a declaration of a function.
#  @param function The function.
#  @param mainwindow A main window of the application.
#  @return Was the function changed?
def function_dialog(function, mainwindow):
    builder = gtkutils.load_ui("function-dialog")
    dlg = builder.get_object("function-dialog")
    try:
        wname = builder.get_object("name")
        wname.set_text(function.get_name())

        wreturn = builder.get_object("return")
        wreturn.set_text(function.get_return_type())

        wparam = builder.get_object("parameters")
        wparam.set_text(function.get_parameters())

        wcontext = builder.get_object("context")
        wcontext.set_active(function.get_with_context())

        dlg.set_title("Function")
        dlg.set_transient_for(mainwindow)
        if dlg.run() == gtk.RESPONSE_OK:
            function.set_name(wname.get_text())
            function.set_return_type(wreturn.get_text())
            function.set_parameters(wparam.get_text())
            function.set_with_context(wcontext.get_active())
            return True
        return False
    finally:
        dlg.destroy()

