#
#    Copyright (C) 2013 Martin Surkovsky
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

import runview
import extensions
from utils import get_file_extension

from tracelog import TraceLog

# User's imports
import gtk
import csv
import gtkutils

"""Supported types for extensions."""

types_repository = []

class DataTypeException(Exception):
    pass


class IncorrectTypeException(DataTypeException):

    def __init__(self, value):
        self.value = value
        message = "The type '{0}' does not contain " \
                   "any supported extensions!".format(str(value))
        DataTypeException.__init__(self, message)


class NoLoaderExists(DataTypeException):

    def __init__(self, value):
        message = "Loader for '{0}' does not exist!".format(str(value))
        DataTypeException.__init__(self, message)


class NoSaverExists(DataTypeException):

    def __init__(self, value):
        message = "Saver for '{0}' does not exists!".format(str(value))
        DataTypeException.__init__(self, message)


class Type(object):

    def __init__(self, name, short_name, files_extensions):
        """Initialize of type of types.

        Arguments:
        name -- name of type
        short_name -- short version of name
        files_extensions -- a list of supported file types

        """
        self.name = name
        self.short_name = short_name
        self.files_extensions = list(files_extensions)
        self.settings = None

        self.loaders = {}
        self.savers = {}

    def load_source(self, filename, app, settings=None):
        file_extension = get_file_extension(filename)
        if file_extension is None:
            raise NoLoaderExists("empty file extension")

        if file_extension in self.loaders:
            fn_load = self.loaders[file_extension]
            data = fn_load(filename, app, settings)
            return extensions.Source(filename, self, data)
        else:
            raise NoLoaderExists("{0} ({1})".format(self.name, file_extension))

    def store_source(self, data, filename, app, settings=None):
        file_extension = get_file_extension(filename)
        if file_extension is None:
            raise NoSaverExists("empty file extension")

        if file_extension in self.savers:
            fn_save = self.savers[file_extension]
            fn_save(data, filename, app, settings)
        else:
            raise NoSaverExists("{0} ({1})".format(self.name, file_extension))

    def get_view(self, data, app):
        return None

    def get_mainmenu_groups(self): # TODO: should it be removed?
        return []

    def register_load_function(self, extension, function):
        self.loaders[extension] = function

    def register_save_function(self, extension, function):
        self.savers[extension] = function


# ******************************************************************************
# module functions
def file_extension_to_type(file_extension):
    for type in types_repository:
        for type_file_extension in type.files_extensions:
            if file_extension == type_file_extension:
                return type
    return None


# ******************************************************************************
# supported types

# Standard data types
t_string = Type("String", "string", [])

# Tracelog type
t_tracelog = Type("Kaira tracelog", "Tracelog", ["kth"])
def load_kth(filename, app, settings=None):
    if filename is None:
        return
    return app._catch_io_error(lambda: TraceLog(filename))
t_tracelog.register_load_function("kth", load_kth)

t_tracelog.get_mainmenu_groups = lambda: ["screenshot"]

def tracelog_view(data, app):
    return runview.RunView(app, data)
t_tracelog.get_view = tracelog_view

types_repository.append(t_tracelog)

# CSV type
t_csv = Type("Comma separated values", "csv", ["csv"])

def show_csv_setting_dialog(parent_window):
    dialog = gtk.Dialog(title="Setting",
                        parent=parent_window,
                        flags=gtk.DIALOG_MODAL,
                        buttons=(gtk.STOCK_OK, gtk.RESPONSE_OK))

    delimiters = {"tab": '\t',
                  "comma": ',',
                  "semicolon": ';',
                  "space": ' '}
    quotemarks = {"single-quotes": '\'',
                  "double-quotes": '\"'}
    header = {"header-yes": True,
               "header-no": False}

    settings = {"delimiter": "comma",
               "quotemark": "double-quotes",
               "header": "header-no"}

    def cb_settings_change(settings, key, value):
        settings[key] = value

    hbox = gtk.HBox(False)
    label = gtk.Label()
    label.set_markup("<b>Delimiter:</b>")
    hbox.pack_start(label, False, False, 10)
    gtkutils.radio_buttons([("tab", "Tab"),
                            ("comma", "Comma"),
                            ("semicolon", "Semicolon"),
                            ("space", "Space")],
                           settings["delimiter"],
                           hbox,
                           lambda key: cb_settings_change(
                               settings, "delimiter", key))
    dialog.vbox.pack_start(hbox, False, False)

    hbox = gtk.HBox(False)
    label = gtk.Label()
    label.set_markup("<b>Quotation mark:</b>")
    hbox.pack_start(label, False, False, 10)
    gtkutils.radio_buttons([("single-quotes", "Single quotes"),
                            ("double-quotes", "Double quotes")],
                           settings["quotemark"],
                           hbox,
                           lambda key: cb_settings_change(
                               settings, "quotemark", key))
    dialog.vbox.pack_start(hbox, False, False)

    hbox = gtk.HBox(False)
    label = gtk.Label()
    label.set_markup("<b>Header:</b>")
    hbox.pack_start(label, False, False, 10)
    gtkutils.radio_buttons([("header-yes", "Yes"),
                            ("header-no", "No")],
                           settings["header"],
                           hbox,
                           lambda key: cb_settings_change(
                               settings, "header", key))
    dialog.vbox.pack_start(hbox, False, False)
    dialog.vbox.show_all()

    response = dialog.run()
    if response == gtk.RESPONSE_OK:
        dialog.destroy()

    return (delimiters[settings["delimiter"]],
            quotemarks[settings["quotemark"]],
            header[settings["header"]])

def load_csv(filename, app, settings=None):
    if settings is None:
        settings = show_csv_setting_dialog(app.window)
        t_csv.settings = settings
    delimiter, quotemark, header_yes = settings
    with open(filename, "rb") as csvfile:
        csvreader = csv.reader(
            csvfile, delimiter=delimiter, quotechar=quotemark)
        data = []
        if header_yes:
            header = csvreader.next()
        else:
            row = csvreader.next()
            data.append(row)
            count = len(row)
            header = ["V{0}".format(i) for i in xrange(count)]

        for row in csvreader:
            data.append(row)
        return (header, data)
t_csv.register_load_function("csv", load_csv)

def store_csv(data, filename, app, settings=None):
    header, rows = data
    if settings is None:
        settings = show_csv_setting_dialog(app.window)
    delimiter, quotemark, header_yes = settings
    with open(filename, "wb") as csvfile:
        csvwriter = csv.writer(
            csvfile, delimiter=delimiter, quotechar=quotemark)
        if header_yes:
            csvwriter.writerow(header)
        for row in rows:
            csvwriter.writerow(row)
t_csv.register_save_function("csv", store_csv)

def csv_view(data, app):
    header, rows = data
    colnames = [(title, str) for title in header]

    view = gtkutils.SimpleList(colnames)
    for row in rows:
        view.append(row) # TODO: catch bad adding; all rows must have the same count of items
    return view
t_csv.get_view = csv_view

types_repository.append(t_csv)
