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

import extensions
from utils import get_file_extension

# User's imports
import gtk
import csv
import gtkutils
import settingswindow
import runview
from tracelog import TraceLog

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
        self.setting = None

        self.loaders = {}
        self.savers = {}

    def load_source(self, filename, app, setting=None):
        file_extension = get_file_extension(filename)
        if file_extension is None:
            raise NoLoaderExists("empty file extension")

        if file_extension in self.loaders:
            fn_load = self.loaders[file_extension]
            data = fn_load(filename, app, setting)
            if data is None:
                return None
            return extensions.Source(filename, self, data, True)
        else:
            raise NoLoaderExists("{0} ({1})".format(self.name, file_extension))

    def store_source(self, data, filename, file_extension, app, setting=None):
        if file_extension is None:
            raise NoSaverExists("empty file extension")

        if file_extension in self.savers:
            fn_save = self.savers[file_extension]
            fn_save(data, filename, file_extension, app, setting)
        else:
            raise NoSaverExists("{0} ({1})".format(self.name, file_extension))

    def get_view(self, data, app):
        return None

    def register_load_function(self, extension, function):
        self.loaders[extension] = function

    def register_save_function(self, extension, function):
        self.savers[extension] = function


# *****************************************************************************
# module functions
def file_extension_to_type(file_extension):
    for type in types_repository:
        for type_file_extension in type.files_extensions:
            if file_extension == type_file_extension:
                return type
    return None


# *****************************************************************************
# supported types

# Standard data types
t_string = Type("String", "string", [])

# -----------------------------------------------------------------------------
# Tracelog type
t_tracelog = Type("Kaira tracelog", "Tracelog", ["kth"])
def load_kth(filename, app, setting=None):
    if filename is None:
        return
    return app._catch_io_error(lambda: TraceLog(filename))
t_tracelog.register_load_function("kth", load_kth)

def tracelog_view(data, app):
    return runview.RunView(app, data)
t_tracelog.get_view = tracelog_view

types_repository.append(t_tracelog)

# -----------------------------------------------------------------------------
# Table type
t_table = Type("Table", "Table", ["csv"])

def show_csv_setting_dialog(parent_window):
    sw = settingswindow.SettingWidget()

    sw.add_combobox("delimiter",
                    "Delimiter",
                    [("Tab", "\t"), ("Comma", ","),
                    ("Semicolon", ";"), ("Space", " ")],
                    default=1)

    sw.add_combobox("quotechar",
                    "Quote char",
                    [("Single quotes", "\'"), ("Double quotes", "\"")],
                    default=1)

    sw.add_radiobuttons("header",
                        "Header",
                        [("With header", True), ("Without header", False)],
                        default=1,
                        ncols=2)

    dialog = settingswindow.BasicSettingDialog(sw, "Setting", parent_window)
    dialog.set_size_request(400, 250)
    dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
    dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK, True)

    response = dialog.run()
    if response == gtk.RESPONSE_OK:
        dialog.destroy()
        delimiter = sw.get("delimiter")
        quotechar = sw.get("quotechar")
        has_header = sw.get("header")
        return (delimiter, quotechar, has_header)

    dialog.destroy()
    return None

def load_csv(filename, app, setting=None):
    if setting is None:
        setting = show_csv_setting_dialog(app.window)
        t_table.setting = setting
    if setting is None:
        return # setting was canceled

    delimiter, quotechar, has_header = setting
    with open(filename, "rb") as csvfile:
        csvreader = csv.reader(
            csvfile, delimiter=delimiter, quotechar=quotechar)

        data = []
        try:
            if has_header:
                header = csvreader.next()
            else:
                row = csvreader.next()
                data.append(row)
                count = len(row)
                header = ["V{0}".format(i) for i in xrange(count)]
        except StopIteration:
            return (["V0"], [])

        for row in csvreader:
            data.append(row)
        return (header, data)
t_table.register_load_function("csv", load_csv)

def store_csv(data, filename, file_extension, app, setting=None):
    header, rows = data
    if setting is None:
        setting = show_csv_setting_dialog(app.window)
    delimiter, quotechar, has_header = setting
    with open("{0}.{1}".format(filename, file_extension), "wb") as csvfile:
        csvwriter = csv.writer(
            csvfile, delimiter=delimiter, quotechar=quotechar)
        if has_header:
            csvwriter.writerow(header)
        for row in rows:
            csvwriter.writerow(row)
t_table.register_save_function("csv", store_csv)

def csv_view(data, app):
    header, rows = data
    colnames = [(title, str) for title in header]

    view = gtkutils.SimpleList(colnames)
    idx = 1
    for row in rows:
        try:
            view.append(row)
            idx += 1
        except ValueError:
            required_len = len(header) if header is not None else len(rows[0])
            msg = ("Row sequence has wrong length. It must have {0} items"
                    " instead of {1}.\nThe problem row is index is {2}.".
                        format(required_len, len(row), idx))
            app.show_message_dialog(msg, gtk.MESSAGE_WARNING)
    return view
t_table.get_view = csv_view

types_repository.append(t_table)
