#
#    Copyright (C) 2013, 2014 Martin Surkovsky
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

import csv

import gtk
import gtkutils
import settingswindow
import runview
import numpy as np
from tracelog import TraceLog
from table import Table

"""Supported types for extensions."""

types_repository = []


class Type(object):
    """This class serves as a basic class for all data types."""

    def __init__(self, name, short_name=None):
        """Initialize of type of types.

        Arguments:
        name -- a full name of a data type

        Keywords:
        short_name -- shorter version of a name (default: None)

        """
        self.name = name
        if short_name is None:
            self.short_name = name
        else:
            self.short_name = short_name

        """a dictionary with registered loaders for a specific file extension
        (file extension: load function)

        """
        self.loaders = {}

        """a dictionary with registered savers for a specific file extension
        (file extension: save function)

        """
        self.savers = {}
        self.default_saver = None

    def get_view(self, data, app):
        """Return a widget width visualized data or None if the visualization
        is not implemented.

        Arguments:
        data -- data for visualization
        app -- a reference to the main application

        """
        return None

    def register_load_function(self, suffix, function):
        """Register a loading function to a file suffix.

        Arguments:
        suffix -- a suffix of a filename (it specifies a type of data)
        function -- function which can load a files with given suffix

        """
        self.loaders[suffix] = function

    def register_store_function(self, suffix, function, default=False):
        """Register a saving function to a file suffix.

        Arguments:
        suffix -- a suffix of a filename (it specifies a type of data)
        function -- function which can store data to file with given suffix
        default -- specify whether the given function is default saver or not

        """
        self.savers[suffix] = function
        if default or self.default_saver is None:
            self.default_saver = suffix


# *****************************************************************************
# module functions
def get_type_by_suffix(suffix):
    for type in types_repository:
        if suffix in type.loaders:
            return type
    return None

def get_loader_by_suffix(suffix):
    for type in types_repository:
        loader = type.loaders.get(suffix)
        if loader is not None:
            return loader
    return None

def get_saver_by_suffix(suffix):
    for type in types_repository:
        saver = type.savers.get(suffix)
        if saver is not None:
            return saver
    return None

def get_load_file_filters():
    all_supported_types = gtk.FileFilter()
    all_supported_types.set_name("All supported files")

    result = [ all_supported_types ]
    for type in types_repository:
        patterns = [ "*." + s for s in type.loaders.keys() ]
        filter = gtk.FileFilter()
        filter.set_name("{0} ({1})".format(type.short_name, ", ".join(patterns)))
        result.append(filter)

        for pattern in patterns:
            filter.add_pattern(pattern)
            all_supported_types.add_pattern(pattern)
    return result

def get_save_file_filter(type):
    patterns = [ "*." + s for s in type.savers.keys() ]
    filter = gtk.FileFilter()
    filter.set_name("{0} ({1})".format(type.short_name, ", ".join(patterns)))
    for pattern in patterns:
        filter.add_pattern(pattern)
    return filter

# *****************************************************************************
# supported types

# -----------------------------------------------------------------------------
# Tracelog type
t_tracelog = Type("Kaira tracelog", "Tracelog")
def load_kth(filename, app, settings=None):
    if filename is None:
        return
    return (app._catch_io_error(lambda: TraceLog(filename, True)), settings)
t_tracelog.register_load_function("kth", load_kth)

def tracelog_view(data, app):
    return runview.RunView(app, data)
t_tracelog.get_view = tracelog_view

types_repository.append(t_tracelog)

# -----------------------------------------------------------------------------
# Table type
t_table = Type("Table")

def show_csv_settings_dialog(parent_window):
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
                        default=0,
                        ncols=2)

    sw.add_radiobuttons("types",
                        "Types",
                        [("With types", True), ("Without types", False)],
                        default=0,
                        ncols=2)

    dialog = settingswindow.BasicSettingDialog(sw, "Setting", parent_window)
    dialog.set_size_request(400, 250)
    dialog.add_button(gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL)
    dialog.add_button(gtk.STOCK_OK, gtk.RESPONSE_OK, True)

    response = dialog.run()
    if response == gtk.RESPONSE_OK:
        dialog.destroy()
        delimiter = dialog.get_setting("delimiter")
        quotechar = dialog.get_setting("quotechar")
        has_header = dialog.get_setting("header")
        has_types = dialog.get_setting("types")
        return (delimiter, quotechar, has_header, has_types)

    dialog.destroy()
    return None

def load_csv(filename, app, settings):
    if settings is None:
        settings = show_csv_settings_dialog(app.window)
    if settings is None:
        return (None, None) # settings was canceled

    delimiter, quotechar, has_header, has_types = settings
    with open(filename, "rb") as csvfile:
        csvreader = csv.reader(
            csvfile, delimiter=delimiter, quotechar=quotechar)

        try:
            types = None
            if has_types:
                types = csvreader.next()

            header = None
            if has_header:
                header = csvreader.next()

            row = csvreader.next()  # first row with data
        except StopIteration:
            table = Table([("V0", "object")], 0)
            return (table, None);

        if types is None:
            types = ["object"] * len(row)
        if header is None:
            header = ["V {0}".format(i + 1) for i in range(len(row))]
        cols_description = zip(header, types)

        table = Table(cols_description, 100)
        row = [None if value == '' else value for value in row]
        table.add_row(row) # add the first loaded row with data
        for row in csvreader:
            row = [None if value == '' else value for value in row]
            table.add_row(row);
        table.trim()
        return (table, settings)

t_table.register_load_function("csv", load_csv)

def store_csv(table, filename, app, settings):
    if settings is None:
        settings = show_csv_settings_dialog(app.window)
    if settings is None:
        return (False, None)

    delimiter, quotechar, has_header, has_types = settings
    with open(filename, "w") as csvfile:
        csvwriter = csv.writer(
            csvfile, delimiter=delimiter, quotechar=quotechar)
        if has_types:
            csvwriter.writerow(table.types)
        if has_header:
            csvwriter.writerow(table.header)
        for row in table:
            csvwriter.writerow(row)
    return (True, settings)

t_table.register_store_function("csv", store_csv)

def csv_view(table, app):
    colnames = [(title, str) for title in table.header]

    view = gtkutils.SimpleList(colnames)
    idx = 1
    for row in table:
        try:
            view.append(row)
            idx += 1
        except ValueError:
            msg = ("Row sequence has wrong length. It must have {0} items"
                    " instead of {1}.\nThe problem row is index is {2}.".
                        format(len(table.header), len(row), idx))
            app.show_message_dialog(msg, gtk.MESSAGE_WARNING)
    return view
t_table.get_view = csv_view

types_repository.append(t_table)
