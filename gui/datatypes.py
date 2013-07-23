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

from tracelog import TraceLog

"""Supported types for extensions."""

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

        self.loaders = {}
        self.savers = {}

    def load_source(self, filename, *args):
        # getting file extensions (after first dot)
        splitedname = filename.split(".")
        if len(splitedname) >= 2:
            file_extension = splitedname[-1]
        else:
            raise NoLoaderExists("empty file extension")

        if file_extension in self.loaders:
            fn_load = self.loaders[file_extension]
            data = fn_load(filename, *args)
            return extensions.Source(filename, self, data)
        else:
            raise NoLoaderExists(self.name)

    def store_source(self, data, *args):
        pass # FIX: implemet me!

    def get_view(self, data, **kwargs):
        return None

    def register_load_function(self, extension, function):
        self.loaders[extension] = function

    def register_save_function(self, extension, function):
        self.savers[extension] = function


# ******************************************************************************
# supported types
types_repository = []

# Tracelog type
t_tracelog = Type("Kaira tracelog", "Tracelog", ["kth"])
def load_kth(filename):
    return TraceLog(filename)
t_tracelog.register_load_function("kth", load_kth)
def tracelog_view(data, app):
    return runview.RunView(app, data)
t_tracelog.get_view = tracelog_view
types_repository.append(t_tracelog)

# Control sequence type
t_contseq = Type("Kaira control sequence", "Cont. seq.", ["kcs"])
def load_kcs(filename):
    return filename
t_contseq.register_load_function("kcs", load_kcs)
types_repository.append(t_contseq)
