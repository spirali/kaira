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

#from actionselector import Source
import re
import runview
import actionselector

from tracelog import TraceLog

""" Supported types for actions selector """

class IncorrectTypeException(Exception):

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "The type '{0}' does not contain " \
               "any supported extensions!".format(repr(self.value))

class NoLoaderExists(Exception):

    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "Loader for '{0}' does not exist!".format(repr(self.value))

class Type:
    """ Type gives similar types together. It is like types' container. """

    def __init__(self, name, short_name, extensions):
        """ Initialize of type of types.

        Arguments:
        name -- name of type
        short_name -- short version of name
        extensions -- array of supported types

        """
        self.name = name
        self.short_name = short_name
        if not isinstance(extensions, list):
            raise Exception("The extensions must be a list")
        self.extensions = extensions

        self._current_type = None

        self.loaders = {}
        self.savers = {}

    def get_id(self):
        if not self.extensions:
            raise IncorrectTypeException(self.name)

        return hash(tuple(self.extensions))

    def get_name(self):
        return self.name

    def get_short_name(self):
        return self.short_name

    def get_extensions(self):
        return self.extensions

    def compare(self, type):
        return self.get_id() == type.get_id()

    def load_source(self, filename, type, *args):

        # get extension (after first dot)
        extension = re.split("\.", filename)
        extension = ".".join(extension[1:])

        if extension in self.loaders:
            fn_load = self.loaders[extension]
            data = fn_load(filename, *args)
            return actionselector.Source(filename, type, data)
        else:
            raise NoLoaderExists(type.get_name())

    def store_source(self, data, *args):
        pass # FIX: implemet me!

    def get_view(self, data, *args):
        return None

    def _d_register_load_function(self, extension, function):
        self.loaders[extension] = function

    def _d_register_save_function(self, extension, function):
        self.savers[extension] = function

class CategoriesRepository:

    def __init__(self):
        self.types = {}

    def is_registered(self, type):
        return type.get_id() in self.types

    def register_type(self, type):
        """ Registers a new type if the type is already registered than
        is throws the exception.

        Arguments:
        type -- the Type object

        """

        if not self.is_registered(type):
            self.types[type.get_id()] = type
        else:
            raise Exception(
                "The type '{1}' is already registered".format(
                    type.get_name()))

    def deregister_type(self, type):
        if self.is_registered(type):
            del self.types[type.get_id()]

    def get_type(self, extension):
        for id, type in self.types.items():
            if extension in type.get_extensions():
                return type

        return None

    def get_registered_types(self):
        return [type for id, type in self.types.items()]

# ******************************************************************************
# supported types

class TracelogType(Type):

    def __init__(self):
        Type.__init__(self,
                      "Kaira tracelog",
                      "Tracelog",
                      ["kth"])

        self._d_register_load_function("kth", self.load_kth)

    def load_kth(self, filename): # TODO: processed the exceptions of TraceLog
        t = TraceLog(filename)
        return t

    def get_view(self, data, *args):
        app = args[0]
        rv = runview.RunView(app, data)
        return rv

class ControlSequenceType(Type):

    def __init__(self):
        Type.__init__(self,
                      "Control sequence",
                      "Cont. seq.",
                      ["kcs"])

# ******************************************************************************

# default repository
repository = CategoriesRepository()
repository.register_type(TracelogType())
repository.register_type(ControlSequenceType())
