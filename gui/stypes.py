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

""" Supported types for actions selector """

class Type:

    def __init__(self, extension, display_name):
        self._extension = extension
        self._display_name = display_name

    def get_extension(self):
        return self._extension

    def get_display_name(self):
        return self._display_name

    def load_data(self, source):
        """ Loads the data and returns it. The default implementation throws
        the exception, loader is not implemented.

        Arguments:
        source -- source object; usually some stream

        """

        raise Exception("Method for loading data is not implemented!")

    def store_data(self, data):
        """ Stores the data to a disk. The default implementation throws
        the exception, method for storing data is not implemented.

        Arguments:
        data -- data object which should be stored

        """

        raise Exception("Method for storing data is not implemented!")

class KthType(Type):

    def __init__(self):
        Type.__init__(self, "kth", "Kaira tracelog header")

class CsType(Type):

    def __init__(self):
        Type.__init__(self, "cs", "Control sequence")

class TypesRepository:

    def __init__(self):
        self.types = {}

    def is_registered(self, type):
        return type.get_extension() in self.types

    def register_type(self, type):
        """ Registers a new type if the type is already registered than
        is throws the exception.

        Arguments:
        type -- the Type object

        """

        if not self.is_registered(type):
            self.types[type.get_extension()] = type
        else:
            raise Exception(
                "The type {0} - ({1}) is already registered".format(
                    type.get_extension(), type.get_display_name()))

    def deregister_type(self, type):
        if self.is_registered(type):
            del self.types[type.get_extension()]

    def get_type(self, extension):
        if extension in self.types:
            return self.types[extension]

        return None

    def get_registered_types(self):
        return [type for key, type in self.types.items()]

# default repository
repository = TypesRepository()
repository.register_type(KthType())
repository.register_type(CsType())
