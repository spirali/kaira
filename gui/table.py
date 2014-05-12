#
#    Copyright (C) 2014 Martin Surkovsky
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

import numpy as np

class Table(object):

    def __init__(self, columns, rows_number=10, init_data=True):
        """ An initialization of a table.
        Note: there should not be used general PyObject type ('O' description),
        because of an error in NumPy indexing. (The `select` method would not
        work).

        Arguments:
        columns -- a list of couples (name, data type)
        rows_number -- an initializing number of rows
        """
        self.columns_number = len(columns)
        if not columns:
            self.header, self.types = [], []
            columns = None
        else:
            self.header, self.types = map(list, zip(*columns))
        self.rows_number = rows_number
        if init_data:
            self.data = np.ma.zeros((self.rows_number,), dtype=columns)

        self.last_row_index = 0

    @classmethod
    def create_from_data(cls, data):
        assert isinstance(data, np.ma.core.MaskedArray), \
            "The data must be of numpy masked array type."

        columns = data.dtype.descr
        rows_number = len(data)
        t = Table(columns, rows_number)
        if isinstance(data, np.ma.core.MaskedArray):
            t.data = data
        else:
            t.data = np.ma.zeros((rows_number,), dtype=columns)
            t.data.data = data
        t.last_row_index = rows_number
        return t

    def __getitem__(self, key):
        return self.data[key]

    def __len__(self):
        return self.last_row_index

    def __iter__(self):
        self._index = 0
        return self

    def next(self):
        if self._index == self.last_row_index:
            raise StopIteration

        # returns modified data; masked values are replaced by None
        data, mask = self.data.data[self._index], self.data.mask[self._index]
        try:
            ndata, nmask = iter(data), iter(mask)
            raw_data = zip(data, mask)
        except TypeError:
            # data and mask are both one element
            raw_data = [(data, mask)]

        row = [None if masked else item for item, masked in raw_data]

        self._index += 1
        return row

    def add_row(self, row):
        row = tuple(row)
        assert len(row) == self.columns_number, \
               "The row has to have the same length as the table has columns."

        if self.last_row_index >= self.rows_number: # resize
            self.rows_number *= 2
            self.data = np.ma.resize(self.data, self.rows_number)

        for i, item in enumerate(row): # append row
            if item is None: # invalid values
                self.data.mask[self.last_row_index][i] = True
            else:
                self.data.mask[self.last_row_index][i] = False
                self.data.data[self.last_row_index][i] = item
        self.last_row_index += 1

    def trim(self):
        self.data = np.ma.resize(self.data, self.last_row_index)

    def get_column(self, column):
        return self.data.data[self._get_colum_name(column)]

    def select(self, columns=None, filters=[]):
        """ Select columns and filter data.

        Arguments:
        columns -- a list of names or indexes of columns
        filter -- a list of triples where the firs argument is a name or
        index of column, the second one is a compare function, and the last
        is compared value.
        """
        if columns is not None:
            if not isinstance(columns, list):
                columns = [columns]
            columns = [self._get_colum_name(column) for column in columns]

        if not isinstance(filters, list):
            filters = [filters]

        mask = np.ma.ones(self.last_row_index, dtype='bool')
        for col, f_cmp, value in filters:
            mask &= f_cmp(self.data[self._get_colum_name(col)], value)

        if columns is None:
            return self.data[mask]
        elif len(columns) == 1:
            columns = columns[0]
        return self.data[mask][columns]

    def _get_colum_name(self, column):
        if isinstance(column, int) and 0 <= column < self.columns_number:
            return self.header[column]
        elif column in self.header:
            return column
        else:
            raise Exception("Invalid '{0}' column.".format(column))

