#
#    Copyright (C) 2011 Stanislav Bohm
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

import re

class PtpException(Exception):

    def __init__(self, message, source=None):
        if source is not None:
            message = source + ": " + message
        Exception.__init__(self, message)


class EqMixin(object):

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
            and self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self.__eq__(other)


class EqByIdMixin(object):

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
            and self.id == other.id)

    def __ne__(self, other):
        return not self.__eq__(other)


'''
    @param fn: Function f(a, b) that returns True if a depends on b
    @return: Tologicaly ordered list or NULL if there is no such ordering
'''
def topological_ordering(elements, fn):
    if len(elements) == 0:
        return elements
    result = []
    rest = elements[:]
    while True:
        picked = []
        for e in rest:
            for a in rest:
                if a is not e and fn(e, a):
                    break
            else:
                picked.append(e)
        if len(picked) == 0:
            return None # Cannot be ordered
        result += picked
        for e in picked:
            rest.remove(e)
        if len(rest) == 0:
            return result

def dict_eq(d1, d2, ignores):
    k = d1.keys()
    if k != d1.keys():
        return False
    for key in k:
        if key in ignores:
            continue
        if d1[key] != d2[key]:
            return False
    return True

def xml_int(element, attr, default = None):
    if element.get(attr) is None:
        if default is not None:
            return default
        else:
            raise Exception("Element has no attribute: " + attr)
    return int(element.get(attr))

def xml_bool(element, attr, default = None):
    if element.get(attr) is None:
        if default is not None:
            return default
        else:
            raise Exception("Element has no attribute: " + attr)
    return element.get(attr).lower() == "true"

def xml_str(element, attr, default = None):
    if element.get(attr) is None:
        if default is not None:
            return default
        else:
            raise Exception("Element has no attribute: " + attr)
    return element.get(attr)

def multiset(lst):
    result = {}
    for i in lst:
        result.setdefault(i, 0)
        result[i] += 1
    return result

def create_dict(lst, key):
    d = {}
    for item in lst:
        d[key(item)] = item
    return d

def key_not_in_list(dictionary, lst):
    for key in dictionary:
        if key not in lst:
            return key

id_counter = 1000
def get_unique_id():
    global id_counter
    id_counter += 1
    return id_counter

class Makefile:
    """ Simple class for emitting makefile """

    def __init__(self):
        self.variables = []
        self.rules = []
        self.top_comment = ""

    def set(self, variable, value):
        self.variables.append((variable, value))

    def rule(self, target, deps, command = None, phony = False):
        if phony:
            self.rules.append((".PHONY", [ target ], None))
        self.rules.append((target, deps, command))

    def set_top_comment(self, value):
        self.top_comment = value

    def write(self, out):
        for line in self.top_comment.split("\n"):
            out.write("# " + line + "\n")

        out.write("\n")

        for (var, value) in self.variables:
            out.write(var)
            out.write("=")
            out.write(value)
            out.write("\n")

        for (t, deps, c) in self.rules:
            out.write(t + ":")
            for d in deps:
                out.write(" " + d)
            if c is not None:
                out.write("\n\t" + c + "\n")
            else:
                out.write("\n")

    def write_to_file(self, filename):
        with open(filename,"w") as f:
            self.write(f)

def find_first(lst, fn):
    for i in lst:
        if fn(i):
            return i

def unions(lst, fn=None):
    if fn is None:
        fn = lambda x: x
    return set().union(*[ fn(x) for x in lst ])

def get_source_path(id, name):
    return "*{0}/{1}".format(id, name)

def first(lst):
    return lst[0]

def check_uniquness(items, key_fn):
    keys = [ key_fn(item) for item in items ]

    for item, key in zip(items, keys):
        if keys.count(key) != 1:
            return item

integer_parser = re.compile("[-+]?\d+")
def is_integer(value):
    return bool(integer_parser.match(value))


def objects_with_same_attribute(list1, list2, key_fn):
    for a in list1:
        key = key_fn(a)
        for b in list2:
            if key_fn(b) == key:
                yield (a, b)
