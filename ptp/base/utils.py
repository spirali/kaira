

class EqMixin(object):

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
            and self.__dict__ == other.__dict__)

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
                if fn(a, e):
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