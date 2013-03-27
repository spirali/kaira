#
#    Copyright (C) 2010-2013 Stanislav Bohm
#                  2011       Ondrej Garncarz
#                  2012       Martin Surkovsky
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

import utils
from utils import xml_int, xml_str
import xml.etree.ElementTree as xml
import citems
import undo

class Net:

    def __init__(self, project, net_type, name, id = None, test = False):
        assert net_type in [ "main", "module", "test" ]

        if id is None:
            self.id = project.new_id()
        else:
            self.id = id
        self.project = project
        self.net_type = net_type
        self.name = name
        self.items = []
        self.change_callback = lambda n: None
        self.change_item_callback = lambda n, i: None
        self.interface_box = None
        self.undo_manager = undo.UndoManager()

    def get_name(self):
        return self.name

    def get_id(self):
        return self.id

    def is_main(self):
        return self.net_type == "main"

    def is_simulator_net(self):
        return self.project.get_simulator_net() == self

    def set_change_callback(self, callback):
        self.change_callback = callback

    def new_id(self):
        return self.project.new_id()

    def add_item(self, item):
        if item.id is None:
            item.id = self.new_id()
        self.items.append(item)
        self.items.sort(key = lambda i: i.z_level, reverse = True)
        self.changed()

    def set_name(self, name):
        self.name = name
        self.changed()

    def is_test(self):
        return self.net_type == "test"

    def changed(self):
        self.change_callback(self)

    def changed_item(self, item):
        self.change_item_callback(self, item)

    def add_place(self, position, id = None):
        place = Place(self, id, position)
        self.add_item(place)
        self.changed()
        return place

    def add_interface_node(self, position, id = None):
        inode = InterfaceNode(self, id, position)
        self.add_item(inode)
        self.changed()
        return inode

    def add_interface_box(self, position, size, id = None):
        self.interface_box = InterfaceBox(self, id, position, size)
        self.add_item(self.interface_box)
        self.changed()
        return self.interface_box

    def add_transition(self, position, id = None):
        transition = Transition(self, id, position)
        self.add_item(transition)
        self.changed()
        return transition

    def add_edge(self, item1, item2, points, id = None):
        edge = Edge(self, id, item1, item2, points)
        self.add_item(edge)
        self.changed()
        return edge

    def add_area(self, position, size, id = None):
        area = NetArea(self, id, position, size)
        self.add_item(area)
        self.changed()
        return area

    def as_xml(self):
        e = xml.Element("net")
        e.set("name", self.name)
        e.set("id", str(self.id))
        e.set("net-type", self.net_type)
        for item in self.items:
            e.append(item.as_xml())
        return e

    def is_module(self):
        return self.interface_box is not None

    def copy(self):
        xml = self.as_xml()
        return load_net(xml, self.project, NewIdLoader(self.project))

    def get_item(self, id):
        for i in self.items:
            if i.get_id() == id:
                return i
        return None

    def places(self):
        return [ item for item in self.items if item.is_place() ]

    def transitions(self):
        return [ item for item in self.items if item.is_transition() ]

    def areas(self):
        return [ item for item in self.items if item.is_area() ]

    def inodes(self):
        return [ item for item in self.items if item.is_inode() ]

    def export_xml(self, build_config):
        e = xml.Element("net")
        e.set("name", self.name)
        e.set("id", str(self.id))

        for place in self.places():
            e.append(place.export_xml(build_config))

        for transition in self.transitions():
            e.append(transition.export_xml(build_config))

        for area in self.areas():
            e.append(area.export_xml())

        if self.interface_box:
            e.append(self.interface_box.export_xml())
        return e

    def item_by_id(self, id):
        for item in self.items:
            if item.id == id:
                return item
        return None

    def contains(self, item):
        return item in self.items

    def delete_item(self, item):
        self.items.remove(item)
        self.changed()

    def edges_from(self, item, postprocess=False):
        edges = [ i for i in self.items if i.is_edge() and i.from_item == item ]
        if postprocess:
            edges += [ i.make_complement()
                       for i in self.edges_to(item) if i.is_bidirectional() ]
        return edges

    def edges_to(self, item, postprocess=False):
        edges = [ i for i in self.items if i.is_edge() and i.to_item == item ]
        if postprocess:
            edges += [ i.make_complement()
                       for i in self.edges_from(item) if i.is_bidirectional() ]
        return edges

    def edges_of(self, item):
        return [ i for i in self.items
                 if i.is_edge() and (i.to_item == item or i.from_item == item) ]

    def edges_out(self):
        return [ i for i in self.items
                 if i.is_edge() and (i.from_item.is_transition() or i.is_bidirectional()) ]

    def trace_nothing(self):
        for i in self.transitions() + self.places():
            i.tracing = []
        self.changed()

    def trace_everything(self):
        for i in self.transitions():
            if not "fire" in i.tracing:
                i.tracing.insert(0, "fire")
        token_name = ("ca::token_name", "std::string")
        for i in self.places():
            if token_name not in i.tracing:
                i.tracing.insert(0,  token_name)
        self.changed()


class NetItem(object):

    z_level = 0

    def __init__(self, net, id):
        self.net = net
        self.id = id

    def get_id(self):
        return self.id

    def changed(self):
        self.net.changed_item(self)

    def is_place(self):
        return False

    def is_transition(self):
        return False

    def is_edge(self):
        return False

    def is_area(self):
        return False

    def is_inode(self):
        return False

    def is_interfacebox(self):
        return False

    def is_interfacenode(self):
        return False

    def delete(self):
        self.net.delete_item(self)
        return [ self ]

    def create_xml_element(self, name):
        element =  xml.Element(name)
        element.set("id", str(self.id))
        return element

    def get_text_entries(self):
        return []

    def get_canvas_items_dict(self):
        result = {}
        for i in self.get_canvas_items():
            result[i.kind] = i
        return result

    def get_error_items(self):
        result = []
        messages = self.net.project.get_error_messages(self)
        if not messages:
            return result
        items = self.get_canvas_items_dict()
        for name in messages:
            item = items.get(name)
            if item:
                position = utils.vector_add(item.get_position(), item.size)
                position = utils.vector_add(position, (10, 0))
                placement = item.get_relative_placement(position)
                error_item = citems.Text(None, "error", placement)
                error_item.delegate_selection = item
                error_item.background = (255, 0, 0)
                error_item.border = True
                error_item.z_level = 20
                error_item.text = messages[name][0]
                result.append(error_item)
        return result


class NetElement(NetItem):

    code = ""

    def __init__(self, net, id, position):
        NetItem.__init__(self, net, id)

        self.box = citems.ElementBox(
            self,
            "box",
            citems.AbsPlacement(position),
            self.size,
            self.radius)
        self.tracing = []

    def has_code(self):
        return self.code != ""

    def get_code(self):
        return self.code

    def set_code(self, code):
        self.code = code
        self.box.doubleborder = self.has_code()
        self.changed()

    def edges(self):
        return self.net.edges_of(self)

    def edges_from(self, postprocess=False):
        return self.net.edges_from(self, postprocess)

    def edges_to(self, postprocess=False):
        return self.net.edges_to(self, postprocess)

    def delete(self):
        deleted = []
        for edge in self.edges():
            deleted += edge.delete()
        deleted += NetItem.delete(self)
        return deleted

    def xml_code_element(self):
        e = xml.Element("code")
        e.text = self.code
        return e

    def area(self):
        for area in self.net.areas():
            if area.is_inside(self):
                return area


class Transition(NetElement):

    size = (70, 36)
    radius = 0

    def __init__(self, net, id, position):
        NetElement.__init__(self, net, id, position)

        p = (position[0], position[1] - 20)
        self.guard = citems.Text(self, "guard", self.box.get_relative_placement(p))

    def get_canvas_items(self):
        return [ self.box, self.guard ]

    def get_name(self):
        return self.box.name

    def set_name(self, name):
        self.box.name = name
        self.changed()

    def get_guard(self):
        return self.guard.text

    def set_guard(self, guard):
        self.guard.text = guard
        self.changed()

    def is_transition(self):
        return True

    def is_immediate(self):
        return not self.has_code()

    def as_xml(self):
        e = self.create_xml_element("transition")
        e.set("name", self.box.name)
        position = self.box.get_position()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("sx", str(self.box.size[0]))
        e.set("sy", str(self.box.size[1]))
        e.append(canvastext_to_xml(self.guard, "guard"))
        if self.has_code():
            e.append(self.xml_code_element())
        if self.tracing:
            for t in self.tracing:
                trace = xml.Element("trace")
                trace.text = t
                e.append(trace)
        return e

    def get_trace_texts(self):
        return self.tracing

    def export_xml(self, build_config):
        e = self.create_xml_element("transition")
        e.set("name", self.box.name)
        e.set("guard", self.guard.text)
        if self.has_code():
            e.append(self.xml_code_element())

        for edge in self.edges_to(postprocess=True):
            e.append(edge.create_xml_export_element("edge-in"))

        for edge in self.edges_from(postprocess=True):
            e.append(edge.create_xml_export_element("edge-out"))
        if build_config.tracing and self.tracing:
            for t in self.tracing:
                trace = xml.Element("trace")
                trace.text = t
                e.append(trace)
        return e

    def get_text_entries(self):
        return [ ("Name", self.get_name, self.set_name),
                ("Guard", self.get_guard, self.set_guard) ]


class Place(NetElement):

    radius = 20
    size = (0, 0)

    def __init__(self, net, id, position):
        NetElement.__init__(self, net, id, position)

        p = (position[0] + self.radius * 0.85, position[1] + self.radius * 0.85)
        self.place_type = citems.Text(self, "type", self.box.get_relative_placement(p))

        p = (position[0] + self.radius * 0.85, position[1] - self.radius * 1.5)
        self.init = citems.Text(self, "init", self.box.get_relative_placement(p))

    def get_canvas_items(self):
        return [ self.box,
                 self.place_type,
                 self.init ]

    def get_radius(self):
        return self.radius

    def get_name(self):
        return self.box.name

    def set_name(self, name):
        self.box.name = name
        self.changed()

    def get_init_string(self):
        return self.init.text

    def set_place_type(self, place_type):
        self.place_type.text = place_type
        self.changed()

    def set_init_string(self, init_string):
        self.init.text = init_string
        self.changed()

    def get_place_type(self):
        return self.place_type.text

    def is_place(self):
        return True

    def get_trace_texts(self):
        result = []
        for name, t in self.tracing:
            result.append(name)
            result.append("({0})".format(t))
        return result

    def tracing_to_xml(self, element):
        for name, return_type in self.tracing:
            e = xml.Element("trace")
            e.set("name", name)
            e.set("return-type", return_type)
            element.append(e)

    def as_xml(self):
        e = self.create_xml_element("place")
        position = self.box.get_position()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("name", str(self.box.name))
        e.set("radius", str(self.box.radius))
        e.set("sx", str(self.box.size[0]))
        e.set("sy", str(self.box.size[1]))
        e.append(canvastext_to_xml(self.place_type, "place-type"))
        e.append(canvastext_to_xml(self.init, "init"))
        if self.has_code():
            e.append(self.xml_code_element())
        self.tracing_to_xml(e)
        return e

    def export_xml(self, build_config):
        e = self.create_xml_element("place")
        e.set("name", self.box.name)
        e.set("type", self.place_type.text)
        e.set("init-expr", self.init.text)
        if self.has_code():
            e.append(self.xml_code_element())
        if build_config.tracing and self.tracing:
            self.tracing_to_xml(e)
        return e

    def get_text_entries(self):
        return [ ("Type", self.get_place_type, self.set_place_type),
                ("Init", self.get_init_string, self.set_init_string),
                ("Name", self.get_name, self.set_name)]


class Edge(NetItem):

    z_level = 1

    def __init__(self, net, id, from_item, to_item, points):
        NetItem.__init__(self, net, id)
        self.from_item = from_item
        self.to_item = to_item
        self.points = [ citems.Point(self, "point", citems.AbsPlacement(p))
                        for p in points ]
        self.line = citems.ArrowLine(self, "line", self.get_all_points)
        self.inscription = citems.Text(self,
                                       "inscription",
                                       self.line.get_relative_placement(None),
                                       "")

    def simple_copy(self):
        """ Copy of edge that preserves topological properties:
            id, inscription, from_item and to_item """
        e = Edge(self.net, self.id, self.from_item, self.to_item, [])
        e.inscription = self.inscription
        return e

    def get_canvas_items(self):
        return [ self.line, self.inscription ] + self.points

    def add_point(self, position):
        inscription_position = self.inscription.get_position()
        for i, (a, b) in enumerate(utils.pairs_generator(self.get_all_points())):
            if utils.is_near_line_segment(a, b, position, 5):
                point = citems.Point(self, "point", citems.AbsPlacement(position))
                point.owner = self
                self.points.insert(i, point)
                break
        self.inscription.set_position(inscription_position)
        self.net.changed() # Canvas items changed, so self.changed() is not sufficient

    def remove_point(self, item):
        inscription_position = self.inscription.get_position()
        self.points.remove(item)
        self.inscription.set_position(inscription_position)
        self.net.changed() # Canvas items changed, so self.changed() is not sufficient

    def get_inscription(self):
        return self.inscription.text

    def set_inscription(self, inscription):
        self.inscription.text = inscription
        self.changed()

    def is_bidirectional(self):
        return self.line.bidirectional

    def toggle_bidirectional(self):
        self.line.bidirectional = not self.line.bidirectional
        self.changed()

    def make_complement(self):
        """ This function returns exact copy of the edge with changed directions,
            This is used during splitting bidirectional edges """
        c = self.simple_copy()
        c.switch_direction()
        return c

    def get_end_points(self):
        if self.points:
            p1 = self.points[0].get_position()
            p2 = self.points[-1].get_position()
        else:
            p1 = utils.vector_add_t(
                self.to_item.box.get_position(), self.to_item.size, 0.5)
            p2 = utils.vector_add_t(
                self.from_item.box.get_position(), self.from_item.size, 0.5)
        return (self.from_item.box.get_border_point(p1),
                self.to_item.box.get_border_point(p2))

    def compute_insciption_point(self):
        points = self.get_all_points()
        if self.inscription_point < len(points) - 1:
            return utils.interpolate(points[self.inscription_point],
                                     points[self.inscription_point + 1],
                                     self.inscription_param)
        else:
            return self.points[self.inscription_point]

    def is_edge(self):
        return True

    def switch_direction(self):
        inscription_position = self.inscription.get_position()
        i = self.from_item
        self.from_item = self.to_item
        self.to_item = i
        self.points.reverse()
        self.inscription.set_position(inscription_position)
        self.changed()

    def as_xml(self):
        e = self.create_xml_element("edge")
        e.set("from_item", str(self.from_item.id))
        e.set("to_item", str(self.to_item.id))
        if self.line.bidirectional:
            e.set("bidirectional", "true")

        e.append(canvastext_to_xml(self.inscription, "inscription"))

        for point in self.points:
            pe = xml.Element("point")
            position = point.get_position()
            pe.set("x", str(position[0]))
            pe.set("y", str(position[1]))
            e.append(pe)
        return e

    def get_all_points(self):
        sp, ep = self.get_end_points()
        return [sp] + [ p.get_position() for p in self.points ] + [ep]

    def get_text_entries(self):
        return [ ("Inscription", self.get_inscription, self.set_inscription) ]

    def create_xml_export_element(self, name):
        e = xml.Element(name)
        e.set("id", str(self.id))
        if self.from_item.is_place():
            e.set("place-id", str(self.from_item.get_id()))
        else:
            e.set("place-id", str(self.to_item.get_id()))
        e.set("expr", self.inscription.text)
        return e


class RectItem(NetItem):

    def __init__(self, net, id, position, size):
        NetItem.__init__(self, net, id)
        self.point1 = citems.Point(self, "point1", citems.AbsPlacement(position))
        self.point1.action = "resize_ltop"
        self.point2 = citems.Point(self, "point2", citems.AbsPlacement(
            utils.vector_add(position, size)))
        self.point2.owner = self
        self.point2.action = "resize_rbottom"

    def get_size(self):
        return utils.make_vector(self.point1.get_position(), self.point2.get_position())

    def is_inside(self, item):
        return utils.position_inside_rect(
            item.box.get_position(),
            self.point1.get_position(),
            self.get_size())


class NetArea(RectItem):

    def __init__(self, net, id, position, size):
        RectItem.__init__(self, net, id, position, size)
        self.area = citems.Area(self, "area", self.point1, self.point2)

        position = utils.vector_add(self.point1.get_position(), (0, -15))
        self.init = citems.Text(self, "init", self.point1.get_relative_placement(position))

    def get_canvas_items(self):
        return [ self.point1, self.point2,
                 self.init, self.area ]

    def set_init_expr(self, init_expr):
        self.init.text = init_expr
        self.changed()

    def get_init_expr(self):
        return self.init.text

    def get_text_entries(self):
        return [ ("Init", self.get_init_expr, self.set_init_expr) ]

    def is_area(self):
        return True

    def as_xml(self):
        e = self.create_xml_element("area")
        position = self.point1.get_position()
        size = self.get_size()
        e.set("x", str(position[0]))
        e.set("y", str(position[1]))
        e.set("sx", str(size[0]))
        e.set("sy", str(size[1]))
        e.append(canvastext_to_xml(self.init, "init"))
        return e

    def export_xml(self):
        e = self.create_xml_element("area")
        e.set("init-expr", self.init.text)
        e.set("name", "")
        items = [ item for item in self.net.places() if self.is_inside(item) ]
        for item in items:
            element = xml.Element("place")
            element.set("id", str(item.id))
            e.append(element)
        return e

    def places(self):
        return [ place for place in self.net.places() if self.is_inside(place) ]

    def transitions(self):
        return [ transition for transition in self.net.transitions()
                 if self.is_inside(transition) ]


class InterfaceBox(RectItem):

    z_level = -2

    def is_interfacebox(self):
        return True

    def as_xml(self):
        e = self.create_xml_element("interface-box")
        e.set("x", str(self.position[0]))
        e.set("y", str(self.position[1]))
        e.set("sx", str(self.size[0]))
        e.set("sy", str(self.size[1]))
        return e

    def export_xml(self):
        e = xml.Element("interface")
        for inode in self.net.inodes():
            for edge in self.net.edges_from(inode, postprocess = True):
                e.append(edge.create_xml_export_element("edge-out"))
            for edge in self.net.edges_to(inode, postprocess = True):
                e.append(edge.create_xml_export_element("edge-in"))
        return e


class InterfaceNode(NetElement):

    def __init__(self, net, id, position):
        NetItem.__init__(self, net, id)
        self.set_position(position)

    def is_inode(self):
        return True

    def get_border_point(self, outer_point):
        v = utils.make_vector_with_size(self.position, outer_point, 5.0)
        return utils.vector_add(self.position, v)

    def as_xml(self):
        e = self.create_xml_element("interface-node")
        e.set("x", str(self.position[0]))
        e.set("y", str(self.position[1]))
        return e

    def is_interfacenode(self):
        return True


class BasicLoader:
    """
        Loads an element id from xml and preserves original id
    """
    def __init__(self, project):
        self.project = project

    def get_id(self, element):
        id = utils.xml_int(element, "id", 0)
        self.project.id_counter = max(self.project.id_counter, id)
        return id

    def translate_id(self, id):
        return id


class NewIdLoader:
    """
        Loads an element id from xml and assign new id
    """

    def __init__(self, project):
        self.project = project
        self.idtable = {}

    def get_id(self, element):
        id = utils.xml_int(element, "id", 0)
        new_id = self.project.new_id()
        self.idtable[id] = new_id
        return new_id

    def translate_id(self, id):
        return self.idtable[id]


def canvastext_to_xml(obj, name):
    element = xml.Element(name)
    position = obj.get_position()
    element.set("x", str(position[0]))
    element.set("y", str(position[1]))
    element.text = obj.text
    return element

def canvastext_from_xml(element, obj):
    position = (utils.xml_int(element, "x"),
                utils.xml_int(element, "y"))
    obj.set_position(position)
    if element.text is None:
        obj.text = ""
    else:
        obj.text = element.text

def load_code(element):
    if element.find("code") is not None:
        return element.find("code").text
    else:
        return ""

def load_tracing(element):
    trace = []
    for t in element.findall("trace"):
        trace.append(t.text)
    return trace

def load_place_tracing(element):
    tracing = []
    for e in element.findall("trace"):
        name = e.get("name", None)
        return_type = e.get("return-type", None)
        if name is None or return_type is None:
            return tracing # backward compatability
        tracing.append((name, return_type))
    return tracing

def load_place(element, net, loader):
    id = loader.get_id(element)
    place = net.add_place((xml_int(element,"x"), xml_int(element, "y")), id)
    place.box.name = xml_str(element, "name", "")
    place.box.radius = xml_int(element,"radius")
    place.box.size = (xml_int(element,"sx", 0), xml_int(element,"sy", 0))

    if element.find("place-type") is not None:
        canvastext_from_xml(element.find("place-type"), place.place_type)
    else:
        place.place_type.text = element.get("place_type", "") # Backward compatability

    if element.find("init") is not None:
        canvastext_from_xml(element.find("init"), place.init)
    else:
        place.init.text = element.get("init_string", "") # Backward compatability
    place.set_code(load_code(element))
    place.tracing =  load_place_tracing(element)

def load_transition(element, net, loader):
    id = loader.get_id(element)
    transition = net.add_transition((xml_int(element,"x"), xml_int(element, "y")), id)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    transition.box.size = (sx, sy)
    transition.box.name = xml_str(element,"name", "")
    if element.find("guard") is not None:
        canvastext_from_xml(element.find("guard"), transition.guard)
    else:
        transition.guard.text = element.get("guard", "") # Backward compatability
    transition.set_code(load_code(element))
    transition.tracing = load_tracing(element)

def load_edge(element, net, loader):
    id = loader.get_id(element)
    fitem = net.item_by_id(loader.translate_id(xml_int(element, "from_item")))
    assert fitem is not None
    titem = net.item_by_id(loader.translate_id(xml_int(element, "to_item")))
    assert titem is not None
    points = [ (xml_int(e, "x"), xml_int(e,"y")) for e in element.findall("point") ]
    edge = net.add_edge(fitem, titem, points, id)
    edge.line.bidirectional = utils.xml_bool(element, "bidirectional", False)

    if element.find("inscription") is not None:
        canvastext_from_xml(element.find("inscription"), edge.inscription)
    else: # Backward compitabality
        if element.get("inscription") is not None:
            edge.inscription.text = xml_str(element, "inscription")

def load_area(element, net, loader):
    id = loader.get_id(element)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    px = xml_int(element, "x")
    py = xml_int(element, "y")
    area = net.add_area((px, py), (sx, sy), id)
    if element.find("init") is not None:
        canvastext_from_xml(element.find("init"), area.init)
    else:
        area.init.text = xml_str(element,"init-expr", "")

def load_interface_box(element, net, loader):
    id = loader.get_id(element)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    px = xml_int(element, "x")
    py = xml_int(element, "y")
    net.add_interface_box((px, py), (sx, sy), id)

def load_interface_node(element, net, loader):
    id = loader.get_id(element)
    px = xml_int(element, "x")
    py = xml_int(element, "y")
    net.add_interface_node((px, py), id)

def load_net(element, project, loader):
    name = element.get("name", "Main") # Setting "Main" for backward compatability
    id = loader.get_id(element)
    net_type = element.get("net-type")

    if net_type is None: # Backward compatability
        if element.find("interface-box") is None:
            net_type = "main"
        else:
            net_type = "module"

    net = Net(project, net_type, name, id)
    interface_box = element.find("interface-box")
    if interface_box is not None:
        load_interface_box(interface_box, net, loader)

    for e in element.findall("interface-node"):
        load_interface_node(e, net, loader)

    for e in element.findall("area"):
        load_area(e, net, loader)

    for e in element.findall("place"):
        load_place(e, net, loader)

    for e in element.findall("transition"):
        load_transition(e, net, loader)

    for e in element.findall("edge"):
        load_edge(e, net, loader)

    return net
