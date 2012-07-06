#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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
import math
import utils
from utils import xml_int, xml_str
import xml.etree.ElementTree as xml
from copy import copy

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
        self.interface_box = None
        self.autohalt = True

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

    def remove_item(self, item):
        self.items.remove(item)
        self.changed()

    def set_autohalt(self, value):
        self.autohalt = value
        self.changed()

    def get_autohalt(self):
        return self.autohalt

    def set_name(self, name):
        self.name = name
        self.changed()

    def is_test(self):
        return self.net_type == "test"

    def changed(self):
        self.change_callback(self)

    def draw(self, cr, vconfig):
        drawings = [ item.get_drawing(vconfig) for item in self.items ]
        drawings.reverse()
        for drawing in drawings:
            drawing.draw(cr)
        for drawing in drawings:
            drawing.draw_top(cr)

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
        if self.is_module():
            e.set("autohalt", str(self.autohalt))
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
        if self.is_module():
            e.set("autohalt", str(self.autohalt))
        else:
            e.set("autohalt", "False")

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

    def pick_items(self, position):
        return [ item for item in self.items if item.is_at_position(position) ]

    def get_item_at_position(self, position, filter_fn = None):
        for item in filter(filter_fn, self.items):
            if item.is_at_position(position):
                return item
        return None

    def get_transition_at_position(self, position):
        return self.get_item_at_position(position, lambda i: i.is_transition())

    def delete_item(self, item):
        self.items.remove(item)
        self.changed()

    def edges_from(self, item, postprocess = False):
        edges = [ i for i in self.items if i.is_edge() and i.from_item == item ]
        if postprocess:
            edges = edges + [ i.make_complement() for i in self.edges_to(item) if i.is_bidirectional() ]
            return sum([ edge.postprocess() for edge in edges ], [])
        else:
            return edges

    def edges_to(self, item, postprocess = False):
        edges = [ i for i in self.items if i.is_edge() and i.to_item == item ]
        if postprocess:
            edges = edges + [ i.make_complement() for i in self.edges_from(item) if i.is_bidirectional() ]
            return sum( [ edge.postprocess() for edge in edges ], [])
        else:
            return edges

    def edges_of(self, item):
        return [ i for i in self.items if i.is_edge() and (i.to_item == item or i.from_item == item) ]

    def corners(self):
        """ Returns bounding box as left-top and right-bottom points """
        t = 0
        l = 0
        r = 100
        b = 100
        for i in self.items:
            (il, it), (ir, ib) = i.corners()
            t = min(t, it)
            l = min(l, il)
            r = max(r, ir)
            b = max(b, ib)
        return ((l,t), (r, b))

    def trace_nothing(self):
        for i in self.transitions() + self.places():
            i.tracing = False

    def trace_everything(self):
        for i in self.transitions() + self.places():
            i.tracing = True


class NetItem(object):

    z_level = 0

    def __init__(self, net, id):
        self.net = net
        self.id = id

    def get_id(self):
        return self.id

    def changed(self):
        self.net.changed()

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

    def delete(self):
        self.net.delete_item(self)

    def create_xml_element(self, name):
        element =  xml.Element(name)
        element.set("id", str(self.id))
        return element

    def get_text_entries(self):
        return []

class NetElement(NetItem):

    code = ""
    tracing = False

    def __init__(self, net, id, position):
        NetItem.__init__(self, net, id)
        self.position = position

    def has_code(self):
        return self.code != ""

    def get_code(self):
        return self.code

    def set_code(self, code):
        self.code = code.strip()
        self.changed()

    def get_position(self):
        return self.position

    def set_position(self, position):
        self.position = position
        self.changed()

    def edges(self):
        return self.net.edges_of(self)

    def edges_from(self, postprocess = False):
        return self.net.edges_from(self, postprocess)

    def edges_to(self, postprocess = False):
        return self.net.edges_to(self, postprocess)

    def delete(self):
        for edge in self.edges():
            edge.delete()
        NetItem.delete(self)

    def xml_code_element(self):
        e = xml.Element("code")
        e.text = self.code
        return e

    def area(self):
        for area in self.net.areas():
            if area.is_inside(self):
                return area

class Transition(NetElement):

    size = (70, 35)
    name = ""
    guard = ""
    subnet = None

    def get_name(self):
        return self.name

    def set_name(self, name):
        self.name = name
        self.changed()

    def get_guard(self):
        return self.guard

    def set_guard(self, guard):
        self.guard = guard
        self.changed()

    def get_size(self):
        return self.size

    def resize(self, point):
        self.size = (point[0] * 2, point[1] * 2)
        self.changed()

    def is_transition(self):
        return True

    def is_immediate(self):
        return not self.has_code() and self.subnet is None

    def get_trace_text(self):
        if self.tracing:
            return "fire"
        else:
            return None

    def as_xml(self):
        e = self.create_xml_element("transition")
        e.set("name", self.name)
        e.set("guard", self.guard)
        e.set("x", str(self.position[0]))
        e.set("y", str(self.position[1]))
        e.set("sx", str(self.size[0]))
        e.set("sy", str(self.size[1]))
        if self.subnet:
            e.set("subnet", str(self.subnet.get_id()))
        if self.has_code():
            e.append(self.xml_code_element())
        return e

    def set_subnet(self, net):
        self.subnet = net
        self.changed()

    def export_xml(self, build_config):
        e = self.create_xml_element("transition")
        e.set("name", self.name)
        e.set("guard", self.guard)
        if self.has_code():
            e.append(self.xml_code_element())

        if self.subnet:
            e.set("subnet", str(self.subnet.get_id()))

        for edge in self.edges_to(postprocess = True):
            e.append(edge.create_xml_export_element("edge-in"))

        for edge in self.edges_from(postprocess = True):
            e.append(edge.create_xml_export_element("edge-out"))

        if build_config.tracing and self.tracing:
            e.set("tracing", "full")
        return e

    def get_drawing(self, vconfig):
        return vconfig.transition_drawing(self)

    def is_at_position(self, position):
        px, py = position
        mx, my = self.position
        sx, sy = self.size
        sx /= 2
        sy /= 2
        return px >= mx - sx - 5 and py >= my - sy - 5 and px < mx + sx + 5 and py < my + sy + 5

    def get_action(self, position, factory):
        px, py = position
        mx, my = self.position
        sx, sy = self.size
        sx /= 2
        sy /= 2

        if px >= mx + sx - 5 and py >= my + sy - 5 and px < mx + sx + 5 and py < my + sy + 5:
            return factory.get_resize_action(self, position, self.resize)

        if px >= mx - sx - 5 and py >= my - sy - 5 and px < mx + sx + 5 and py < my + sy + 5:
            return factory.get_move_action(self.get_position(), self.set_position, position)

    def get_border_point(self, outer_point):
        px, py = self.position
        ox, oy = outer_point
        sx, sy = self.size
        sx /= 2
        sy /= 2

        if py - sy > oy:
            y = py - sy
        elif py + sy > oy:
            y = py
        else:
            y = py + sy

        if px - sx > ox:
            x = px - sx
        elif px + sx > ox:
            x = px
        else:
            x = px + sx
        return (x, y)

    def get_text_entries(self):
        return [ ("Name", self.get_name, self.set_name),
                ("Guard", self.get_guard, self.set_guard) ]

    def corners(self):
        px, py = self.position
        sx, sy = self.size
        return ((px - sx, py - sy), (px + sx, py + sy))

    def get_packing_input_places(self):
        """ Fast function for tracelog replay """
        result = []
        for i in self.net.items:
            if i.is_edge() and i.inscription.startswith("~"):
                if i.to_item == self:
                    result.append(i.from_item)
                elif i.is_bidirectional() and i.from_item == self:
                    result.append(i.to_item)
        return result

class Place(NetElement):

    radius = 20
    place_type = ""
    init_string = ""

    def get_name(self):
        return str(self.get_id())

    def get_radius(self):
        return self.radius

    def get_init_string(self):
        return self.init_string

    def set_init_string(self, init_string):
        self.init_string = init_string
        self.changed()

    def get_place_type(self):
        return self.place_type

    def set_place_type(self, place_type):
        self.place_type = place_type
        self.changed()

    def is_place(self):
        return True

    def get_trace_text(self):
        if self.tracing:
            return "values"
        else:
            return None

    def as_xml(self):
        e = self.create_xml_element("place")
        e.set("x", str(self.position[0]))
        e.set("y", str(self.position[1]))
        e.set("radius", str(self.radius))
        e.set("place_type", self.place_type)
        e.set("init_string", self.init_string)
        if self.has_code():
            e.append(self.xml_code_element())
        return e

    def export_xml(self, build_config):
        e = self.create_xml_element("place")
        e.set("name", "name")
        e.set("type", self.place_type)
        e.set("init-expr", self.init_string)
        if self.has_code():
            e.append(self.xml_code_element())
        if build_config.tracing and self.tracing:
            e.set("tracing", "full")
        return e

    def get_drawing(self, vconfig):
        return vconfig.place_drawing(self)

    def is_at_position(self, position):
        dist = utils.point_distance(self.position, position)
        return dist < self.radius + 5

    def get_action(self, position, factory):
        dist = utils.point_distance(self.position, position)

        if dist < self.radius + 5 and dist > self.radius - 5:
            return factory.get_resize_action(self, position, self.resize)

        if dist < self.radius:
            return factory.get_move_action(self.get_position(), self.set_position, position)

    def resize(self, point):
        px, py = point
        self.radius = math.sqrt(px * px + py * py)
        self.changed()

    def get_border_point(self, outer_point):
        px, py = self.position
        ox, oy = outer_point
        vx = ox - px
        vy = oy - py
        d = math.sqrt(vx * vx + vy * vy)
        if d < 0.0001:
            return outer_point
        nx = vx / d * self.radius
        ny = vy / d * self.radius
        return (nx + px, ny + py)

    def get_text_entries(self):
        return [ ("Type", self.get_place_type, self.set_place_type),
                ("Init", self.get_init_string, self.set_init_string) ]

    def corners(self):
        px, py = self.position
        r = self.radius
        return ((px - r, py - r), (px + r, py + r))

class Edge(NetItem):

    bidirectional = False

    def __init__(self, net, id, from_item, to_item, points):
        NetItem.__init__(self, net, id)
        self.from_item = from_item
        self.to_item = to_item
        self.points = points
        self.inscription = ""
        self.inscription_position = None
        self.inscription_size = (0,0) # real value obtained by dirty hack in EdgeDrawing
        self.inscription_point = len(self.get_all_points()) / 2 - 1
        self.inscription_param = 0.5
        self.offset = (0,10)

    def add_point(self, point):
        x, y = point
        all_points = [self.from_item.position] + self.points + [self.to_item.position]
        for idx in range(len(all_points) - 1):
            p1_x, p1_y = all_points[idx]
            p2_x, p2_y = all_points[idx + 1]
            if (utils.is_between(p1_x, p2_x, x) and utils.is_between(p1_y, p2_y, y)):
                all_points = all_points[:idx+1] + [point] + all_points[idx+1:]
                break;

        self.points = all_points[1:-1]
        self.changed()

    def get_inscription(self):
        return self.inscription

    def set_inscription(self, inscription):
        self.inscription = inscription
        self.changed()

    def is_bidirectional(self):
        return self.bidirectional

    def toggle_bidirectional(self):
        self.bidirectional = not self.bidirectional
        self.changed()

    def set_inscription_position(self, position):
        points = self.get_all_points()
        self.inscription_point, self.inscription_param = utils.nearest_point_of_multiline(points, position)
        self.offset = utils.vector_diff(self.compute_insciption_point() , position)
        self.inscription_position = position
        self.changed()

    def get_inscription_position(self):
        self.inscription_position = utils.vector_diff(self.compute_insciption_point(), self.offset)
        return self.inscription_position

    def make_complement(self):
        """ This function returns exact copy of the edge with changed directions,
            This is used during splitting bidirectional edges """
        c = copy(self)
        c.switch_direction()
        return c

    def postprocess(self):
        if self.inscription.strip() == "":
            return [self]
        edges = []
        for inscription in self.inscription.split(";"):
            if inscription.strip() != "":
                c = copy(self)
                c.inscription = inscription
                edges.append(c)
        return edges

    def get_end_points(self):
        if self.points:
            p1 = self.points[0]
            p2 = self.points[-1]
        else:
            p1 = self.to_item.position
            p2 = self.from_item.position
        return (self.from_item.get_border_point(p1), self.to_item.get_border_point(p2))

    def compute_insciption_point(self):
        points = self.get_all_points()
        if self.inscription_point < len(points) - 1:
            vec = utils.make_vector(points[self.inscription_point], points[self.inscription_point + 1])
            vec = utils.vector_mul_scalar(vec, self.inscription_param)
        else:
            vec = (0, 0)
        return utils.vector_add(vec, points[self.inscription_point])

    def is_edge(self):
        return True

    def switch_direction(self):
        i = self.from_item
        self.from_item = self.to_item
        self.to_item = i
        self.points.reverse()
        self.changed()

    def as_xml(self):
        e = self.create_xml_element("edge")
        e.set("from_item", str(self.from_item.id))
        e.set("to_item", str(self.to_item.id))
        if self.bidirectional:
            e.set("bidirectional", "true")
        if self.inscription:
            e.set("inscription", self.inscription)
            e.set("inscription_x", str(self.inscription_position[0]))
            e.set("inscription_y", str(self.inscription_position[1]))
        for px, py in self.points:
            pe = xml.Element("point")
            pe.set("x", str(px))
            pe.set("y", str(py))
            e.append(pe)
        return e

    def get_drawing(self, vconfig):
        return vconfig.edge_drawing(self)

    def get_all_points(self):
        sp, ep = self.get_end_points()
        return [sp] + self.points + [ep]

    def is_at_position(self, position):
        if self.inscription_position and utils.position_inside_rect(position, utils.vector_diff(self.inscription_position, (self.inscription_size[0]/2, 0)), self.inscription_size, 4):
            return True

        for p in self.points:
            if utils.point_distance(p, position) < 7:
                return True
        for (a, b) in utils.pairs_generator(self.get_all_points()):
            dist = utils.point_distance(a, b) - 5
            d1 = utils.point_distance(position, a)
            d2 = utils.point_distance(position, b)
            if d1 < dist and d2 < dist and utils.distance_to_line(a, b, position) < 5:
                return True
        return False

    def get_action(self, position, factory):
        def set_point(i, p):
            self.points[i] = p
            self.changed()
        if self.inscription_position and utils.position_inside_rect(position, utils.vector_diff(self.inscription_position, (self.inscription_size[0]/2, 0)), self.inscription_size, 4):
            return factory.get_move_action(self.get_inscription_position(), self.set_inscription_position, position)

        for i, p in enumerate(self.points):
            if utils.point_distance(p, position) < 7:
                return factory.get_move_action(p, lambda x: set_point(i, x), position)
        return factory.get_empty_action()

    def get_text_entries(self):
        return [ ("Inscription", self.get_inscription, self.set_inscription) ]

    def corners(self):
        # FIXME
        return ((0,0), (0,0))

    def is_packing_edge(self):
        return self.inscription and self.inscription[0] == "~"

    def create_xml_export_element(self, name):
        e = xml.Element(name)
        e.set("id", str(self.id))
        if self.from_item.is_place():
            e.set("place-id", str(self.from_item.get_id()))
        else:
            e.set("place-id", str(self.to_item.get_id()))
        e.set("expr", self.inscription)
        return e

class RectItem(NetItem):

    def __init__(self, net, id, position, size):
        NetItem.__init__(self, net, id)
        self.position = position
        self.size = size

    def get_size(self):
        return self.size

    def resize_rbottom(self, original_pos, original_size, rel_change):
        self.size = utils.vector_add(original_size, rel_change)
        self.changed()

    def resize_ltop(self, original_pos, original_size, rel_change):
        self.size = utils.vector_diff(original_size, rel_change)
        self.position = utils.vector_add(original_pos, rel_change)
        self.changed()

    def resize_lbottom(self, original_pos, original_size, rel_change):
        self.size = utils.vector_add(original_size, (-rel_change[0], rel_change[1]))
        self.position = utils.vector_add(original_pos, (rel_change[0], 0))
        self.changed()

    def resize_rtop(self, original_pos, original_size, rel_change):
        self.size = utils.vector_add(original_size, (rel_change[0], -rel_change[1]))
        self.position = utils.vector_add(original_pos, (0, rel_change[1]))
        self.changed()

    def get_position(self):
        return self.position

    def set_position(self, position):
        self.position = position
        self.changed()

    def is_inside(self, item):
        return utils.position_inside_rect(item.position, self.position, self.size)

    def corners(self):
        return (self.position, utils.vector_add(self.position, self.size))

    def is_at_position(self, position):
        return utils.position_on_rect(position, self.position, self.size, 5)

    def get_action(self, position, factory):
        def make_action(f, cursor):
            return factory.get_custom_move_action(position, lambda r: f(original_pos, original_size, r), cursor)
        px, py = position
        mx, my = self.position
        sx, sy = self.size

        original_size = self.size
        original_pos = self.position
        if px >= mx + sx - 10 and py >= my + sy - 10 and px < mx + sx + 5 and py < my + sy + 5:
            return make_action(self.resize_rbottom, "resize_rbottom")

        if px >= mx - 10 and py >= my - 10 and px < mx + 5 and py < my + 5:
            return make_action(self.resize_ltop, "resize_ltop")

        if px >= mx - 10 and py >= my + sy - 10 and px < mx + 5 and py < my + sy + 5:
            return make_action(self.resize_lbottom, "resize_lbottom")

        if px >= mx + sx - 10 and py >= my - 10 and px < mx + sx + 5 and py < my + 5:
            return make_action(self.resize_rtop, "resize_rtop")

class NetArea(RectItem):

    name = ""
    init_expr = ""
    z_level = -1

    def set_init_expr(self, init_expr):
        self.init_expr = init_expr
        self.changed()

    def get_init_expr(self):
        return self.init_expr

    def set_name(self, name):
        self.name = name
        self.changed()

    def get_name(self):
        return self.name

    def get_drawing(self, vconfig):
        return vconfig.area_drawing(self)

    def get_text_entries(self):
        return [ ("Init", self.get_init_expr, self.set_init_expr),
                ("Name", self.get_name, self.set_name) ]

    def is_area(self):
        return True

    def as_xml(self):
        e = self.create_xml_element("area")
        e.set("init-expr", self.init_expr)
        e.set("name", self.name)
        e.set("x", str(self.position[0]))
        e.set("y", str(self.position[1]))
        e.set("sx", str(self.size[0]))
        e.set("sy", str(self.size[1]))
        return e

    def export_xml(self):
        e = self.create_xml_element("area")
        e.set("init-expr", self.init_expr)
        e.set("name", self.name)
        items = [ item for item in self.net.places() if self.is_inside(item) ]
        for item in items:
            element = xml.Element("place")
            element.set("id", str(item.id))
            e.append(element)
        return e

    def places(self):
        return [ place for place in self.net.places() if self.is_inside(place) ]

    def transitions(self):
        return [ transition for transition in self.net.transitions() if self.is_inside(transition) ]

class InterfaceBox(RectItem):

    z_level = -2

    def get_drawing(self, vconfig):
        return vconfig.interface_drawing(self)

    def resize_ltop(self, original_pos, original_size, rel_change):
        orig = self.position
        RectItem.resize_ltop(self, original_pos, original_size, rel_change)
        self.search_and_set_nodes(orig, self.position)

    def resize_lbottom(self, original_pos, original_size, rel_change):
        orig = (self.position[0], self.position[1] + self.size[1])
        RectItem.resize_lbottom(self, original_pos, original_size, rel_change)
        self.search_and_set_nodes(orig, (self.position[0], self.position[1] + self.size[1]))

    def resize_rtop(self, original_pos, original_size, rel_change):
        orig = (self.position[0] + self.size[0], self.position[1])
        RectItem.resize_rtop(self, original_pos, original_size, rel_change)
        self.search_and_set_nodes(orig, (self.position[0] + self.size[0], self.position[1]))

    def resize_rbottom(self, original_pos, original_size, rel_change):
        orig = utils.vector_add(self.position, self.size)
        RectItem.resize_rbottom(self, original_pos, original_size, rel_change)
        self.search_and_set_nodes(orig, utils.vector_add(self.position, self.size))

    def search_and_set_nodes(self, old, new):
        for item in filter(lambda i: i.position[0] == old[0], self.net.inodes()):
            item.position = (new[0], item.position[1])
            item.changed()
        for item in filter(lambda i: i.position[1] == old[1], self.net.inodes()):
            item.position = (item.position[0], new[1])
            item.changed()

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

    def get_drawing(self, vconfig):
        return vconfig.interfacenode_drawing(self)

    def set_position(self, position):
        px, py = self.net.interface_box.get_position()
        sx, sy = self.net.interface_box.get_size()

        nx = max(px, min(position[0], px + sx))
        ny = max(py, min(position[1], py + sy))

        pos = [
            (abs(px - position[0]), (px, ny)),
            (abs(py - position[1]), (nx, py)),
            (abs(px + sx - position[0]), (px + sx, ny)),
            (abs(py + sy - position[1]), (nx, py + sy)),
        ]
        self.position = min(pos, key=lambda x: x[0])[1]
        self.changed()

    def get_action(self, position, factory):
        return factory.get_move_action(self.position, self.set_position, position)

    def is_at_position(self, position):
        return utils.point_distance(self.position, position) < 6

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

def load_code(element):
    if element.find("code") is not None:
        return element.find("code").text
    else:
        return ""

def load_place(element, net, loader):
    id = loader.get_id(element)
    place = net.add_place((xml_int(element,"x"), xml_int(element, "y")), id)
    place.radius = xml_int(element,"radius")
    place.place_type = xml_str(element,"place_type", "")
    place.init_string = xml_str(element,"init_string", "")
    place.code = load_code(element)

def load_transition(element, net, loader):
    id = loader.get_id(element)
    transition = net.add_transition((xml_int(element,"x"), xml_int(element, "y")), id)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    transition.size = (sx, sy)
    transition.name = xml_str(element,"name", "")
    transition.guard = xml_str(element,"guard", "")
    if element.get("subnet") is not None:
        """ Subnet should be network, but all networks is not yet loaded so we only remember the id
            and we replace id by network lately """
        transition.subnet = xml_int(element,"subnet")
    transition.code = load_code(element)

def load_edge(element, net, loader):
    id = loader.get_id(element)
    fitem = net.item_by_id(loader.translate_id(xml_int(element, "from_item")))
    assert fitem is not None
    titem = net.item_by_id(loader.translate_id(xml_int(element, "to_item")))
    assert titem is not None
    points = [ (xml_int(e, "x"), xml_int(e,"y")) for e in element.findall("point") ]
    edge = net.add_edge(fitem, titem, points, id)
    edge.bidirectional = utils.xml_bool(element, "bidirectional", False)

    if element.get("inscription") is not None:
        edge.inscription = xml_str(element, "inscription")
        ix = xml_int(element, "inscription_x")
        iy = xml_int(element, "inscription_y")
        edge.inscription_position = (ix, iy)
        edge.inscription_point, edge.inscription_param = utils.nearest_point_of_multiline(edge.get_all_points(), edge.inscription_position)
        edge.offset = utils.vector_diff(edge.compute_insciption_point() , edge.inscription_position)

def load_area(element, net, loader):
    id = loader.get_id(element)
    sx = xml_int(element,"sx")
    sy = xml_int(element,"sy")
    px = xml_int(element, "x")
    py = xml_int(element, "y")
    area = net.add_area((px, py), (sx, sy), id)
    area.init_expr = xml_str(element,"init-expr", "")
    area.name = xml_str(element, "name", "")

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

def nets_postload_process(project, loader):
    for net in project.get_nets():
        for transition in net.transitions():
            if transition.subnet is not None and isinstance(transition.subnet, int):
                transition.subnet = project.find_net(loader.translate_id(transition.subnet))

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
        net.set_autohalt(utils.xml_bool(element, "autohalt", True))

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


