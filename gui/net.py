#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                  2011       Ondrej Garncarz
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

class ExportException(Exception):
	pass

class Net:
	
	def __init__(self, project):
		self.project = project
		self.items = []
		self.change_callback = lambda n: None

	def set_change_callback(self, callback):
		self.change_callback = callback

	def new_id(self):
		return self.project.new_id()

	def add_item(self, item):
		if item.id is None:
			item.id = self.new_id()
		self.items.append(item)
		self.changed()

	def remove_item(self, item):
		self.items.remove(item)
		self.changed()

	def changed(self):
		self.change_callback(self)

	def draw(self, cr, vconfig):
		drawings = [ item.get_drawing(vconfig) for item in self.items ]
		drawings.sort(key=lambda x: x.z_level())
		for drawing in drawings:
			drawing.draw(cr)
		for drawing in drawings:
			drawing.draw_top(cr)

	def add_place(self, position, id = None):
		place = Place(self, id, position)
		self.add_item(place)
		self.changed()
		return place

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

		for item in self.items:
			e.append(item.as_xml())
		return e

	def copy(self):
		xml = self.as_xml()
		return load_net(xml, self.project)

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

	def export_xml(self):
		e = xml.Element("net")

		for place in self.places():
			e.append(place.export_xml())

		for transition in self.transitions():
			e.append(transition.export_xml())

		for transition in self.areas():
			e.append(transition.export_xml())
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


class NetItem(object):

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

	def delete(self):
		self.net.delete_item(self)

	def create_xml_element(self, name):
		element =  xml.Element(name)
		element.set("id", str(self.id))
		return element

class NetElement(NetItem):

	code = ""

	def __init__(self, net, id, position):
		NetItem.__init__(self, net, id)
		self.position = position

	def has_code(self):
		return self.code.strip() != ""

	def get_code(self):
		return self.code

	def set_code(self, code):
		self.code = code
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

	def as_xml(self):
		e = self.create_xml_element("transition")
		e.set("name", self.name)
		e.set("guard", self.guard)
		e.set("x", str(self.position[0]))
		e.set("y", str(self.position[1]))
		e.set("sx", str(self.size[0]))
		e.set("sy", str(self.size[1]))
		if self.has_code():
			e.append(self.xml_code_element())
		return e

	def export_xml(self):

		def make_edge(name, edge, place):
			ea = xml.Element(name)
			ea.set("place-id", str(place.get_id()))
			ea.set("id", str(edge.get_id()))
			ea.set("expr", edge.inscription)
			return ea
		

		e = self.create_xml_element("transition")
		e.set("name", self.name)
		e.set("guard", self.guard)
		if self.has_code():
			e.append(self.xml_code_element())

		for edge in self.edges_to(postprocess = True):
			ea = make_edge("edge-in", edge, edge.from_item);
			e.append(ea)

		for edge in self.edges_from(postprocess = True):
			ea = make_edge("edge-out", edge, edge.to_item);
			e.append(ea)
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
			return factory.get_resize_action(self, position)

		if px >= mx - sx and py >= my - sy and px < mx + sx and py < my + sy:
			return factory.get_move_action(self.get_position, self.set_position, position)

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

	def export_xml(self):
		e = self.create_xml_element("place")
		e.set("name", "name")
		e.set("type", self.place_type)
		e.set("init-expr", self.init_string)
		if self.has_code():
			e.append(self.xml_code_element())
		return e

	def get_drawing(self, vconfig):
		return vconfig.place_drawing(self)

	def is_at_position(self, position):
		dist = utils.point_distance(self.position, position)
		return dist < self.radius + 5

	def get_action(self, position, factory):
		dist = utils.point_distance(self.position, position)

		if dist < self.radius + 5 and dist > self.radius - 5:
			return factory.get_resize_action(self, position)

		if dist < self.radius:
			return factory.get_move_action(self.get_position, self.set_position, position)

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

	def get_inscription(self):
		return self.inscription

	def set_inscription(self, inscription):
		if self.inscription_position is None:
			self.inscription_position = self.default_inscription_position()
		self.inscription = inscription
		self.changed()

	def is_bidirectional(self):
		return self.bidirectional

	def toggle_bidirectional(self):
		self.bidirectional = not self.bidirectional
		self.changed()

	def set_inscription_position(self, position):
		self.inscription_position = position
		self.changed()

	def get_inscription_position(self):
		if self.inscription_position is None:
			return self.default_inscription_position()
		else:
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

	def default_inscription_position(self):
		points = self.get_all_points()
		bp1 = points[len(points) / 2 - 1]
		bp2 = points[len(points) / 2]
		vec = utils.normalize_vector(utils.make_vector(bp1, bp2))
		vec = utils.vector_mul_scalar(vec, 5)
		vec = (vec[1], -vec[0])
		return utils.vector_add(utils.middle_point(bp1, bp2), vec)
	
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
		if self.inscription_position and utils.position_inside_rect(position, self.inscription_position, self.inscription_size, 4):
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
		if self.inscription_position and utils.position_inside_rect(position, self.inscription_position, self.inscription_size, 4):
			return factory.get_move_action(self.get_inscription_position, self.set_inscription_position, position)

		for i, p in enumerate(self.points):
			if utils.point_distance(p, position) < 7:
				return factory.get_move_action(lambda: p, lambda x: set_point(i, x), position)
		return factory.get_empty_action()

	def get_text_entries(self):
		return [ ("Inscription", self.get_inscription, self.set_inscription) ]

	def corners(self):
		# FIXME
		return ((0,0), (0,0))

	def is_packing_edge(self):
		return self.inscription and self.inscription[0] == "~"

class NetArea(NetItem):

	name = ""
	init_expr = ""

	def __init__(self, net, id, position, size):
		NetItem.__init__(self, net, id)
		self.position = position
		self.size = size

	def get_size(self):
		return self.size

	def resize(self, point):
		self.size = point
		self.changed()

	def get_position(self):
		return self.position

	def set_position(self, position):
		self.position = position
		self.changed()

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

	def is_at_position(self, position):
		px, py = position
		mx, my = self.position
		sx, sy = self.size
		return px >= mx - 10 and py >= my - 10 and px < mx + sx + 10 and py < my + sy + 10

	def get_action(self, position, factory):
		px, py = position
		mx, my = self.position
		sx, sy = self.size

		if px >= mx + sx - 10 and py >= my + sy - 10 and px < mx + sx + 5 and py < my + sy + 5:
			return factory.get_resize_action(self, position)

		if px >= mx and py >= my and px < mx + sx and py < my + sy:
			return factory.get_move_action(self.get_position, self.set_position, position)

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

	def is_inside(self, item):
		return utils.position_inside_rect(item.position, self.position, self.size)
		
	def places(self):
		return [ place for place in self.net.places() if self.is_inside(place) ]

	def transitions(self):
		return [ transition for transition in self.net.transitions() if self.is_inside(transition) ]

	def corners(self):
		px, py = self.position
		sx, sy = self.size
		return (self.position, (sx + px, sy + py))

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

def load_area(element, net, loader):
	id = loader.get_id(element)
	sx = xml_int(element,"sx")
	sy = xml_int(element,"sy")
	px = xml_int(element, "x")
	py = xml_int(element, "y")
	area = net.add_area((px, py), (sx, sy), id)
	area.init_expr = xml_str(element,"init-expr", "")
	area.name = xml_str(element, "name", "")

def load_net(element, project, loader):
	net = Net(project)

	for e in element.findall("area"):
		load_area(e, net, loader)

	for e in element.findall("place"):
		load_place(e, net, loader)

	for e in element.findall("transition"):
		load_transition(e, net, loader)

	for e in element.findall("edge"):
		load_edge(e, net, loader)

	return net
