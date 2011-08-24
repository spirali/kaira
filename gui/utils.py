#
#    Copyright (C) 2010 Stanislav Bohm
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
import os


def make_vector(point1, point2):
	return (point2[0] - point1[0], point2[1] - point1[1])


def middle_point(point1, point2):
	return ((point2[0] + point1[0]) / 2.0, (point2[1] + point1[1]) / 2.0)


def vector_add(vector1, vector2):
	return (vector1[0] + vector2[0], vector1[1] + vector2[1])

def vector_diff(vector1, vector2):
	return (vector1[0] - vector2[0], vector1[1] - vector2[1])

def vector_mul_scalar(vector, scalar):
	return (vector[0] * scalar, vector[1] * scalar)

def vector_add_scalar(vector, scalar):
	return (vector[0] + scalar, vector[1] + scalar)

def vector_len(vec):
	vx, vy = vec
	return math.sqrt(vx * vx + vy * vy)


def normalize_vector(vec):
	d = vector_len(vec)
	return (vec[0] / d, vec[1] / d)


def position_and_size_from_points(point1, point2):
	p1x, p1y = point1
	p2x, p2y = point2

	ax = min(p1x, p2x)
	ay = min(p1y, p2y)
	bx = max(p1x, p2x)
	by = max(p1y, p2y)

	return ((ax, ay), (bx - ax, by - ay))


def position_inside_rect(position, rect_position, size, tolerance = 0):
	px, py = position
	rx, ry = rect_position
	sx, sy = size
	return px >= rx - tolerance and py >= ry - tolerance and px < rx + sx + tolerance and py < ry + sy + tolerance

def position_on_rect(position, rect_position, size, tolerance = 0):
	if not position_inside_rect(position, rect_position, size, tolerance):
		return False
	px, py = position
	rx, ry = rect_position
	sx, sy = size
	return px <= rx + tolerance or py <= ry + tolerance \
		or px > rx + sx - tolerance or py > ry + sy - tolerance

def translate(idtable, source):
	output = {}
	for key, value in source.items():
		output[idtable[key]] = value
	return output

def join_dicts(dict1, dict2, merge_fun = None):
	x = dict1.copy()
	for key, value in dict2.items():
		if x.has_key(key):
			if merge_fun is None:
				raise Exception("Both dictionaries has same key: " + str(key))
			value = merge_fun(value, x[key])
		x[key] = value
	return x

def draw_arrow(cr, dir_vector, arrow_degrees, arrow_len):
	dx, dy = dir_vector
	angle = math.atan2 (dx, dy) + math.pi;
	x1 = arrow_len * math.sin(angle - arrow_degrees);
	y1 = arrow_len * math.cos(angle - arrow_degrees);
	x2 = arrow_len * math.sin(angle + arrow_degrees);
	y2 = arrow_len * math.cos(angle + arrow_degrees);
	cr.rel_line_to(x1, y1)
	cr.rel_line_to(x2 - x1, y2 - y1)
	cr.rel_line_to(-x2, -y2)
	cr.fill()

def draw_polyline_arrow(cr, points, arrow_degrees, arrow_len):

	sx, sy = points[-2]
	ex, ey = points[-1]

	angle = math.atan2 (ey-sy, ex - sx) + math.pi;
	x1 = ex + arrow_len * math.cos(angle - arrow_degrees);
	y1 = ey + arrow_len * math.sin(angle - arrow_degrees);
	x2 = ex + arrow_len * math.cos(angle + arrow_degrees);
	y2 = ey + arrow_len * math.sin(angle + arrow_degrees);

	cr.move_to(points[0][0],points[0][1])
	for (px, py) in points:
		cr.line_to(px, py)
	cr.stroke()

	cr.line_to(x1, y1)
	cr.line_to(x2, y2)
	cr.line_to(ex, ey)
	cr.fill()

def draw_polyline_nice_corners(cr, points, arrow_degrees, arrow_len, arrow_start = False, arrow_end = False):
	ex, ey = points[-1]
	prev = points[0]

	if arrow_start:
		cr.move_to(prev[0],prev[1])
		draw_arrow(cr, make_vector(points[1], points[0]), arrow_degrees, arrow_len)

	cr.move_to(prev[0],prev[1])
	for i in xrange(1, len(points) - 1):
		a = make_vector(points[i-1], points[i])
		b = make_vector(points[i], points[i + 1])
		la = vector_len(a)
		lb = vector_len(b)
		if la < 0.01 or lb < 0.01:
			continue
		v = vector_mul_scalar(normalize_vector(a), min(la, 20.0))
		w = vector_mul_scalar(normalize_vector(b), min(lb, 20.0))
		t = vector_diff(points[i], v)
		cr.line_to(t[0], t[1])
		cr.rel_curve_to(v[0], v[1], v[0], v[1], v[0] + w[0], v[1] + w[1])
	cr.line_to(ex,ey)
	cr.stroke()

	if arrow_end:
		cr.move_to(ex, ey)
		draw_arrow(cr, make_vector(points[-2], points[-1]), arrow_degrees, arrow_len)

def pairs_generator(lst):
	for x in xrange(len(lst) - 1):
		yield (lst[x], lst[x+1])

def text_size(cr, text):
	extends = cr.text_extents(text)
	return (extends[2], extends[3])

def snap_to_grid(point, grid_size):
	px, py = point
	return (int(px / grid_size) * grid_size, int(py / grid_size) * grid_size)

def point_distance(point1, point2):
	return vector_len(make_vector(point1, point2))


def distance_to_line(line_point1, line_point2, point):
	px, py = point
	x1, y1 = line_point1
	x2, y2 = line_point2

	vx = x2 - x1
	vy = y2 - y1

	nx = -vy
	ny = vx
	c = - (x1 * nx + y1 * ny)

	return abs(nx * px + ny * py + c) / math.sqrt(nx * nx + ny * ny)

def abs_vector(vector):
	return (abs(vector[0]), abs(vector[1]))

def find_by_first(lst, key):
	for item in lst:
		if item[0] == key:
			return item
	return None

def xml_int(element, attr, default = None):
	if element.get(attr) is None:
		if default is not None:
			return default
		else:
			raise Exception("Element has no attribute: " + attr)
	return int(float(element.get(attr)))

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

class Makefile:
	""" Simple class for emitting makefile """

	def __init__(self):
		self.variables = []
		self.rules = []
		self.top_comment = ""

	def set(self, variable, value):
		self.variables.append((variable, value))

	def rule(self, target, deps, command = None):
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

def write_file_if_not_exists(filename, content):
	if not os.path.exists(filename):
		with open(filename, "w") as f:
			f.write(content)
