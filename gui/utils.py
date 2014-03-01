#
#    Copyright (C) 2010 Stanislav Bohm
#                  2012 Martin Surkovsky
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
import re
import time
import numpy as np

id_counter = 1000
def get_unique_id():
    global id_counter
    id_counter += 1
    return id_counter

def empty_fn(*args, **kwords):
    pass

def make_vector(point1, point2):
    return (point2[0] - point1[0], point2[1] - point1[1])

def make_vector_with_size(point1, point2, size):
    return vector_mul_scalar(normalize_vector(make_vector(point1, point2)), size)

def middle_point(point1, point2):
    return ((point2[0] + point1[0]) / 2.0, (point2[1] + point1[1]) / 2.0)

def vector_add(vector1, vector2):
    return (vector1[0] + vector2[0], vector1[1] + vector2[1])

def vector_add_t(vector1, vector2, t):
    return (vector1[0] + vector2[0] * t, vector1[1] + vector2[1] * t)

def vector_diff(vector1, vector2):
    return (vector1[0] - vector2[0], vector1[1] - vector2[1])

def vector_mul_scalar(vector, scalar):
    return (vector[0] * scalar, vector[1] * scalar)

def vector_add_scalar(vector, scalar):
    return (vector[0] + scalar, vector[1] + scalar)

def vector_at_least(vector, x, y):
    if vector[0] >= x and vector[1] >= y:
        return vector
    else:
        return (max(x, vector[0]), max(y, vector[1]))

def interpolate(point1, point2, param):
    x1, y1 = point1
    x2, y2 = point2

    return ((x2 - x1) * param + x1,
            (y2 - y1) * param + y1)

def vector_len(vec):
    vx, vy = vec
    return math.sqrt(vx * vx + vy * vy)

def normalize_vector(vec):
    d = vector_len(vec)
    if d == 0.0:
        return (0.0, 0.0)
    return (vec[0] / d, vec[1] / d)

def position_and_size_from_points(point1, point2):
    p1x, p1y = point1
    p2x, p2y = point2

    ax = min(p1x, p2x)
    ay = min(p1y, p2y)
    bx = max(p1x, p2x)
    by = max(p1y, p2y)

    return ((ax, ay), (bx - ax, by - ay))

def position_inside_rect(position, rect_position, size, tolerance=0):
    px, py = position
    rx, ry = rect_position
    sx, sy = size
    return (px >= rx - tolerance and
            py >= ry - tolerance and
            px < rx + sx + tolerance and
            py < ry + sy + tolerance)

def is_round_rectangle_in_rect(position, size, radius, rect_position, rect_size):
    #FIXME: Consider radius
    return (position_inside_rect(position, rect_position, rect_size) and
            position_inside_rect(vector_add(position, size), rect_position, rect_size))


def position_on_rect(position, rect_position, size, tolerance=0):
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

def pairs_generator(lst):
    for x in xrange(len(lst) - 1):
        yield (lst[x], lst[x+1])

def text_size(cr, text, min_h=0, min_w=0):
    extends = cr.text_extents(text)
    # extends[1] is negative and it means distance from the top
    # to the baseline of a text
    return max(min_w, extends[2]), max(min_h, -extends[1])

def snap_to_grid(point, grid_size):
    if grid_size == 1:
        return point
    px, py = point
    return (int(px / grid_size) * grid_size, int(py / grid_size) * grid_size)

def point_square_distance(point1, point2):
    dx, dy = make_vector(point1, point2)
    return dx*dx + dy*dy

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

def is_near_line_segment(point1, point2, position, tolerance):
    length = point_distance(point1, point2) + tolerance
    if point_distance(position, point1) > length:
        return False
    if point_distance(position, point2) > length:
        return False
    return distance_to_line(point1, point2, position) <= tolerance

def nearest_point_on_line(line_start, line_vector, point):
    lx, ly = line_vector
    d = lx * lx + ly * ly
    return float(point[0]*lx-line_start[1]*ly+point[1]*ly-line_start[0]*lx) / d

def nearest_point_to_points(points, point):
    if len(points) == 0:
        return 0
    nearest = point_distance(points[0], point)
    index = 0
    for i, p in enumerate(points):
        dist = point_distance(p, point)
        if nearest > dist:
            nearest = dist
            index = i
    return index

def nearest_point_of_multiline(line_points, point):
    nearest = 100000;
    nearest_index = -1
    param = 0

    for i, (p1, p2) in enumerate(pairs_generator(line_points)):
        t = nearest_point_on_line(p1, make_vector(p1, p2), point)
        if t >= 0.0 and t <= 1.0:
            dist = distance_to_line(p1, p2, point)
            if dist < nearest:
                nearest = dist
                nearest_index = i
                param = t

    tmp = nearest_point_to_points(line_points, point)
    if nearest_index != -1:
        if nearest_index != len(line_points) - 1:
            d = distance_to_line(line_points[nearest_index], line_points[nearest_index + 1], point)
            if d > 5 * vector_len(make_vector(line_points[tmp], point)):
                return tmp, 0
        return nearest_index, param
    else:
        return tmp, param

def abs_vector(vector):
    return (abs(vector[0]), abs(vector[1]))

def find_by_first(lst, key):
    for item in lst:
        if item[0] == key:
            return item
    return None

def index_of_minimal_value(items):
    """ Return index of minimal value in list, ignore None value """
    value = None
    for i in xrange(0, len(items)):
        if items[i] is not None:
           value = items[i]
           break

    if value is None:
          return None

    index = i
    for j in xrange(i + 1, len(items)):
         if items[j] is not None and value > items[j]:
             value = items[j]
             index = j
    return index

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

def write_file_if_not_exists(filename, content):
    if not os.path.exists(filename):
        with open(filename, "w") as f:
            f.write(content)

def makedir_if_not_exists(dirname):
    if not os.path.isdir(dirname):
        os.makedirs(dirname)

def time_to_string(nanosec, seconds=False):
    nanosec=long(nanosec)
    s = nanosec / 1000000000
    nsec = nanosec % 1000000000
    if seconds:
        return "{0:0>2}:{1:0>9}".format(s, nsec)
    sec = s % 60
    minutes = (s / 60) % 60
    hours = s / 60 / 60
    return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)

def mkdir_if_needed_and_open(filename, mode="w"):
    directory = os.path.dirname(filename)
    if not os.path.isdir(directory):
       os.makedirs(directory)
    return open(filename, mode)

# Collision with line (point, direction) = point, velocity
# Circle (center, radius)
# Returns triplet (cross_x, cross_y, t)
def circle_collision(point, velocity, center, radius):
    a = velocity[0]*velocity[0]+velocity[1]*velocity[1];
    b = 2 * (point[0]*velocity[0]+point[1]*velocity[1]- \
        velocity[0]*center[0]-velocity[1]*center[1]);
    c = center[0]*center[0]+center[1]*center[1]-radius*radius-2*point[0]*center[0] \
        -2*point[1]*center[1]+point[0]*point[0]+point[1]*point[1]
    d = b*b-4*a*c;

    if d<0.00001:
        return None

    t1 = (-b+math.sqrt(d))/(2*a);
    t2 = (-b-math.sqrt(d))/(2*a);

    tax = point[0] + velocity[0] * t1;
    tay = point[1] + velocity[1] * t1;
    tbx = point[0] + velocity[0] * t2;
    tby = point[1] + velocity[1] * t2;

    if t1 > -0.000001 and t1 < 1.000001:
        if t2 > -0.000001 and t2 < 1.000001:
            if t1 < t2:
                return (tax, tay, t1)
            else:
                return (tbx, tby, t2)
        else:
            return (tax, tay, t1)
    else:
        if t2 > -0.000001 and t2 < 1.000001:
            return (tbx, tby, t2)
        else:
            return None

def line_intersec_get_t(p1, v1, p2, v2):
    d = v2[0] * v1[1] - v2[1] * v1[0]
    if abs(d) < 0.000001:
        return None
    z = p2[1] * v1[0] - p1[1] * v1[0] - p2[0] * v1[1]  + p1[0] * v1[1]
    s = z / d

    if s < -0.000001 or s > 1.000001:
        return None

    if abs(v1[1]) > abs(v1[0]):
        t = (p2[1] - p1[1] + v2[1] * s) / v1[1]
    else:
        t = (p2[0] - p1[0] + v2[0] * s) / v1[0]

    if t < -0.000001 or t > 1.000001:
        return None

    return t

def is_in_round_rectangle(position, size, r, point, tolerance=5):
    px, py = position
    sx, sy = size[0] + tolerance, size[1] + tolerance

    # current position
    cpx, cpy = point

    if  (px - r) <= cpx <= (px + sx + r) and \
            (py - r) <= cpy <= (py + sy + r):

        if ((px <= cpx <= (px+sx)) and (py-r) <= cpy <= (py+sy+r)) or \
                ((px-r) <= cpx <= (px+sx+r) and (py <= cpy <= (py+sy))):
            return True
        else:
            r_sq = r * r
            if point_square_distance((px,py), (cpx, cpy)) <= r_sq or \
                   point_square_distance((px+sx, py), (cpx, cpy)) <= r_sq or \
                   point_square_distance((px, py+sy), (cpx, cpy)) <= r_sq or \
                   point_square_distance((px+sx, py+sy), (cpx, cpy)) <= r_sq:
                return True
            else:
                return False
    else:
        return False

def make_rect(point1, point2):
    px = min(point1[0], point2[0])
    py = min(point1[1], point2[1])
    sx = max(point1[0], point2[0]) - px
    sy = max(point1[1], point2[1]) - py
    return (px, py), (sx, sy)

def merge_bounding_boxes(box1, box2):
    if box1 is None:
        return box2
    if box2 is None:
        return box1
    a1, a2 = box1
    b1, b2 = box2
    c1 = (min(a1[0], b1[0]),
          min(a1[1], b1[1]))
    c2 = (max(a2[0], b2[0]),
          max(a2[1], b2[1]))
    return (c1, c2)

integer_parser = re.compile("\d+")
def is_integer(value):
    return bool(integer_parser.match(value))

def get_filename_suffix(filename):
    suffix = os.path.splitext(filename)[1]
    if suffix.startswith("."):
        return suffix[1:]

def trim_filename_suffix(filename):
    return os.path.splitext(filename)[0]

def get_timestamp_string():
    return time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(time.time()))

def convert_to_type(numpy_type_description, value):
    return np.dtype(numpy_type_description).type(value)

def numpy_type_to_string(numpy_type_description):
    return np.dtype(numpy_type_description).name

ctypes_to_numpy_types_dict = { 'int': '<i4',
                               'double': '<f8',
                               'std::string': 'O' }

def ctype_to_numpy_type(ctype):
    return ctypes_to_numpy_types_dict[ctype]

def collapse_line_repetitions(items):
    def add(line, count):
        if count == 1:
            result.append(line)
        else:
            result.append(format("[{0}x] {1}".format(count, line)))

    result = []
    if not items:
        return result

    count = 1
    i = iter(items)
    last = i.next()

    for line in i:
        if line != last:
            add(last, count)
            count = 1
            last = line
        else:
            count += 1
    add(last, count)
    return result

def sanitize_name(name):
    if "\n" not in name:
        return name
    return name.replace("\n", "_")


class EqMixin(object):

    def __eq__(self, other):
        return (isinstance(other, self.__class__)
            and self.__dict__ == other.__dict__)

    def __ne__(self, other):
        return not self.__eq__(other)
