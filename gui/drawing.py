#
#    Copyright (C) 2010-2013 Stanislav Bohm
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

def draw_round_rectangle(cr, px, py, sx, sy, radius):
    """
        It draws an rectangle with acr corners.

        px -- x position of base rectangle
        py -- y position of base rectangle
        sx -- width of base rectangle
        sy -- height of base rectangle
        radius -- radius in corner
    """
    cr.new_sub_path()
    cr.arc(px, py, radius, math.pi, 1.5*math.pi)
    cr.line_to(px + sx, py - radius)

    cr.arc(px + sx, py, radius, 1.5*math.pi, 0)
    cr.line_to(px + sx + radius, py + sy)

    cr.arc(px + sx, py + sy, radius, 0, math.pi/2)
    cr.line_to(px, py + sy + radius)

    cr.arc(px, py + sy, radius, math.pi/2, math.pi)
    cr.line_to(px - radius, py)
    cr.close_path()

def draw_centered_text(cr, px, py, text):
    draw_text(cr, px, py, text, 0.5, 0.5)

def draw_text(cr, px, py, text,
              align_x, align_y,
              padding_x=0, padding_y=0,
              background_color=None,
              border_color=None,
              radius=None):
    lines = text.strip().replace("\t", "    ").split("\n")
    tx = max(utils.text_size(cr, text)[0] for text in lines)
    w_height = utils.text_size(cr, "W")[1] + 2
    count = len(lines)
    x = px - tx * align_x
    y = py + w_height * count * align_y
    sx = tx + padding_x * 2
    sy = w_height * count + padding_y * 2
    if background_color is not None:
        cr.save()
        cr.set_source_rgba(*background_color)
        if radius:
            rounded_rectangle(cr, x - padding_x, y - padding_y - sy, sx, sy, radius)
        else:
            cr.rectangle(x - padding_x, y - padding_y - w_height * count, sx, sy)
        cr.fill()
        cr.restore()
    if border_color is not None:
        cr.save()
        cr.set_source_rgba(*border_color)
        if radius:
            rounded_rectangle(cr, x - padding_x, y - padding_y - sy, sx, sy, radius)
        else:
            cr.rectangle(x - padding_x, y - padding_y - w_height * count, sx, sy)
        cr.stroke()
        cr.restore()
    for i in xrange(count):
        cr.move_to(x, y - i * w_height)
        cr.show_text(lines[count - i - 1])
    return (sx, sy)

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
        draw_arrow(cr, utils.make_vector(points[1], points[0]), arrow_degrees, arrow_len)

    cr.move_to(prev[0],prev[1])
    for i in xrange(1, len(points) - 1):
        a = utils.make_vector(points[i-1], points[i])
        b = utils.make_vector(points[i], points[i + 1])
        la = utils.vector_len(a)
        lb = utils.vector_len(b)
        if la < 0.01 or lb < 0.01:
            continue
        v = utils.vector_mul_scalar(utils.normalize_vector(a), min(la, 20.0))
        w = utils.vector_mul_scalar(utils.normalize_vector(b), min(lb, 20.0))
        t = utils.vector_diff(points[i], v)
        cr.line_to(t[0], t[1])
        cr.rel_curve_to(v[0], v[1], v[0], v[1], v[0] + w[0], v[1] + w[1])
    cr.line_to(ex,ey)
    cr.stroke()

    if arrow_end:
        cr.move_to(ex, ey)
        draw_arrow(cr, utils.make_vector(points[-2], points[-1]), arrow_degrees, arrow_len)

def rounded_rectangle(cr, x, y, w, h, r):
    cr.move_to(x + r, y)
    cr.line_to(x + w-r, y)
    cr.curve_to(x + w, y, x + w, y, x + w, y + r)
    cr.line_to(x + w,y + h - r)
    cr.curve_to(x + w,y + h,x + w, y + h, x + w-r, y + h)
    cr.line_to(x + r, y + h)
    cr.curve_to(x, y + h, x, y + h, x, y + h - r)
    cr.line_to(x, y + r)
    cr.curve_to(x, y, x, y, x + r, y)

def draw_label(cr, x, y, text, symbol, text_color, background_color):
    tw = 0
    th = 0
    for txt in text:
        tx, ty = utils.text_size(cr, txt, 8)
        th += ty
        tw = tw if tw > tx else tx
    cr.set_line_width(2)
    rounded_rectangle(cr, x, y, tw + 40, th + len(text) * 10, 10)
    cr.set_source_rgba(background_color[0],
                       background_color[1],
                       background_color[2],
                       0.9)
    cr.fill()
    rounded_rectangle(cr, x, y, tw + 40, th + len(text) * 10, 10)
    cr.set_source_rgb(*background_color)
    cr.stroke()

    cr.set_source_rgb(*text_color)
    th = -5
    for txt in text:
        tx, ty = utils.text_size(cr, txt, 8)
        th += ty + 10
        cr.move_to(x + 25, y + th)
        cr.show_text(txt)
    cr.fill()

    if symbol == "lookingglass":
        cr.arc(x + 12, y + 7, 4, 0, 2 * math.pi)
        cr.move_to(x + 16, y + 11)
        cr.rel_line_to(5, 5)
        cr.stroke()
    elif symbol == "arrow":
        cr.set_line_width(1)

        cr.move_to(x + 8, y + 5)
        cr.rel_line_to(4, 4)
        cr.rel_line_to(-4, 4)
        cr.close_path()
        cr.stroke()

        cr.move_to(x + 13, y + 4)
        cr.rel_line_to(5, 5)
        cr.rel_line_to(-5, 5)
        cr.fill()

        rounded_rectangle(cr, x + 5, y + 2, 15, 14, 3)
        cr.stroke()

