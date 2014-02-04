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
import gtk
import cairo

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
    return draw_text(cr, px, py, text, 0.5, 0.5)

def draw_text(cr, px, py, text,
              align_x, align_y,
              padding_left=0, padding_top=0,
              padding_right=0, padding_bottom=0,
              background_color=None,
              border_color=None,
              border_width=1,
              radius=None):
    if isinstance(text, str) or isinstance(text, unicode):
        lines = text.replace("\t", "    ").split("\n")
    else:
        lines = text
    sizes = [ utils.text_size(cr, text)[0] for text in lines ]
    tx = max(sizes)
    w_height = utils.text_size(cr, "W")[1] + 2
    count = len(lines)
    x = px - tx * align_x
    y = py + w_height * count * align_y
    sx = tx + padding_left + padding_right
    sy = w_height * count + padding_top + padding_bottom
    if background_color is not None:
        cr.save()
        cr.set_source_rgba(*background_color)
        if radius:
            rounded_rectangle(cr, x - padding_left, y - padding_top - w_height * count, sx, sy, radius)
        else:
            cr.rectangle(x - padding_left, y - padding_top - w_height * count, sx, sy)
        cr.fill()
        cr.restore()
    if border_color is not None:
        cr.save()
        cr.set_source_rgba(*border_color)
        cr.set_line_width(border_width)
        if radius:
            rounded_rectangle(cr, x - padding_left, y - padding_top - w_height * count, sx, sy, radius)
        else:
            cr.rectangle(x - padding_left, y - padding_top - w_height * count, sx, sy)
        cr.stroke()
        cr.restore()
    for i in xrange(count):
        j = count - i - 1
        cr.move_to(px - sizes[j] * align_x, y - i * w_height)
        cr.show_text(lines[j])
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

def draw_label(cr, x, y, text, symbol, text_color, background_color, border_color=None):
    cr.set_source_rgb(*text_color)
    size = draw_text(cr,
              x + 25, y,
              text,
              0, 1,
              padding_left=25, padding_right=6,
              padding_top=4, padding_bottom=6,
              background_color=background_color,
              border_color=border_color,
              border_width=2,
              radius=10)
    if symbol == "lookingglass":
        cr.set_line_width(2)
        cr.new_sub_path()
        cr.arc(x + 10, y + 4, 4, 0, 2 * math.pi)
        cr.move_to(x + 14, y + 8)
        cr.rel_line_to(5, 5)
        cr.stroke()
    elif symbol == "arrow":
        cr.set_line_width(1)
        cr.move_to(x + 8, y + 2)
        cr.rel_line_to(4, 4)
        cr.rel_line_to(-4, 4)
        cr.close_path()
        cr.stroke()

        cr.move_to(x + 13, y + 2)
        cr.rel_line_to(5, 5)
        cr.rel_line_to(-5, 5)
        cr.fill()
        rounded_rectangle(cr, x + 5, y, 15, 14, 3)
        cr.stroke()
    elif symbol == "tick":
        cr.set_line_width(2)
        cr.move_to(x + 5, y + 7)
        cr.rel_line_to(4, 3)
        cr.rel_line_to(9, -10)
        cr.stroke()
    return size


class StateIcon(gtk.DrawingArea):

    def __init__(self, state, width=30, height=30):
        """ Initialize an icon.

        Arguments:
        state -- possible values: "ready", "incomplete", "incorrect"

        Keyword arguments:
        width -- the width of icon (default 30)
        height -- the height of icon (default 30)

        """
        assert (state == "ready" or
                state == "incomplete" or
                state == "incorrect")
        self.icon_state = state
        gtk.DrawingArea.__init__(self)
        self.set_size_request(width, height)
        self.connect("expose_event", self._expose)

    def set_state(self, state):
        assert (state == "ready" or
               state == "incomplete" or
               state == "incorrect")
        self.icon_state = state
        self.queue_draw()

    def _expose(self, widget, event):
        cr = widget.window.cairo_create()
        rect = self.get_allocation()
        self._draw(cr, rect.width, rect.height)

    def _draw(self, cr, width, height):
        # clear background
        cr.set_source_rgb(0.95,0.95,0.95)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        # draw
        x = width / 2
        y = height / 2
        radius = min(width / 2, height / 2) - 5

        cr.arc(x, y, radius, 0, 2 * math.pi)
        if self.icon_state == "ready":
            cr.set_source_rgb(0, 0.8, 0)
        elif self.icon_state == "incomplete":
            cr.set_source_rgb(1, 0.4, 0)
        elif self.icon_state == "incorrect":
            cr.set_source_rgb(1, 0, 0)

        cr.fill()

        radial = cairo.RadialGradient(
            x, height, 0,
            x, height, height-0.2*height)
        radial.add_color_stop_rgba(0, 0, 0, 0, 0.4)
        radial.add_color_stop_rgba(1, 0, 0, 0, 0.0)
        cr.set_source(radial)
        cr.arc(x, y, radius, 0, 2 * math.pi)
        cr.fill()

        cr.set_line_width(1)
        cr.arc(x, y, radius, 0, 2 * math.pi)
        cr.set_source_rgb(0, 0, 0)
        cr.stroke()
