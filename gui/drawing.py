#
#    Copyright (C) 2010, 2011, 2013 Stanislav Bohm
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

class VisualConfig:

    def transition_drawing(self, item):
        return self.preprocess(item, TransitionDrawing(item))

    def place_drawing(self, item):
        return self.preprocess(item, PlaceDrawing(item))

    def edge_drawing(self, item):
        return self.preprocess(item, EdgeDrawing(item))

    def area_drawing(self, item):
        return self.preprocess(item, AreaDrawing(item))

    def interface_drawing(self, item):
        return self.preprocess(item, InterfaceDrawing(item))

    def interfacenode_drawing(self, item):
        return self.preprocess(item, InterfaceNodeDrawing(item))

    def preprocess(self, item, drawing):
        return drawing


class DrawingBase:

    highlight = None
    error_messages = None
    trace_text = []

    def __init__(self):
        pass

    def set_highlight(self, highlight):
        self.highlight = highlight

    def set_error_messages(self, error_messages):
        self.error_messages = error_messages

    def draw_top(self, cr):
        pass


class TransitionDrawing(DrawingBase):

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.position = item.get_position()
        self.size = item.get_size()
        self.name = item.get_name()
        self.guard = item.get_guard()
        self.doubleborder = item.get_code().strip() != ""
        self.executions = None
        self.with_values = False

    def draw(self, cr):
        px, py = self.position
        sx, sy = self.size

        cr.rectangle(px - sx/2, py - sy/2, sx, sy)
        cr.set_source_rgb(1.0,1.0,1.0)
        cr.fill()

        if self.highlight:
            cr.rectangle(px - sx/2, py - sy/2, sx, sy)
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            cr.stroke()

        cr.rectangle(px - sx/2, py - sy/2, sx, sy)
        cr.set_line_width(1.5)
        cr.set_source_rgb(0.0,0.0,0.0)
        cr.stroke()

        if self.doubleborder:
            cr.rectangle(px - sx/2 + 4, py - sy/2 + 4, sx - 8, sy - 8)
            cr.stroke()

        if self.name:
            tx, ty = utils.text_size(cr, self.name)
            cr.set_source_rgb(0,0,0)
            cr.move_to(px - tx / 2, py + ty / 2)
            cr.show_text(self.name)

        if self.guard:
            tx, ty = utils.text_size(cr, self.guard)
            cr.set_source_rgb(0.3,0.3,0.3)
            cr.move_to(px - tx / 2, py - self.size[1]/2 - ty/2 - 2)
            cr.show_text(self.guard)

        if self.executions:
            x = self.position[0] - self.size[0] / 2
            y = self.position[1] + self.size[1]
            for text, color in self.executions:
                tx, ty = utils.text_size(cr, text)
                cr.move_to(x - 2, y + ty/2)
                cr.rel_line_to(tx + 10, 0)
                cr.rel_line_to(-4, -ty)
                cr.rel_line_to(4, -ty)
                cr.rel_line_to(-tx - 10, 0)
                cr.rel_line_to(-4, ty)
                cr.rel_line_to(4, ty)
                cr.set_source_rgba(*color)
                cr.fill()
                cr.move_to(x, y)
                x += tx + 12
                cr.set_source_rgb(0, 0, 0)
                cr.show_text(text)
        if self.trace_text:
            draw_trace_box(cr, px - sx / 2 - 5, py - sy / 2 - 5, self.trace_text)

        if self.with_values:
            cr.move_to(px + sx/2 - 20, py - sy/2)
            cr.line_to(px + sx/2, py - sy/2)
            cr.line_to(px + sx/2, py - sy/2 + 20)
            cr.close_path();

            cr.set_source_rgb(0, 0, 0);
            cr.stroke_preserve();
            cr.set_source_rgb(1, 0.5, 0.2);
            cr.fill();

            cr.move_to(px + sx/2 - 9, py - sy/2 + 11)
            cr.set_source_rgb(0, 0, 0)
            cr.show_text("T")

    def draw_top(self, cr):
        if self.error_messages and "guard" in self.error_messages:
            px, py = self.position
            py = py - self.size[1] / 2
            draw_error_box_after_text(cr,
                                      self.guard,
                                      (px, py),
                                      self.error_messages["guard"],
                                      centered=True)
        if self.error_messages and None in self.error_messages:
            draw_error_box(cr, self.position, self.error_messages[None])

def draw_round_rectangle(cr, px, py, sx, sy, radius):
    """
        It draws an rectangle with acr corners.

        px -- x position of base rectangle
        py -- y position of base rectangle
        sx -- width of base rectangle
        sy -- height of base rectangle
        radius -- radius in corner
    """
    cr.arc(px, py, radius, math.pi, 1.5*math.pi)
    cr.line_to(px + sx, py - radius)

    cr.arc(px + sx, py, radius, 1.5*math.pi, 0)
    cr.line_to(px + sx + radius, py + sy)

    cr.arc(px + sx, py + sy, radius, 0, math.pi/2)
    cr.line_to(px, py + sy + radius)

    cr.arc(px, py + sy, radius, math.pi/2, math.pi)
    cr.line_to(px - radius, py)
    cr.close_path()

class PlaceDrawing(DrawingBase):

    max_shown_tokens = 10

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.position = item.get_position()
        self.radius = item.get_radius()
        self.name = item.get_name()
        self.size = item.get_size()
        self.error_messages = None
        self.highlight = None
        self.doubleborder = item.get_code().strip() != ""
        self.init_string = item.get_init_string()
        self.place_type = item.get_place_type()
        self.tokens = None
        self.new_tokens = None
        self.removed_tokens = None

    def set_tokens(self, tokens, new_tokens, removed_tokens):
        self.tokens = []
        for token in tokens:
            if len(token) > 25:
                self.tokens.append(token[:18] + " ... (%i chars)" % len(token))
            else:
                self.tokens.append(token)
        self.new_tokens = []
        for token in new_tokens:
            if len(token) > 25:
                self.new_tokens.append(token[:18] + " ... (%i chars)" % len(token))
            else:
                self.new_tokens.append(token)
        self.removed_tokens = []
        for token in removed_tokens:
            if len(token) > 25:
                self.removed_tokens.append(token[:18] + " ... (%i chars)" % len(token))
            else:
                self.removed_tokens.append(token)

    def draw(self, cr):
        px, py = self.position
        sx, sy = self.size

        # draw rounded rectangle
        draw_round_rectangle(cr, px, py, sx, sy, self.radius)
        cr.set_source_rgb(1, 1, 1)
        cr.fill()

        if self.highlight:
            draw_round_rectangle(cr, px, py, sx, sy, self.radius)
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            cr.stroke()

        draw_round_rectangle(cr, px, py, sx, sy, self.radius)
        cr.set_line_width(1.5)
        cr.set_source_rgb(0,0,0)
        cr.stroke()

        if self.doubleborder:
            draw_round_rectangle(cr, px, py, sx, sy, self.radius - 3)
            cr.stroke()

        if self.name:
            tx, ty = utils.text_size(cr, self.name)
            cr.set_source_rgb(0,0,0)
            cr.move_to(px + (sx - tx)/2, py + (sy + ty)/2)
            cr.show_text(self.name)

        x = sx + self.radius + 2
        if self.init_string:
            cr.set_source_rgb(0,0,0)
            cr.move_to(px + x, py - self.radius/2)
            cr.show_text(self.init_string)
        if self.place_type:
            cr.set_source_rgb(0,0,0)
            cr.move_to(px + x, py + sy + self.radius)
            cr.show_text(self.place_type)
        if self.trace_text:
            draw_trace_box(cr, px - 15, py - 15, self.trace_text)

    def draw_top(self, cr):
        px, py = self.position

        if self.tokens or self.new_tokens or self.removed_tokens:
            tokens = self.removed_tokens[:self.max_shown_tokens]
            rem_t_in = len(tokens)
            if len(tokens) < self.max_shown_tokens:
                tokens += self.tokens[:self.max_shown_tokens-len(tokens)]
            t_in = len(tokens) - rem_t_in
            if len(tokens) < self.max_shown_tokens:
                tokens += self.new_tokens[:self.max_shown_tokens-len(tokens)]
            new_t_in = len(tokens) - rem_t_in - t_in
            if len(self.tokens) + len(self.new_tokens) + len(self.removed_tokens) != len(tokens):
                tokens.append("...")
                if new_t_in < len(self.new_tokens):
                    new_t_in += 1
                elif t_in < len(self.tokens):
                    t_in += 1
                elif rem_t_in < len(self.removed_tokens):
                    rem_t_in += 1

            # Draw circle
            xcoord = px + self.size[0] + self.radius
            ycoord = py + self.size[1] / 2
            if self.new_tokens:
                cr.set_source_rgb(0.2,0.6,0)
            else:
                cr.set_source_rgb(0.2,0.45,0)
            cr.arc(xcoord, ycoord,8, 0, 2 * math.pi)
            cr.fill()

            cr.set_line_width(0.5)
            cr.arc(xcoord, ycoord,8, 0, 2 * math.pi)
            cr.set_source_rgb(0,0,0)
            cr.stroke()

            init_text = str(len(self.tokens + self.new_tokens))
            w, h = utils.text_size(cr, init_text)
            cr.set_source_rgb(0.8,0.8,0.8)
            cr.move_to(xcoord - w/2, ycoord + h/2)
            cr.show_text(init_text)

            # Print token names
            w_size = utils.text_size(cr, "W")[1] + 6
            texts = [ (t, utils.text_size(cr, t)[0]) for t in tokens ]
            text_height = len(tokens) * w_size
            text_width = max([ x[1] for x in texts ])
            text_x = xcoord + 12
            text_y = ycoord - text_height / 2

            rem_height = rem_t_in * w_size
            tok_height = t_in * w_size
            new_height = new_t_in * w_size

            # Print gray rectangle for removed tokens
            if self.removed_tokens:
                cr.set_source_rgba(0.2,0.2,0.2,0.6)
                cr.rectangle(text_x - 3, text_y + 4, text_width + 6, rem_height)
                cr.fill()

            # Print green rectangle for tokens
            if self.tokens:
                cr.set_source_rgba(0.2,0.45,0,0.5)
                cr.rectangle(text_x - 3, text_y + rem_height + 4, text_width + 6, tok_height)
                cr.fill()

            # Print bordered rectangle for new tokens
            if self.new_tokens:
                cr.rectangle(text_x - 3, text_y + rem_height + tok_height + 4, text_width + 6, new_height)
                cr.set_source_rgb(0,0,0)
                cr.set_line_width(1.5)
                cr.stroke()
                cr.rectangle(text_x - 3, text_y + rem_height + tok_height + 4, text_width + 6, new_height)
                cr.set_line_width(0.5)
                cr.set_source_rgba(0.2,0.6,0,0.5)
                cr.fill()

            cr.set_source_rgb(0.0,0.0,0.0)
            cr.set_source_rgb(1.0,1.0,1.0)

            for (t, x) in texts:
                text_y += w_size
                cr.move_to(text_x, text_y)
                cr.show_text(t)

        if self.error_messages and "type" in self.error_messages:
            draw_error_box_after_text(
                cr,
                self.place_type,
                (px + self.size[0] + self.radius, py + self.size[1] + self.radius),
                self.error_messages["type"])

        if self.error_messages and "init-expr" in self.error_messages:
            draw_error_box_after_text(
                cr,
                self.init_string,
                (px + self.size[0] + self.radius, py - self.size[1] - self.radius),
                self.error_messages["init-expr"])

class EdgeDrawing(DrawingBase):

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.points = item.get_all_points()
        self.error_messages = None
        self.highlight = None
        self.inscription = item.get_inscription()
        self.inscription_position = item.get_inscription_position()
        self.bidirectional = item.is_bidirectional()
        self.item = item

    def draw(self, cr):

        if self.highlight:
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            utils.draw_polyline_nice_corners(cr, self.points, 0.5, 12, self.bidirectional, True)

        cr.set_line_width(1.5)
        cr.set_source_rgb(0.0,0.0,0.0)
        utils.draw_polyline_nice_corners(cr, self.points, 0.5, 12, self.bidirectional, True)

        point = self.inscription_position
        # Cheap hack how to obtain inscription size
        self.item.inscription_size = utils.text_size(cr, self.inscription)
        sx, sy = self.item.inscription_size
        if self.highlight:
            cr.set_source_rgba(*self.highlight)
            cr.rectangle(point[0] - sx/2.0 + 1, point[1], sx, sy)
            cr.fill()

        cr.set_source_rgb(0,0,0)
        cr.set_line_width(1.0)
        cr.move_to(point[0] - sx/2.0, point[1] + sy)
        cr.show_text(self.inscription)

    def draw_top(self, cr):
        if self.error_messages and "inscription" in self.error_messages:
            draw_error_box_after_text(cr,
                                      self.inscription,
                                      self.inscription_position,
                                      self.error_messages["inscription"],
                                      centered=True)


class AreaDrawing(DrawingBase):

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.position = item.get_position()
        self.size = item.get_size()
        self.error_messages = None
        self.highlight = None
        self.init_expr = item.get_init_expr()
        self.name = item.get_name()

    def draw(self, cr):
        px, py = self.position
        sx, sy = self.size
        cr.set_source_rgb(0.6,0.7,0.9)
        cr.rectangle(px, py, sx, sy)
        cr.fill()

        if self.highlight:
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            cr.rectangle(px, py, sx, sy)
            cr.stroke()

        cr.set_line_width(0.5)
        cr.set_source_rgb(0,0,0)
        cr.rectangle(px, py, sx, sy)
        cr.stroke()

        cr.set_source_rgb(0.4,0.4,0.6)
        cr.move_to(px, py - 5)
        cr.show_text(self.init_expr)

        textx, texty = utils.text_size(cr, self.name)
        cr.move_to(px + sx - textx, py - 5)
        cr.show_text(self.name)

    def draw_top(self, cr):
        if self.error_messages and "init-expr" in self.error_messages:
            px, py = self.position
            draw_error_box_after_text(cr,
                                      self.init_expr,(px, py - 5),
                                      self.error_messages["init-expr"])


class InterfaceDrawing(DrawingBase):

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.position = item.get_position()
        self.size = item.get_size()

    def draw(self, cr):
        px, py = self.position
        sx, sy = self.size

        if self.highlight:
            cr.set_line_width(13.0)
            cr.set_source_rgba(*self.highlight)
            cr.rectangle(px, py, sx, sy)
            cr.stroke()

        cr.set_line_width(6.0)
        cr.set_source_rgb(0.6,0.6,0.6)
        cr.rectangle(px, py, sx, sy)
        cr.stroke()

        cr.set_line_width(1.0)
        cr.set_source_rgb(0.0,0.0,0.0)
        cr.rectangle(px - 3, py - 3, sx + 6, sy + 6)
        cr.stroke()
        cr.rectangle(px + 3, py + 3, sx - 6, sy - 6)
        cr.stroke()


class InterfaceNodeDrawing(DrawingBase):

    def __init__(self, item):
        DrawingBase.__init__(self)
        self.position = item.get_position()

    def draw(self, cr):
        px, py = self.position

        if self.highlight:
            cr.set_line_width(4)
            cr.set_source_rgba(*self.highlight)
            cr.rectangle(px - 8, py - 8, 16,  16)
            cr.stroke()

        cr.set_source_rgb(0.0,0.0,0.0)
        cr.rectangle(px - 6, py - 6, 12,  12)
        cr.fill()


def draw_error_box_after_text(cr, text, position, lines, centered=False):
    if text is not None:
        sx, sy = utils.text_size(cr, text)
        if centered:
            sx = sx / 2
        draw_error_box(cr, (position[0] + 5 + sx, position[1]), lines)
    else:
        draw_error_box(cr, position, lines)

def draw_error_box(cr, position, lines):
    assert len(lines) > 0
    letter_y = utils.text_size(cr,"W")[1] * 1.5
    size_x = max([ utils.text_size(cr, line)[0] for line in lines ]) + 15
    size_y = letter_y * (len(lines) + 1)
    #size_x = max( [ s[0] for s in sizes ] ) + 10
    px, py = position
    cr.set_source_rgb(0.9,0.1,0.1)
    cr.rectangle(px, py - letter_y * 1.3, size_x, size_y)
    cr.fill()
    cr.set_source_rgb(0,0,0)
    cr.rectangle(px, py - letter_y * 1.3, size_x, size_y)
    cr.stroke()

    cr.set_line_width(1.0)
    px += 5
    for (i, line) in enumerate(lines):
        cr.move_to(px, py)
        cr.show_text(line)
        py += letter_y

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

def draw_trace_box(cr, x, y, text):
    tw = 0
    th = 0
    for txt in text:
        tx, ty = utils.text_size(cr, txt, 8)
        th += ty
        tw = tw if tw > tx else tx
    rounded_rectangle(cr, x, y, tw + 40, th + len(text)*10, 10)
    cr.set_source_rgba(1.0, 0.5, 0.2, 0.9)
    cr.fill()
    rounded_rectangle(cr, x, y, tw + 40, th + 10, 10)
    cr.set_source_rgba(1.0, 0.5, 0.2)
    cr.stroke()

    cr.set_source_rgb(1, 1, 1)
    th = -5
    for txt in text:
        tx, ty = utils.text_size(cr, txt, 8)
        th += ty + 10
        cr.move_to(x + 30, y + th)
        cr.show_text(txt)
    cr.fill()

    cr.arc(x + 12, y + 7, 4, 0, 2 * math.pi)
    cr.move_to(x + 16, y + 11)
    cr.rel_line_to(5, 5)
    cr.stroke()
