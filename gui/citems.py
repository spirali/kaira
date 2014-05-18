#
#    Copyright (C) 2013 Stanislav Bohm
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
import drawing
import utils
import math


class AbsPlacement:

    def __init__(self, position):
        self.position = position

    def get_position(self):
        return self.position

    def set_position(self, value):
        self.position = value


class RelativePlacement:

    def __init__(self, parent_placement, position):
        self.parent_placement = parent_placement
        self.set_position(position)

    def get_position(self):
        return utils.vector_add(self.parent_placement.get_position(), self.position)

    def set_position(self, position):
        self.position = utils.vector_diff(position,
                                          self.parent_placement.get_position())


class MultilineRelativePlacement:

    # multiline - object with method getPoints()
    def __init__(self, multiline, position):
        self.multiline = multiline
        self.set_position(position)

    def compute_point_on_multiline(self, points):
        if self.point_index < len(points) - 1:
            return utils.interpolate(points[self.point_index],
                                    points[self.point_index + 1],
                                    self.line_param)
        else:
            return points[-1]


    def set_position(self, position):
        points = self.multiline.get_points()
        if position is None:
            self.point_index = (len(points) - 1) / 2
            self.line_param = 0.5
            self.offset = (0.0, 0.0)
        else:
            self.point_index, self.line_param = \
                utils.nearest_point_of_multiline(points, position)
            self.offset = utils.make_vector(self.compute_point_on_multiline(points),
                                            position)

    def get_position(self):
        points = self.multiline.get_points()
        return utils.vector_add(self.compute_point_on_multiline(points),
                                self.offset)


class CanvasItem:
    z_level = 0
    highlight = None
    action = None
    owner = None
    group = None
    inactive = False
    placement = None
    create_context_menu = None
    delegate_selection = None

    def __init__(self, owner, kind, placement):
        self.owner = owner
        self.kind = kind
        self.placement = placement

    def get_group(self):
        if self.group is None:
            return [self]
        else:
            return self.group

    def is_at_position(self, position):
        return False

    def is_in_rect(self, rect_position, rect_size):
        return False

    def draw(self, cr):
        pass

    def get_bounding_box(self):
        return None

    def get_position(self):
        return self.placement.get_position()

    def set_position(self, position):
        self.placement.set_position(position)

    def get_relative_placement(self, position, absolute=True):
        if not absolute:
            position = utils.vector_add(position, self.placement.get_position())
        return RelativePlacement(self.placement, position)


class ElementBox(CanvasItem):

    text = ""
    action = "move"
    name = ""
    corner_text = ""

    doubleborder = False
    thicklines = False

    def __init__(self, owner, kind, placement, size, radius):
        CanvasItem.__init__(self, owner, kind, placement)
        self.size = size
        self.radius = radius

    def draw(self, cr):
        px, py = self.get_position()
        sx, sy = self.size
        drawing.draw_round_rectangle(cr, px, py, sx, sy, self.radius)

        if self.inactive:
            color = (0.5, 0.5, 0.5)
        else:
            color = (0, 0, 0)

        cr.set_source_rgb(1, 1, 1)
        cr.fill()

        if self.highlight:
            drawing.draw_round_rectangle(cr, px, py, sx, sy, self.radius)
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            cr.stroke()

        drawing.draw_round_rectangle(cr, px, py, sx, sy, self.radius)
        cr.set_line_width(1.2)
        cr.set_source_rgb(*color)
        cr.stroke()

        if self.thicklines:
            cr.rectangle(px, py, 8, sy)
            cr.rectangle(px + sx - 8, py, 8, sy)
            cr.fill()
        elif self.doubleborder:
            if self.radius > 3:
                drawing.draw_round_rectangle(cr, px, py, sx, sy, self.radius - 3.5)
            else:
                drawing.draw_round_rectangle(cr, px + 3, py + 3, sx - 6, sy - 6, self.radius)
            cr.stroke()
        drawing.draw_centered_text(cr, px + sx / 2, py + sy / 2, self.name)
        if self.corner_text:
            if self.thicklines:
                shift_x = 10
                shift_y = 3
            elif self.doubleborder:
                shift_x = 6
                shift_y = 6
            else:
                shift_x = 3
                shift_y = 3
            cr.save()
            cr.set_font_size(8)
            drawing.draw_text(cr, px + sx - shift_x, py + shift_y, self.corner_text, 1, 1)
            cr.restore()

    def is_at_position(self, position):
        return utils.is_in_round_rectangle(
            self.get_position(), self.size, self.radius, position, 10)

    def is_in_rect(self, rect_position, rect_size):
        return utils.is_round_rectangle_in_rect(
            self.get_position(), self.size, self.radius, rect_position, rect_size)

    def get_border_point(self, outer_point):
        e = 0.01
        r = self.radius
        px, py = self.get_position()
        ox, oy = outer_point
        sx, sy = self.size
        cx, cy = px + sx/2, py + sy/2
        ux, uy = cx - ox, cy - oy
        t = []
        v = utils.line_intersec_get_t((ox, oy), (ux, uy), (px-e, py-r), (sx+e, 0.0))
        if v is not None:
            t.append(v)
        v = utils.line_intersec_get_t((ox, oy), (ux, uy), (px-e, py+sy+r), (sx+e, 0.0))
        if v is not None:
            t.append(v)
        v = utils.line_intersec_get_t((ox, oy), (ux, uy), (px-r, py-e), (0.0, sy+e))
        if v is not None:
            t.append(v)
        v = utils.line_intersec_get_t((ox, oy), (ux, uy), (px+sx+r, py-e), (0.0, sy+e))
        if v is not None:
            t.append(v)
        v = utils.circle_collision((ox, oy), (ux, uy), (px, py), r)
        if v is not None and v[2] is not None:
            t.append(v[2])
        v = utils.circle_collision((ox, oy), (ux, uy), (px+sx, py), r)
        if v is not None and v[2] is not None:
            t.append(v[2])
        v = utils.circle_collision((ox, oy), (ux, uy), (px,    py+sy), r)
        if v is not None and v[2] is not None:
            t.append(v[2])
        v = utils.circle_collision((ox, oy), (ux, uy), (px+sx, py+sy), r)
        if v is not None and v[2] is not None:
            t.append(v[2])
        if t:
            p = min(t)
        else:
            p = 0
        return (ox + ux*p, oy + uy*p)

    def get_bounding_box(self):
        return (self.get_position(), utils.vector_add(self.get_position(), self.size))


class Label(CanvasItem):

    text = None
    text_fn = None
    z_level = 5
    color = (1,1,1)
    background_color = (0,0,0)
    action = "move"
    size = None

    def draw(self, cr):
        text = None
        if self.text:
            text = self.text
        elif self.text_fn:
            text = self.text_fn()
        if text:
            px, py = self.get_position()
            if not self.highlight:
                self.size = drawing.draw_label(
                        cr, px, py, text, self.symbol,
                        self.color, self.background_color)
            else:
                self.size = drawing.draw_label(
                        cr, px, py, text, self.symbol,
                        (0,0,0), self.highlight)

    def is_at_position(self, position):
        if not self.size:
            return False
        px, py = position
        x, y = self.get_position()
        return (x < px and y < py and
                x + self.size[0] > px and y + self.size[1] > py)


class TraceLabel(Label):
    background_color = (1.0, 0.5, 0.2)
    symbol = "lookingglass"


class SimRunLabel(Label):
    background_color = (0.2, 0.5, 1.0)
    symbol = "arrow"


class VerifLabel(Label):
    background_color = (0.7, 0, 0.8)
    symbol = "tick"


class ArrowLine(CanvasItem):

    bidirectional = False
    z_level = 2

    def __init__(self, owner, kind, get_points):
        CanvasItem.__init__(self, owner, kind, None)
        self.get_points = get_points

    def draw(self, cr):
       points = self.get_points()
       if self.highlight:
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            drawing.draw_polyline_nice_corners(cr, points, 0.5, 12, self.bidirectional, True)

       cr.set_line_width(1.5)
       if self.inactive:
           cr.set_source_rgb(0.5,0.5,0.5)
       else:
           cr.set_source_rgb(0.0,0.0,0.0)
       drawing.draw_polyline_nice_corners(cr, points, 0.5, 12, self.bidirectional, True)

    def is_at_position(self, position):
        for a, b in utils.pairs_generator(self.get_points()):
            if utils.is_near_line_segment(a, b, position, 5):
                return True
        return False

    def get_relative_placement(self, position):
        return MultilineRelativePlacement(self, position)


class Point(CanvasItem):

    z_level = 10
    radius = 10
    action = "move"
    color = None

    def is_at_position(self, position):
        return utils.point_distance(position, self.get_position()) <= self.radius

    def get_bounding_box(self):
        return (self.get_position(), self.get_position())

    def draw(self, cr):
        if self.color:
            px, py = self.get_position()
            cr.set_source_rgb(*self.color)
            cr.new_sub_path()
            cr.arc(px, py, self.radius, 0, 2 * math.pi)
            cr.fill()


class Text(CanvasItem):

    z_level = 3
    action = "move"
    size = (0, 0)
    background_color = None
    border_color = None
    padding_left = 6
    padding_right = 6
    padding_top = 2
    padding_bottom = 2
    align_x = 0
    align_y = 1
    radius = None
    color = (0.0, 0.0, 0.0)
    format = None

    def __init__(self, owner, kind, placement, text=""):
        CanvasItem.__init__(self, owner, kind, placement)
        self.text = text

    def is_at_position(self, position):
        px, py = position
        x, y = self.get_position()
        x -= self.align_x * self.size[0]
        return (x < px and y < py and
                x + self.size[0] > px and y + self.size[1] > py)

    def draw(self, cr):
        if self.text:
            px, py = self.get_position()

            if self.inactive:
               cr.set_source_rgb(0.5,0.5,0.5)
            else:
               cr.set_source_rgb(*self.color)

            if self.highlight:
                background_color = self.highlight
            else:
                background_color = self.background_color

            if self.format:
                text = self.format.format(self.text)
            else:
                text = self.text

            self.size = drawing.draw_text(
                   cr, px, py, text, self.align_x, self.align_y,
                   padding_left=self.padding_left,
                   padding_right=self.padding_right,
                   padding_top=self.padding_top,
                   padding_bottom=self.padding_bottom,
                   radius=self.radius,
                   background_color=background_color,
                   border_color=self.border_color)
        else:
            self.size = (0, 0)

    def get_bounding_box(self):
        return (self.get_position(), self.get_position())


class Area(CanvasItem):

    z_level = -1

    def __init__(self, owner, kind, point1, point2):
        CanvasItem.__init__(self, owner, kind, None)
        self.point1 = point1
        self.point2 = point2

    def draw(self, cr):
        x1, y1 = self.point1.get_position()
        x2, y2 = self.point2.get_position()
        sx = x2 - x1
        sy = y2 - y1

        cr.set_source_rgb(0.6,0.7,0.9)
        cr.rectangle(x1, y1, sx, sy)
        cr.fill()

        if self.highlight:
            cr.set_line_width(6.5)
            cr.set_source_rgba(*self.highlight)
            cr.rectangle(x1, y1, sx, sy)
            cr.stroke()

        cr.set_line_width(0.5)
        if self.inactive:
            cr.set_source_rgb(0.5,0.5,0.5)
        else:
            cr.set_source_rgb(0,0,0)
        cr.rectangle(x1, y1, sx, sy)
        cr.stroke()

    def is_at_position(self, position):
        p1 = self.point1.get_position()
        p2 = self.point2.get_position()
        return utils.position_on_rect(position, p1, utils.vector_diff(p2, p1), 5)


class TokenBox(CanvasItem):

    z_level = 15

    max_shown_tokens = 10
    action = "move"

    def __init__(self, owner, kind, placement):
        CanvasItem.__init__(self, owner, kind, placement)
        self.tokens = []
        self.new_tokens = []
        self.removed_tokens = []
        self.tokens_count = 0
        self.size = None
        self.visual_position = None

    def set_tokens(self, tokens, new_tokens, removed_tokens):
        self.tokens_count = len(tokens) + len(new_tokens)
        t = utils.collapse_line_repetitions(tokens)
        if len(t) > self.max_shown_tokens:
            self.tokens = map(shorten_token_name, t[:self.max_shown_tokens])
            self.tokens.append("...")
        else:
            self.tokens = map(shorten_token_name, t)

        self.new_tokens = map(shorten_token_name,
                              utils.collapse_line_repetitions(new_tokens))
        self.removed_tokens = map(shorten_token_name,
                                  utils.collapse_line_repetitions(removed_tokens))

    def is_at_position(self, position):
        if self.visual_position:
            return utils.position_inside_rect(position, self.visual_position, self.size)
        else:
            return False

    def draw(self, cr):
        w_size = utils.text_size(cr, "W")[1] + 6

        if self.tokens_count == 0:
            return

        px, py = self.get_position()

        text_width = 0
        all =  self.removed_tokens + self.tokens + self.new_tokens
        if all:
            text_width = max(utils.text_size(cr, t)[0] for t in all) + 5

        size_y = len(all) * w_size
        self.size = (text_width, size_y)
        self.visual_position = (px + 10, py - size_y / 2)
        top = py - size_y / 2 + 2
        y = top
        if self.removed_tokens:
            cr.set_source_rgba(0.2, 0.2, 0.2, 0.7)
            cr.rectangle(px + 10, y + 4, text_width + 6, w_size * len(self.removed_tokens))
            cr.fill()
            y += w_size * len(self.removed_tokens)

        if self.tokens:
            cr.set_source_rgba(0.2, 0.45, 0, 0.7)
            cr.rectangle(px + 10, y + 4, text_width + 6, w_size * len(self.tokens))
            cr.fill()
            y += w_size * len(self.tokens)

        if self.new_tokens:
            cr.set_source_rgba(0.2,0.7,0,0.7)
            cr.rectangle(px + 10, y + 4, text_width + 6, w_size * len(self.new_tokens))
            cr.fill()
            y += w_size * len(self.new_tokens)

        y = top
        cr.set_source_rgb(1.0,1.0,1.0)
        for t in all:
            y += w_size
            cr.move_to(px + 15, y)
            cr.show_text(t)

        if self.new_tokens:
            cr.set_source_rgb(0.2,0.6,0)
        else:
            cr.set_source_rgb(0.2,0.45,0)


        cr.new_sub_path()
        cr.arc(px, py, 8, 0, 2 * math.pi)
        cr.fill()

        cr.set_line_width(0.5)
        cr.new_sub_path()
        cr.arc(px, py, 8, 0, 2 * math.pi)
        cr.set_source_rgb(0,0,0)
        cr.stroke()

        drawing.draw_centered_text(cr, px, py, str(self.tokens_count))


class TransitionActivation(CanvasItem):

    z_level = 16
    size = (34, 14)

    def __init__(self, owner, kind, placement, text, color):
        CanvasItem.__init__(self, owner, kind, placement)
        self.text = text
        self.color = color

    def draw(self, cr):
        x, y = self.get_position()
        sx, sy = self.size
        cr.move_to(x - 2, y + sy/2)
        cr.rel_line_to(sx, 0)
        cr.rel_line_to(-4, -sy/2)
        cr.rel_line_to(4, -sy/2)
        cr.rel_line_to(-sx, 0)
        cr.rel_line_to(-4, sy/2)
        cr.rel_line_to(4, sy/2)
        cr.set_source_rgba(*self.color)
        cr.fill()
        cr.set_source_rgb(0, 0, 0)
        drawing.draw_centered_text(cr, x + sx/2-4, y, self.text)

    def is_at_position(self, position):
        return utils.position_inside_rect(position, self.get_position(), self.size, 3)


class TransitionActivations(Point):

    z_level = 16
    space_x = 3
    space_y = 3

    def __init__(self, owner, kind, placement):
        Point.__init__(self, owner, kind, placement)

    def is_at_position(self, position):
        return Point.is_at_position(self, position)

    def create_activations(self, values):
        results = []
        position = self.get_position()
        start = utils.vector_add(position, (12, 0))
        position = start
        count = 0
        for text, color, data in values:
            activation = TransitionActivation(data,
                                              "activation",
                                              self.get_relative_placement(position),
                                              text,
                                              color)
            results.append(activation)
            position = utils.vector_add(position, (self.space_x + activation.size[0], 0))
            count += 1
            if count == 6:
                count = 0
                start = utils.vector_add(start, (0, self.space_y + activation.size[1]))
                position = start
        return results

    def draw(self, cr):
        px, py = self.get_position()

        w_size = utils.text_size(cr, "W")[1]
        cr.set_source_rgba(0.4, 0.4, 0.4,0.8)
        cr.move_to(px - 3, py - w_size)
        cr.rel_line_to(8, 0)
        cr.rel_line_to(-3, w_size)
        cr.rel_line_to(3, w_size)
        cr.rel_line_to(-8, 0)
        cr.rel_line_to(0, -2 * w_size)
        cr.fill()
        """
        x = px + 10
        y = py
        count = 0
        for text, color in self.values:
            tx, ty = utils.text_size(cr, text)
            cr.move_to(x - 2, y + w_size)
            cr.rel_line_to(tx + 10, 0)
            cr.rel_line_to(-4, -w_size)
            cr.rel_line_to(4, -w_size)
            cr.rel_line_to(-tx - 10, 0)
            cr.rel_line_to(-4, w_size)
            cr.rel_line_to(4, w_size)
            cr.set_source_rgba(*color)
            cr.fill()
            cr.move_to(x, y + w_size/2)
            x += tx + 12
            cr.set_source_rgb(0, 0, 0)
            cr.show_text(text)
            count += 1
            if count == 6:
                count = 0
                y += 2 * w_size
                x = px + 10"""


class Box(Point):

    z_level = 14
    background = None
    size = None
    size_fn = None
    radius = None
    action = "move"

    def draw(self, cr):
        px, py = self.get_position()

        if self.size_fn:
            self.size = self.size_fn(self, cr)

        if self.size is None:
            return

        if self.background:
            cr.set_source_rgba(*self.background)
            if self.radius:
                drawing.rounded_rectangle(
                    cr, px, py, self.size[0], self.size[1], self.radius)
            else:
                cr.rectangle(px, py, self.size[0], self.size[1])
            cr.fill()

    def is_at_position(self, position):
        if self.size:
            return utils.position_inside_rect(position, self.get_position(), self.size)
        else:
            return False

class ClockIcon(Point):

    z_level = 1

    def draw(self, cr):
        px, py = self.get_position()
        cr.new_sub_path()
        cr.set_source_rgb(1,1,1)
        cr.arc(px, py, 6, 0, 2 * math.pi)
        cr.fill()
        cr.set_source_rgb(0,0,0)
        cr.set_line_width(1)
        cr.new_sub_path()
        cr.arc(px, py, 6, 0, 2 * math.pi)
        cr.move_to(px, py)
        cr.rel_line_to(0, -3.5)
        cr.move_to(px, py)
        cr.rel_line_to(3.7, 1.4)
        cr.stroke()


class PlaceInterface(Text):

    color = (0.2, 0.2, 0.2)
    background_color = (0.4, 0.4, 0.4, 0.2)
    align_x = 1

    interface_in = None
    interface_out = None

    def update(self):
        self.text = ""
        if self.interface_in is not None:
            self.text = self.interface_in + u" \u25B6\n"
        if self.interface_out is not None:
            self.text += self.interface_out + u" \u25C0\n"

    def is_visible(self):
        return self.interface_in is not None or self.interface_out is not None


def shorten_token_name(name):
    if len(name) > 25:
        return name[:18] + " ... ({0} chars)".format(len(name))
    else:
        return name

def make_group(items):
    group = list(items)
    for item in items:
        if item.group is not None:
            item.group.remove(item)
        item.group = group
