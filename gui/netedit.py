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

import cconfig
import drawing
import citems
import net
import utils
import tracing

class NetEditCanvasConfig(cconfig.NetCanvasConfig):

    def __init__(self, neteditor):
        cconfig.NetCanvasConfig.__init__(self)
        self.neteditor = neteditor

    def configure(self):
        cconfig.NetCanvasConfig.configure(self)
        self.neteditor.set_entry_types([])

    def collect_items(self):
        items = cconfig.NetCanvasConfig.collect_items(self)
        if self.net:
            for i in self.net.items:
                items += i.get_error_items()
        return items

    def configure_item(self, item):
        item.inactive = False

        if item.kind == "box":
            if item.owner.is_place():
                item.create_context_menu = contextmenu_place
            if item.owner.is_transition():
                item.create_context_menu = contextmenu_transition
        if item.owner.is_edge():
            item.create_context_menu = contextmenu_edge
        if item.owner.is_area():
            item.create_context_menu = contextmenu_delete


class SelectionCanvasConfig(NetEditCanvasConfig):

    def on_items_selected(self):
        if self.selected_items:
            owners = [ i.owner for i in self.selected_items if i.owner ]
        else:
            owners = []
        if len(set(owners)) == 1:
            self.neteditor.set_entry_types(owners[0].get_text_entries())
        else:
            self.neteditor.set_entry_types([])

    def configure(self):
        NetEditCanvasConfig.configure(self)
        self.on_items_selected()


class NewElementCanvasConfig(NetEditCanvasConfig):

    selection = False
    mouse_position = None

    def configure(self):
        NetEditCanvasConfig.configure(self)

    def get_position(self, position):
        return utils.interpolate(
            position,
            utils.vector_add(position, self.element.size),
            -0.5)

    def draw(self, cr):
        NetEditCanvasConfig.draw(self, cr)
        if self.mouse_position:
            position = self.get_position(self.mouse_position)
            placement = citems.AbsPlacement(position)
            box = citems.ElementBox(placement,
                                    self.element.size,
                                    self.element.radius)
            box.draw(cr)

    def on_mouse_move(self, event, position):
        self.mouse_position = position
        self.canvas.redraw()

    def on_mouse_leave(self, event):
        self.mouse_position = None
        self.canvas.redraw()


class NewTransitionCanvasConfig(NewElementCanvasConfig):

    element = net.Transition

    def on_mouse_left_down(self, event, position):
        item = self.net.add_transition(self.get_position(position))
        self.neteditor.set_tool("selection", set_button=True)
        self.canvas.config.select_item(item.box)


class NewPlaceCanvasConfig(NewElementCanvasConfig):

    element = net.Place

    def on_mouse_left_down(self, event, position):
        item = self.net.add_place(self.get_position(position))
        self.neteditor.set_tool("selection", set_button=True)
        self.canvas.config.select_item(item.box)


class NewEdgeCanvasConfig(NetEditCanvasConfig):

    points = None
    from_element = None
    mouse_position = None

    def configure_item(self, item):
        NetEditCanvasConfig.configure_item(self, item)
        item.inactive = not self.is_place_box(item) and not self.is_transition_box(item)

    def is_place_box(self, item):
        return item.kind == "box" and item.owner.is_place()

    def is_transition_box(self, item):
        return item.kind == "box" and item.owner.is_transition()

    def on_mouse_left_up(self, event, position):
        item = self.get_item_at_position(position)
        if item:
            t = self.is_transition_box(item)
            p = self.is_place_box(item)
        else:
            t = False
            p = False

        if self.from_element is None:
            if t:
                for i in self.items:
                    i.inactive = not self.is_place_box(i)
            if p:
                for i in self.items:
                    i.inactive = not self.is_transition_box(i)
            if t or p:
                self.from_element = item
                self.mouse_position = position
                self.points = []
                self.canvas.redraw()
        elif item:
            if not item.inactive:
                edge = self.net.add_edge(self.from_element.owner, item.owner, self.points)
                self.from_element = None
                self.points = None
                self.neteditor.set_tool("selection", set_button=True)
                self.canvas.config.select_item(edge.line)
        else:
             self.points.append(position)

    def on_mouse_right_down(self, event, position):
        if self.from_element:
            self.from_element = None
            self.points = None
            self.reset_inactives()
            self.canvas.redraw()
        else:
            NetEditCanvasConfig.on_mouse_right_down(self, event, position)

    def on_mouse_move(self, event, position):
        if self.from_element:
            self.mouse_position = position
            self.canvas.redraw()

    def draw(self, cr):
        NetEditCanvasConfig.draw(self, cr)
        if self.from_element:
            p = self.points[:]
            p.append(self.mouse_position)
            p.insert(0, self.from_element.get_border_point(p[0]))
            cr.set_source_rgb(0, 0, 0)
            drawing.draw_polyline_nice_corners(
                cr, p, 0.5, 12, False, True)


class NewAreaCanvasConfig(NetEditCanvasConfig):

    start_position = None
    mouse_position = None
    selection = False

    def configure_item(self, item):
        NetEditCanvasConfig.configure_item(self, item)
        item.inactive = True

    def on_mouse_left_down(self, event, position):
        self.start_position = position

    def on_mouse_right_down(self, event, position):
        if self.start_position:
            self.start_position = None
            self.canvas.redraw()
        else:
            NetEditCanvasConfig.on_mouse_right_down(self, event, position)

    def on_mouse_left_up(self, event, position):
        if self.start_position:
            x1, y1 = self.start_position
            x2, y2 = position

            p = (min(x1, x2), min(y1, y2))
            s = (abs(x2 - x1), abs(y2 - y1))

            area = self.net.add_area(p, s)
            self.start_position = None
            self.neteditor.set_tool("selection", set_button=True)
            self.canvas.config.select_item(area.area)


    def on_mouse_move(self, event, position):
        if self.start_position:
            self.mouse_position = position
            self.canvas.redraw()

    def draw(self, cr):
        NetEditCanvasConfig.draw(self, cr)
        if self.start_position:
            cr.set_source_rgba(1,1,1,0.5)
            px, py = self.start_position
            sx, sy = utils.vector_diff(self.mouse_position, self.start_position)
            cr.rectangle(px, py, sx, sy)
            cr.fill()
            cr.set_line_width(0.5)
            cr.set_source_rgb(0,0,0)
            cr.rectangle(px, py, sx, sy)
            cr.stroke()


def delete_item(config, item):
    #for item in self.selected_item.delete():
    #    self.netview.undolist.add(undoredo.RemoveAction(self.net, item))
    item.delete()
    config.select_item(None)

def contextmenu_place(config, item, position):
    place = item.owner

    def tracingfn_callback():
        result = tracing.tracefn_dialog(config.neteditor.app.window, "", "int")
        if result is not None:
            place.tracing.append(result)

    def callback(place, value, check):
            return lambda w: set_tracing(config, place, value, check)

    trace_menu = [ ("Add function",
                    lambda w: tracingfn_callback()) ]
    token_name = ("ca::token_name", "std::string")
    if token_name not in place.tracing:
        trace_menu.append(("Add function 'ca::token_name'",
                           lambda w: set_tracing(config, place, token_name, True)))

    trace_fns = place.tracing
    if trace_fns:
        trace_menu.append(("-", None))
        for name, t in trace_fns:
            trace_menu.append(("Remove tracing function '{0}'".format(name, t),
                               callback(place, (name, t), False)))

    return [
        ("Delete", lambda w: delete_item(config, place)),
        ("Edit init code",
            lambda w: config.neteditor.place_edit_callback(place)),
        ("Tracing", trace_menu)
    ]

def contextmenu_transition(config, item, position):
    transition = item.owner
    fire = "fire" in transition.tracing
    trace_menu = [("fire",
                   (fire, lambda w: set_tracing(config, transition, "fire", not fire)))]
    return [
        ("Delete", lambda w: delete_item(config, transition)),
        ("Edit code",
            lambda w: config.neteditor.transition_edit_callback(transition)),
        ("Tracing", trace_menu)
    ]

def contextmenu_edge(config, item, position):
    edge = item.owner
    menu = [ ("Delete", lambda w: delete_item(config, edge)) ]
    if item.kind == "point" is not None:
       menu.append(
            ("Remove point",
             lambda w: edge.remove_point(item)))
    else:
        menu.append(
            ("Add point", lambda w: edge.add_point(position)))
    menu += [
        ("Switch direction",
            lambda w: edge.switch_direction()),
        ("Bidirectional",
            lambda w: edge.toggle_bidirectional()) ]
    return menu

def contextmenu_delete(config, item, position):
    return [ ("Delete", lambda w: delete_item(config, item.owner)) ]

def set_tracing(config, obj, value, add):
    if add:
        obj.tracing.append(value)
    else:
        obj.tracing.remove(value)
    config.canvas.redraw()
