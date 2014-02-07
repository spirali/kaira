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

import utils
import gtk
import gtkutils
import citems

def set_items_highlight(items, color):
    for item in items:
        item.highlight = color

def get_cursor(action):
    if action == "move":
       return gtk.gdk.Cursor(gtk.gdk.FLEUR)
    if action == "scroll":
       return gtk.gdk.Cursor(gtk.gdk.HAND2)
    if action == "resize_rbottom":
        return gtk.gdk.Cursor(gtk.gdk.BOTTOM_RIGHT_CORNER)
    if action == "resize_lbottom":
        return gtk.gdk.Cursor(gtk.gdk.BOTTOM_LEFT_CORNER)
    if action == "resize_ltop":
        return gtk.gdk.Cursor(gtk.gdk.TOP_LEFT_CORNER)
    if action == "resize_rtop":
        return gtk.gdk.Cursor(gtk.gdk.TOP_RIGHT_CORNER)
    else:
        return None


class CanvasConfig:

    canvas = None
    mouseover_highligt_color = (0.6,0.6,0.8,8.0)
    select_color = (0.86,0.86,0.0,1.0)

    selection = True
    grid_size = 1

    def __init__(self):
        self.items = []
        self.mouseover_items = None
        self.selected_items = None
        self.drag_mouse_origin = None
        self.drag_items_origin = None
        self.drag_items = None
        self.scroll_point = None
        self.mass_selection = False
        self.mass_selection_origin = None
        self.mass_selection_cursor = None

    def on_items_selected(self):
        pass

    def set_highlight(self):
        self.reset_highlight()

        if not self.selection:
            return

        if self.mouseover_items:
            set_items_highlight(self.mouseover_items,
                                self.mouseover_highligt_color)

        if self.selected_items:
            set_items_highlight(self.selected_items,
                                self.select_color)
        self.canvas.redraw()

    def draw(self, cr):
        for item in reversed(self.items):
            item.draw(cr)
        if self.mass_selection_origin:
            cr.new_sub_path()
            cr.set_source_rgba(1, 1, 1, 0.5)
            cr.rectangle(self.mass_selection_origin[0],
                         self.mass_selection_origin[1],
                         self.mass_selection_cursor[0] - self.mass_selection_origin[0],
                         self.mass_selection_cursor[1] - self.mass_selection_origin[1])
            cr.fill()

    def get_bounding_box(self):
        boxes = [ item.get_bounding_box() for item in self.items if item is not None ]

        if not boxes:
            return ((0,0), (0,0))

        box = boxes[0]

        for b in boxes[1:]:
            box = utils.merge_bounding_boxes(box, b)
        return box

    def get_default_viewport(self):
        a, b = self.get_bounding_box()
        return utils.middle_point(a, b)

    def reset_highlight(self):
        for item in self.items:
            item.highlight = None

    def get_item_at_position(self, position):
        for item in self.items:
            if item.is_at_position(position):
                return item

    def get_items_in_rect(self, rect_position, rect_size):
        return [ item for item in self.items if item.is_in_rect(rect_position, rect_size) ]

    def get_items_in_mass_selection(self):
        position, size = utils.make_rect(self.mass_selection_origin, self.mass_selection_cursor)
        items = []
        for item in self.get_items_in_rect(position, size):
            items += item.get_group()
        return items

    def on_mouse_move(self, event, position):
        if self.scroll_point:
            change = utils.make_vector(position, self.scroll_point)
            self.canvas.set_viewport(
                utils.vector_add(self.canvas.get_viewport(), change))
            return

        if self.mass_selection_origin:
            self.mass_selection_cursor = position
            self.mouseover_items = self.get_items_in_mass_selection()
            self.set_highlight()
            return

        if self.drag_items:
            change = utils.make_vector(self.drag_mouse_origin, position)
            for i, item in enumerate(self.drag_items):
                new_position = utils.vector_add(self.drag_items_origin[i], change)
                new_position = utils.snap_to_grid(new_position, self.grid_size)
                item.set_position(new_position)
            self.canvas.redraw()
            return

        item = self.get_item_at_position(position)
        if item:
            self.canvas.set_cursor(get_cursor(item.action))
            if self.selection:
                self.mouseover_items = item.get_group()
                self.set_highlight()
        elif self.mouseover_items:
            self.canvas.set_cursor(None)
            self.mouseover_items = []
            self.set_highlight()
        else:
            self.canvas.set_cursor(None)

    def on_mouse_left_down(self, event, position):
        item = self.get_item_at_position(position)
        if item:
            if self.mass_selection and item in self.selected_items:
                # dirty hack
                self.drag_items = [ i for i in self.selected_items if i.kind == "box" or i.kind == "point" ]
                self.drag_items_origin = self.drag_items_origin = [ i.get_position()
                                                                    for i in self.drag_items ]
                self.drag_mouse_origin = position
            elif item.action is not None:
                self.drag_items = [item]
                self.drag_items_origin = [ i.get_position()
                                               for i in self.drag_items ]
                self.drag_mouse_origin = position
            else: # Click on non-selected item
                pass
        else:
            self.select_item(None)
            self.mass_selection_origin = position
            self.mass_selection_cursor = position
        self.set_highlight()

    def select_item(self, item):
        if item is None:
            self.selected_items = ()
            self.canvas.set_cursor(None)
        else:
            if item.delegate_selection:
                self.select_item(item.delegate_selection)
                return
            self.selected_items = item.get_group()[:]
            if item.action:
                self.canvas.set_cursor(get_cursor(item.action))
        self.on_items_selected()
        self.set_highlight()

    def mass_select_items(self, items):
        self.selected_items = items
        self.on_items_selected()
        self.set_highlight()

    def on_mouse_left_up(self, event, position):
        if self.mass_selection_origin:
            items = self.get_items_in_mass_selection()
            self.mass_select_items(items)
            self.mass_selection_origin = None
            self.mass_selection_cursor = None
            self.mass_selection = bool(items)
            self.canvas.redraw()
            return
        item = self.get_item_at_position(position)
        if item:
            self.on_item_click(item, position)

        if self.drag_items and self.drag_mouse_origin != position:
            for i, p in zip(self.drag_items, self.drag_items_origin):
                self.on_item_move(i, p)

        self.drag_items_origin = None
        self.drag_items = None
        self.drag_mouse_origin = None

        if not self.selection:
            return

        if item and (not self.selected_items or item not in self.selected_items):
            self.select_item(item)

    def on_mouse_right_down(self, event, position):
        item = self.get_item_at_position(position)

        if item:
            self.select_item(item)
            if item.create_context_menu:
                menu = item.create_context_menu(self, item, position)
                if menu:
                    gtkutils.show_context_menu(menu, event)
            return

        self.scroll_point = position
        self.canvas.set_cursor(get_cursor("scroll"))

    def on_mouse_right_up(self, event, position):
        self.scroll_point = None
        self.canvas.set_cursor(None)
        self.on_mouse_move(event, position)

    def on_mouse_leave(self, event):
        pass

    def on_item_click(self, item, position):
        pass

    def on_item_move(self, item, original_position):
        pass


class NetCanvasConfig(CanvasConfig):

    net = None

    def set_net(self, net, viewport=None):
        if net != self.net:
            self.net = net
            if viewport is None:
                self.canvas.reset_viewport()
            else:
                self.canvas.set_viewport(viewport)
            self.configure()

    def configure(self):
        self.items = self.collect_items()
        self.items.sort(key=lambda i: i.z_level, reverse=True)
        self.set_highlight()

    def get_view_mode(self):
        return None

    def collect_items(self):
        canvas_items = []
        view_mode = self.get_view_mode()
        if self.net is not None:
            for item in self.net.items:
                i = item.get_canvas_items(view_mode)
                citems.make_group(i)
                canvas_items += i
            for item in canvas_items:
                self.configure_item(item)
        return canvas_items

    def configure_item(self, item):
        pass

    def get_items_in_mass_selection(self):
        items = CanvasConfig.get_items_in_mass_selection(self)
        owners = [ item.owner for item in items ]
        for item in items[:]:
            if item.kind == "box":
                for edge in item.owner.edges_from():
                    if edge.to_item in owners:
                        items += edge.get_canvas_items(None)
        return items
