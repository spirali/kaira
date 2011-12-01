#
#    Copyright (C) 2010 Stanislav Bohm
#                  2011 Ondrej Garncarz
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

from net import Place, Transition, Edge
import utils
import gtkutils

## @brief The base class for editing operations over network
#
#  (e.g. creating new places and transitions).
class NetTool:

	## The running action over network.
    action = None
    
    ## The original position of scrolling.
    scroll_point = None
    
    ## The selected item from the network.
    selected_item = None
    
    ## The last position of the mouse.
    mouse_last_pos = None
    
    ## @var net The network we're working with.
    
    ## @var netview The network view we're working with.

    ## The constructor.
    #  @param netview What network view are we working with?
    def __init__(self, netview):
        self.netview = netview
        self.net = netview.get_net()

    ## Starts working with the network.
    def start(self):
        self.set_cursor(None)
        self.netview.set_entry_types([])

    ## Stops working with elements from the network.
    def stop(self):
        self.deselect_item()

    ## Sets a right cursor for a running action.
    #  @param action_name The name of the running action
    def set_cursor(self, action_name):
        self.netview.set_cursor(action_name)

    ## Sets a new working network.
    #  @param net The new network.
    def set_net(self, net):
        self.deselect_item()
        self.net = net

    ## Draws the object. (Actually does nothing.)
    def draw(self, cr):
        pass

    ## Sets an item to work with.
    #  @param item The item.
    def select_item(self, item):
        self.selected_item = item
        if item:
            self.netview.highlight(item)
            self.netview.set_entry_types(item.get_text_entries())
        else:
            self.netview.set_entry_types([])
            self.netview.highlight_off()

    ## Checks whether an item is selectable.
    #  @param item The item.
    #  @return Is the item selectable?
    def is_selectable(self, item):
        return True

    ## Highlights a (hovered) item.
    #  @param item The item.
    def mouseover_highlight(self, item):
        self.netview.mouseover_highlight(item)

    ## Unhighlights a (formerly hovered) item.
    #  @param item The item.
    def mouseover_highlight_off(self):
        self.netview.mouseover_highlight_off()

    ## Sets no item to work with.
    def deselect_item(self):
        self.select_item(None)

    ## Returns the grid size.
    #  @return The grid size.
    def get_grid_size(self):
        return self.netview.get_grid_size()

    ## Returns a list of couples consisting of selectable subnetworks and their
    #  set functions related to a given transition.
    #  @param transition The transition.
    #  @return The list of couples.
    def build_netlist_menu(self, transition):
        def call(net):
            return lambda w: transition.set_subnet(net)
        menu = [ (gtkutils.escape_menu_name(net.get_name()), call(net))
            for net in self.netview.project.get_nets_with_interface() ]
        return [ ("<None>", call(None)) ] + menu

    ## @brief The event handler for the mouse right button pressed down.
    #
    #  Shows a context menu for a position or scrolls when there's nothing
    #  to work with.
    #  @param event The mouse event.
    #  @param position The position in the network.
    def right_button_down(self, event, position):
        def delete_event(w):
            self.selected_item.delete()
            self.deselect_item()

        if self.selected_item in self.net.pick_items(position):

            actions_dict = {
                Transition: [("Edit code",
                    lambda w: self.netview.transition_edit_callback(self.selected_item)),
                    ("Set subnet", self.build_netlist_menu(self.selected_item))
                ],
                Place: [("Edit init code",
                    lambda w: self.netview.place_edit_callback(self.selected_item))],
                Edge: [ ("Switch direction",
                            lambda w: self.selected_item.switch_direction()),
                        ("Bidirectional",
                            lambda w: self.selected_item.toggle_bidirectional()) ]
            }

            if self.selected_item.is_interfacebox():
                menu_actions = [] # interface box cannot be deleted
            else:
                menu_actions = [("Delete", delete_event)]

            if type(self.selected_item) in actions_dict:
                menu_actions = actions_dict[type(self.selected_item)] + menu_actions

            if menu_actions:
                gtkutils.show_context_menu(menu_actions, event)
        else:
            self.scroll_point = (event.x, event.y)
            self.set_cursor("scroll")

    ## @brief The event handler for the mouse right button released.
    #
    #  Stops scrolling if it was.
    #  @param event A mouse event.
    #  @param position A position in the network.
    def right_button_up(self, event, position):
        if self.scroll_point is not None:
            self.scroll_point = None
            self.set_cursor(None)

    ## @brief The event handler for the network being changed.
    #
    #  Stops working with an item which isn't contained in the network.
    def net_changed(self):
        if self.selected_item and not self.net.contains(self.selected_item):
            self.selected_item = None
            self.set_cursor(None)

    ## Returns a item at a given position.
    #  @param position The position in the network.
    #  @return The item.
    def item_at_position(self, position):
        return self.net.get_item_at_position(position, self.is_selectable)

    ## @brief The event handler for the mouse left button pressed down.
    #
    #  Selects an item at a given position and changes the running action
    #  appropriately.
    #  @param event A mouse event.
    #  @param position The position in the network.
    #  @return Has any item been selected?
    def left_button_down(self, event, position):
        item = self.item_at_position(position)
        if item:
            self.select_item(item)
            self.action = item.get_action(position, self)
            if self.action:
                self.action.set_cursor()
            else:
                self.set_cursor(None)
            return True
        return False

    ## @brief The event handler for the mouse left button released.
    #
    #  Deactivates the running action.
    #  @param event A mouse event.
    #  @param position The position in the network.
    def left_button_up(self, event, position):
        self.action = None

    ## @brief The event handler for the mouse being moved.
    #
    #  @li Scrolls, if scrolling.
    #  @li Highlights a hovered item.
    #  @li Delegates to the running action or offers an appropriate action
    #    for the selected item.
    #  @param event A mouse event.
    #  @param position The position in the network.
    def mouse_move(self, event, position):

        if self.scroll_point:
            p = (event.x, event.y)
            diff = utils.vector_diff(self.scroll_point, p)
            viewport = self.netview.get_viewport()
            self.netview.set_viewport(utils.vector_diff(viewport,diff))
            self.scroll_point = p
            return

        item = self.item_at_position(position)
        if item is None:
            self.mouseover_highlight_off()
        else:
            self.mouseover_highlight(item)

        if self.selected_item:
            if self.action:
                self.action.mouse_move(position)
            else:
                action = self.selected_item.get_action(position, self)
                if action:
                    action.set_cursor()
                else:
                    self.set_cursor(None)
        self.mouse_last_pos = position

    ## Returns an action ... # TODO ???
    #
    #  @param original_position The original positon in the network.
    #  @param set_fn # TODO co to je?
    #  @param position The new position in the network.
    #  @return The tool.
    def get_move_action(self, original_position, set_fn, position):
        tool = ToolActionGridMove(original_position, set_fn, position, self, "move")
        return tool

    def get_resize_action(self, item, position, set_fn):
        original = utils.vector_diff(position, item.position)
        return ToolActionGridMove(original, set_fn, position, self, "resize_rbottom")

    def get_custom_move_action(self, position, fn, cursor):
        return ToolActionCustomMove(fn, position, self, cursor)

    def get_empty_action(self):
        return ToolActionEmpty(self)

class SelectTool(NetTool):
    pass

class NetItemTool(NetTool):

    def left_button_down(self, event, position):
        if not NetTool.left_button_down(self, event, position):
            self.select_item(self.create_new(position))
            self.action = self.selected_item.get_action(position, self)
            self.action.set_cursor()

class PlaceTool(NetItemTool):

    def create_new(self, position):
        return self.net.add_place(position)

class TransitionTool(NetItemTool):

    def create_new(self, position):
        return self.net.add_transition(position)

class EdgeTool(NetTool):

    from_item = None
    points = []

    def is_selectable(self, item):
        return item.is_edge()

    def left_button_up(self, event, position):
        if self.action:
            self.action = None
        else:
            if self.from_item:
                if self.from_item.is_place():
                    item = self.net.get_item_at_position(position)
                    if item:
                        if item.is_interfacebox():
                            item = self.net.add_interface_node(position)
                        elif not (item.is_inode() or item.is_transition()):
                            item = None
                else: # self.from_item is transition or inode
                    item = self.net.get_item_at_position(position, lambda i: i.is_place())
                if item:
                    edge = self.net.add_edge(self.from_item, item, self.points)
                    self.select_item(edge)
                    self.from_item = None
                else:
                    self.points.append(position)
            else:
                item = self.net.get_item_at_position(position)
                if item:
                    if item.is_transition() or item.is_place() or item.is_inode():
                        self.from_item = item
                    elif item.is_interfacebox():
                        self.from_item = self.net.add_interface_node(position)
                    self.last_position = position
                    self.points = []

    def right_button_down(self, event, position):
        self.from_item = None
        self.action = None
        NetTool.right_button_down(self, event, position)
        self.netview.redraw()

    def mouse_move(self, event, position):
        NetTool.mouse_move(self, event, position)
        if self.from_item:
            self.netview.redraw()

    def draw(self, cr):
        if self.from_item:
            cr.set_line_width(1.5)
            cr.set_source_rgb(0.0,0.0,0.0)
            if self.points:
                pp = self.points[0]
            else:
                pp = self.mouse_last_pos
            pos = self.from_item.get_border_point(pp)
            utils.draw_polyline_nice_corners(cr, [pos] + self.points + [self.mouse_last_pos], 0.5, 12, False, True)

class AreaTool(NetTool):

    point1 = None

    def left_button_down(self, event, position):
        if not NetTool.left_button_down(self, event, position):
            self.point1 = position

    def mouse_move(self, event, position):
        NetTool.mouse_move(self, event, position)
        if self.point1:
            self.netview.redraw()

    def left_button_up(self, event, position):
        if self.action:
            self.action = None
            self.set_cursor(None)
        elif self.point1:
            p, s = utils.position_and_size_from_points(self.point1, self.mouse_last_pos)
            if s[0] > 5 and s[1] > 5:
                area = self.net.add_area(p,s)
                self.select_item(area)
            else:
                self.netview.redraw()
            self.point1 = None

    def draw(self, cr):
        if self.point1:
            cr.set_source_rgba(1,1,1,0.5)
            self._rect(cr)
            cr.fill()
            cr.set_line_width(0.5)
            cr.set_source_rgb(0,0,0)
            self._rect(cr)
            cr.stroke()

    def _rect(self, cr):
        p, s = utils.position_and_size_from_points(self.point1, self.mouse_last_pos)
        cr.rectangle(p[0], p[1], s[0], s[1])


class ToolAction:

    def __init__(self, position, tool, cursor):
        self.start_position = position
        self.tool = tool
        self.cursor = cursor

    def get_rel_change(self, position):
        return utils.vector_diff(position, self.start_position)

    def set_cursor(self):
        self.tool.set_cursor(self.cursor)

class ToolActionGridMove(ToolAction):

    def __init__(self, original_position, set_fn, position, tool, cursor):
        ToolAction.__init__(self, position, tool, cursor)
        self.set_fn = set_fn
        self.original_position = original_position

    def mouse_move(self, position):
        pos = utils.vector_add(self.original_position, self.get_rel_change(position))
        pos = utils.snap_to_grid(pos, self.tool.get_grid_size())
        self.set_fn(pos)

class ToolActionCustomMove(ToolAction):

    def __init__(self, fn, position, tool, cursor):
        ToolAction.__init__(self, position, tool, cursor)
        self.fn = fn

    def mouse_move(self, position):
        self.fn(self.get_rel_change(position))

class ToolActionEmpty:

    def __init__(self, tool):
        self.tool = tool

    def set_cursor(self):
        self.tool.set_cursor(None)

    def mouse_move(self, position):
        pass
