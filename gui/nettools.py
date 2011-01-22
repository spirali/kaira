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

from net import Place, Transition, Edge
import utils
import gtkutils

class NetTool:

	action = None
	action_start = None
	action_last_pos = None
	scroll_point = None
	selected_item = None

	def __init__(self, netview):
		self.netview = netview
		self.net = netview.net

	def start(self):
		self.set_cursor(None)
		self.netview.set_entry_types([])

	def stop(self):
		self.deselect_item()

	def set_cursor(self, action_name):
		self.netview.set_cursor(action_name)

	def draw(self, cr):
		pass

	def select_item(self, item):
		self.selected_item = item
		if item:
			self.netview.highlight(item)
			self.netview.set_entry_types(item.get_text_entries())
		else:
			self.netview.set_entry_types([])
			self.netview.highlight_off()

	def mouseover_highlight(self, item):
		self.netview.mouseover_highlight(item)

	def mouseover_highlight_off(self):
		self.netview.mouseover_highlight_off()

	def deselect_item(self):
		self.select_item(None)

	def right_button_down(self, event, position):
		def delete_event(w):
			self.selected_item.delete()
			self.deselect_item()

		if self.selected_item in self.net.pick_items(position):

			actions_dict = {
				Transition: [("Edit code", 
					lambda w: self.netview.transition_edit_callback(self.selected_item))],
				Place: [("Edit init code", 
					lambda w: self.netview.place_edit_callback(self.selected_item))],
				Edge: [ ("Switch direction",
							lambda w: self.selected_item.switch_direction()), 
						("Bidirectional",
							lambda w: self.selected_item.toggle_bidirectional()) ]
			}

			menu_actions = [("Delete", delete_event)]

			if type(self.selected_item) in actions_dict:
				menu_actions = actions_dict[type(self.selected_item)] + menu_actions

			gtkutils.show_context_menu(menu_actions, event)
		else:
			self.scroll_point = (event.x, event.y)
			self.set_cursor("scroll")

	def right_button_up(self, event, position):
		if self.scroll_point is not None:
			self.scroll_point = None
			self.set_cursor(None)

	def net_changed(self):
		if self.selected_item and not self.net.contains(self.selected_item):
			self.selected_item = None
			self.set_cursor(None)

	def item_at_position(self, position):
		return self.net.get_item_at_position(position, self.is_selectable)

	def left_button_down(self, event, position):
		item = self.item_at_position(position)
		if item:
			self.select_item(item)
			self.action = item.get_action(position)
			self.set_cursor(self.action)
			return True
		return False

	def left_button_up(self, event, position):
		self.action = None

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
				rel = utils.vector_diff(position, self.mouse_last_pos)
				self.selected_item.drag_move(self.action, self.action_start, position, rel)
			else:
				action = self.selected_item.get_action(position)
				self.set_cursor(action)
		self.mouse_last_pos = position


class SelectTool(NetTool):

	def is_selectable(self, item):
		return not item.is_area()


class NetItemTool(NetTool):

	def is_selectable(self, item):
		return not item.is_area()

	def left_button_down(self, event, position):
		if not NetTool.left_button_down(self, event, position):
			self.select_item(self.create_new(position))
			self.action = self.selected_item.get_action(position)
			self.set_cursor(self.action)

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
					item = self.net.get_item_at_position(position, lambda i: i.is_transition())
				else:
					item = self.net.get_item_at_position(position, lambda i: i.is_place())
				if item:
					edge = self.net.add_edge(self.from_item, item, self.points)
					self.select_item(edge)
					self.from_item = None
				else:
					self.points.append(position)
			else:
				self.from_item = self.net.get_item_at_position(position, lambda i: i.is_transition() or i.is_place())
				if self.from_item:
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

	def is_selectable(self, item):
		return item.is_area()

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



