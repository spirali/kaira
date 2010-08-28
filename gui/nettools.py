from net import Place, Transition
import utils

class NetTool:

	def __init__(self, netview):
		self.netview = netview
		self.net = netview.net

	def start(self):
		self.selected_item = None
		self.set_cursor(None)
		self.netview.set_entry_types([])

	def stop(self):
		self.deselect_item()

	def button_down(self, position):
		pass

	def button_up(self, position):
		pass

	def mouse_move(self, position):
		pass

	def set_cursor(self, action_name):
		self.netview.set_cursor(action_name)

	def draw(self, cr):
		pass

	def select_item(self, item):
		if self.selected_item:
			self.selected_item.highlight_off()
		self.selected_item = item
		if item:
			item.highlight_on()
			self.netview.set_entry_types(item.get_text_entries())

	def deselect_item(self):
		self.select_item(None)

	def right_button(self, event, position):
		if self.selected_item in self.net.pick_items(position):

			actions_dict = {
				Transition: [("Edit code", lambda w: self.netview.transition_edit_callback(self.selected_item))],
				Place: [("Edit init code", lambda w: self.netview.place_edit_callback(self.selected_item))]
			}

			menu_actions = [("Delete", lambda w: self.selected_item.delete())]

			if type(self.selected_item) in actions_dict:
				menu_actions = actions_dict[type(self.selected_item)] + menu_actions
			
			self.netview.show_context_menu(event, menu_actions)

	def net_changed(self):
		if self.selected_item and not self.net.contains(self.selected_item):
			self.selected_item = None
			self.set_cursor(None)

class NetItemTool(NetTool):

	def __init__(self, netview):
		NetTool.__init__(self, netview)

	def start(self):
		NetTool.start(self)
		self.action = None
		self.action_start = None
		self.action_last_pos = None

	def stop(self):
		NetTool.stop(self)

	def button_down(self, position):
		if self.selected_item:
			action = self.selected_item.get_action(position)
			if action:
				self.action = action
				self.action_start = position
				self.action_last_pos = position
				return
	
		action_tuple = self.net.get_action(position)

		if action_tuple is not None:
			item, action = action_tuple
			self.select_item(item)
		else:
			item = self.create_new(position)
			self.select_item(item)
		self.mouse_move(position)

	def button_up(self, position):
		self.action = None

	def mouse_move(self, position):
		if self.selected_item:
			if self.action:
				rel = utils.vector_diff(position, self.action_last_pos)
				self.selected_item.drag_move(self.action, self.action_start, position, rel)
			else:
				action = self.selected_item.get_action(position)
				self.set_cursor(action)
		self.action_last_pos = position
				


class PlaceTool(NetItemTool):

	def create_new(self, position):
		return self.net.add_place(position)


class TransitionTool(NetItemTool):

	def create_new(self, position):
		return self.net.add_transition(position)

class ArcTool(NetTool):

	def __init__(self, netview):
		NetTool.__init__(self, netview)

	def start(self):
		NetTool.start(self)
		self.from_item = None
		self.points = []
		self.action = None
		self.action_start = None


	def button_down(self, position):
		if self.selected_item and not self.from_item:
			self.action = self.selected_item.get_action(position)
			self.last_position = position
			self.action_start = position

	def button_up(self, position):
		if self.action:
			self.action = None
		else:
			if self.from_item:
				item = self.net.get_transition_or_place(position)
				if item:
					arc = self.net.add_arc(self.from_item, item, self.points)
					self.select_item(arc)
					self.from_item = None
				else:
					self.points.append(position)
			else:
				self.from_item = self.net.get_transition_or_place(position)
				if self.from_item:			
					self.last_position = position
					self.points = []
				else:
					action_tuple = self.net.get_action(position)
					if action_tuple and action_tuple[0].is_arc():
						item, action = action_tuple
						self.select_item(item)

	def mouse_move(self, position):
		if self.from_item:
			self.netview.redraw()
		if self.selected_item:
			if self.action:
				rel = utils.vector_diff(position, self.last_position)
				self.selected_item.drag_move(self.action, self.action_start, position, rel)
			else:
				action = self.selected_item.get_action(position)
				self.set_cursor(action)
		self.last_position = position

	def draw(self, cr):
		if self.from_item:
			cr.set_line_width(1.5)
			cr.set_source_rgb(0.0,0.0,0.0)
			if self.points:
				pp = self.points[0]
			else:
				pp = self.last_position
			pos = self.from_item.get_border_point(pp)
			utils.draw_polyline_arrow(cr, [pos] + self.points + [self.last_position], 0.5, 12)

class AreaTool(NetTool):

	def start(self):
		NetTool.start(self)
		self.point1 = None
		self.point2 = None
		self.action = None
		self.action_start = None

	def button_down(self, position):
		if self.selected_item:
			action = self.selected_item.get_action(position)
			if action:
				self.action = action
				self.action_start = position
				self.action_last_pos = position
				return
	
		action_tuple = self.net.get_area_action(position)
		if action_tuple is not None:
			item, action = action_tuple
			self.select_item(item)
			self.mouse_move(position)
		else:
			if self.point1:
				pass
			else:
				self.point1 = position
				self.point2 = position

	def mouse_move(self, position):
		if self.point1:
			self.netview.redraw()
		if self.selected_item:
			if self.action:
				rel = utils.vector_diff(position, self.point2)
				self.selected_item.drag_move(self.action, self.action_start, position, rel)
			else:
				action = self.selected_item.get_action(position)
				self.set_cursor(action)
		self.point2 = position


	def button_up(self, position):
		if self.action:
			self.action = None
			self.set_cursor(None)
		elif self.point1:
			p, s = utils.position_and_size_from_points(self.point1, self.point2)
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
		p, s = utils.position_and_size_from_points(self.point1, self.point2)
		cr.rectangle(p[0], p[1], s[0], s[1])



