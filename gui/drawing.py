import math
import utils

class VisualConfig:

	def get_token_strings(self, p):
		return []

	def get_highlight(self, item):
		return None

	def get_messages(self, item):
		return None

	def transition_drawing(self, item):
		return TransitionDrawing(item)

	def place_drawing(self, item):
		return PlaceDrawing(item)

	def edge_drawing(self, item):
		return EdgeDrawing(item)

	def area_drawing(self, item):
		return AreaDrawing(item)


class DrawingBase:
	
	def __init__(self):
		self.highlight = None
		self.error_messages = None

	def set_highlight(self, highlight):
		self.highlight = highlight

	def set_error_messages(self, error_messages):
		self.error_messages = error_messages

	def z_level(self):
		return 0

class TransitionDrawing(DrawingBase):

	def __init__(self, item):
		DrawingBase.__init__(self)
		self.position = item.get_position()
		self.size = item.get_size()
		self.name = item.get_name()
		self.guard = item.get_guard()
		self.doubleborder = item.get_code().strip() != ""

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

	def draw_top(self, cr):
		if self.error_messages and "guard" in self.error_messages:
			px, py = self.position
			sx, sy = utils.text_size(cr, self.guard)
			tx = px - sx / 2
			ty = py - self.size[1]/2 - sy/2 - 2
			draw_error_box_after_text(cr, self.guard, tx, ty, self.error_messages["guard"])


class PlaceDrawing(DrawingBase):

	def __init__(self, item):
		DrawingBase.__init__(self)
		self.position = item.get_position()
		self.radius = item.get_radius()
		self.error_messages = None
		self.highlight = None
		self.doubleborder = item.get_code().strip() != ""
		self.init_string = item.get_init_string()
		self.place_type = item.get_place_type()
		self.tokens = None

	def set_tokens(self, tokens):
		self.tokens = []
		for token in tokens:
			if len(token) > 25:
				self.tokens.append(token[:18] + " ... (%i chars)" % len(token))
			else:
				self.tokens.append(token)

	def draw(self, cr):
		px, py = self.position
		cr.arc(px, py, self.radius, 0, 2 * math.pi)
		cr.set_source_rgb(1, 1, 1)
		cr.fill()

		if self.highlight:
			cr.arc(px, py, self.radius, 0, 2 * math.pi)
			cr.set_line_width(6.5)
			cr.set_source_rgba(*self.highlight)
			cr.stroke()

		cr.arc(px, py, self.radius, 0, 2 * math.pi)
		cr.set_line_width(1.5)
		cr.set_source_rgb(0,0,0)
		cr.stroke()

		if self.doubleborder:
			cr.arc(px, py, self.radius - 3, 0, math.pi * 2)
			cr.stroke()

		x = math.sqrt((self.radius * self.radius) / 2) + 5
		if self.init_string:
			cr.set_source_rgb(0,0,0)
			cr.move_to(px + x, py - x)
			cr.show_text(self.init_string)
		if self.place_type:
			cr.set_source_rgb(0,0,0)
			cr.move_to(px + x, py + x)
			cr.show_text(self.place_type)


	def draw_top(self, cr):
		px, py = self.position
		if self.tokens:
			# Draw green circle
			x = math.sqrt((self.radius * self.radius) / 2) + 15
			cr.set_source_rgb(0.2,0.45,0)
			cr.arc(px + self.radius,py,8, 0, 2 * math.pi)
			cr.fill()

			cr.set_line_width(0.5)
			cr.arc(px + self.radius,py,8, 0, 2 * math.pi)
			cr.set_source_rgb(0,0,0)
			cr.stroke()

			count_text = str(len(self.tokens))
			w, h = utils.text_size(cr, count_text)
			cr.set_source_rgb(0.8,0.8,0.8)
			cr.move_to(px + self.radius - w/2, py + h/2)
			cr.show_text(count_text)

			# Print token names
			texts = [ (t, utils.text_size(cr, t)) for t in self.tokens ]
			texts = [ (t, (x, y + 1)) for (t, (x, y)) in texts ]
			text_height = sum([ x[1][1] for x in texts ])
			text_width = max([ x[1][0] for x in texts ])

			text_x = px + self.radius + 12
			text_y = py - text_height / 2

			cr.set_source_rgba(0.2,0.45,0,0.5)
			cr.rectangle(text_x - 3, text_y - 3, text_width + 6, text_height + 6)
			cr.fill()

			cr.set_source_rgb(0.0,0.0,0.0)
			cr.set_source_rgb(1.0,1.0,1.0)

			for (t, (x, y)) in texts:
				text_y += y
				cr.move_to(text_x, text_y)
				cr.show_text(t)

		x = math.sqrt((self.radius * self.radius) / 2) + 5
		if self.error_messages and "type" in self.error_messages:
			draw_error_box_after_text(cr, self.place_type, (px + x, py + x), self.error_messages["type"])
		if self.error_messages and "init" in self.error_messages:
			draw_error_box_after_text(cr, self.init_string, (px + x, py - x), self.error_messages["init"])

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

		if self.inscription_position:
			point = self.inscription_position
			# Cheal hack how to obtain inscription size
			self.item.inscription_size = utils.text_size(cr, self.inscription)
			sx, sy = self.item.inscription_size
			if self.highlight:
				cr.set_source_rgba(*self.highlight)
				cr.rectangle(point[0], point[1], sx, sy)
				cr.fill()
				
			cr.set_source_rgb(0,0,0)
			cr.set_line_width(1.0)
			cr.move_to(point[0], point[1] + sy)
			cr.show_text(self.inscription)

	def draw_top(self, cr):
		if self.error_messages and "inscription" in self.error_messages:
			draw_error_box_after_text(cr, self.inscription, self.inscription_position, self.error_messages["inscription"])


class AreaDrawing(DrawingBase):

	def __init__(self, item):
		DrawingBase.__init__(self)
		self.position = item.get_position()
		self.size = item.get_size()
		self.error_messages = None
		self.highlight = None
		self.count_expr = item.get_count_expr()

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

		cr.set_line_width(2.5)
		cr.set_source_rgb(0,0,0)
		cr.rectangle(px, py, sx, sy)
		cr.stroke()

		cr.set_source_rgb(0,0,0)
		cr.set_line_width(1.0)
		cr.move_to(px, py - 5)
		cr.show_text(self.count_expr)

	def draw_top(self, cr):
		if self.error_messages and "instances" in self.error_messages:
			px, py = self.position
			draw_error_box_after_text(cr, self.count_expr,(px, py - 5), self.error_messages["instances"])

	def z_level(self):
		return -1


def draw_error_box_after_text(cr, text, position, lines):
	if text is not None:
		sx, sy = utils.text_size(cr, text)
		draw_error_box(cr, (position[0] + 5 + sx, position[1]), lines)
	else:
		draw_error_box(cr, position, lines)

def draw_error_box(cr, position, lines):
	assert len(lines) > 0
	letter_y = utils.text_size(cr,"W")[1] * 1.5
	size_x = max([ utils.text_size(cr, line)[0] for line in lines ]) + 10
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


