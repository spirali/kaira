#
#    Copyright (C) 2010, 2011 Stanislav Bohm
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

import gtk
import cairo

class Canvas(gtk.DrawingArea):

    def __init__(self, config, zoom = 1.0):
        gtk.DrawingArea.__init__(self);
        self.zoom = zoom
        self.viewport = None

        self.set_config(config)

        self.set_events(
            gtk.gdk.BUTTON_PRESS_MASK |
            gtk.gdk.BUTTON_RELEASE_MASK |
            gtk.gdk.POINTER_MOTION_MASK |
            gtk.gdk.LEAVE_NOTIFY_MASK)

        self.connect("expose_event", self._expose)
        self.connect("button_press_event", self._button_down)
        self.connect("button_release_event", self._button_up)
        self.connect("motion_notify_event", self._mouse_move)
        self.connect("leave-notify-event", self._mouse_leave)

    def set_config(self, config):
        if config:
            config.canvas = self
        self.config = config
        self.redraw()

    def reset_viewport(self):
        self.set_viewport(None)

    def set_viewport(self, viewport):
        self.viewport = viewport
        self.redraw()

    def get_viewport(self):
        return self.viewport

    def redraw(self):
        self.queue_draw()

    def zoom_in(self):
        self.zoom *= 1.25
        self.redraw()

    def zoom_out(self):
        self.zoom /= 1.25
        self.redraw()

    def get_zoom(self):
        return self.zoom

    def set_cursor(self, cursor):
        self.window.set_cursor(cursor)

    def save_as_svg(self, filename):
        size = (1000, 1000)
        surface = cairo.SVGSurface(filename, size[0], size[1])
        try:
            cr = cairo.Context(surface)
            self._draw(cr, size[0], size[1])
        finally:
            surface.finish()

    def _expose(self, w, event):
        cr = self.window.cairo_create()
        self.cr = cr
        cr.rectangle(event.area.x, event.area.y,
                event.area.width, event.area.height)
        cr.clip()
        self._draw(cr, *self.window.get_size())


    def _draw(self, cr, width, height):
        cr.set_source_rgb(0.8, 0.8, 0.8)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        # If viewport is None then set viewport to the center of net
        if self.viewport is None:
            if self.config is not None:
                self.viewport = self.config.get_default_viewport()
            else:
                self.viewport = (0, 0)

        cr.translate(width / 2, height / 2)
        cr.scale(self.zoom, self.zoom)
        cr.translate(-self.viewport[0], -self.viewport[1])

        if self.config:
            self.config.draw(cr)

    def _mouse_to_canvas(self, event):
        return self.cr.device_to_user(event.x, event.y)

    def _button_down(self, w, event):
        if event.button == 1:
            self.config.on_mouse_left_down(event, self._mouse_to_canvas(event))
        elif event.button == 3:
            self.config.on_mouse_right_down(event, self._mouse_to_canvas(event))

    def _button_up(self, w, event):
        if event.button == 1:
            self.config.on_mouse_left_up(event, self._mouse_to_canvas(event))
        elif event.button == 3:
            self.config.on_mouse_right_up(event, self._mouse_to_canvas(event))

    def _mouse_move(self, w, event):
        self.config.on_mouse_move(event, self._mouse_to_canvas(event))

    def _mouse_leave(self, w, event):
        self.config.on_mouse_leave(event)
