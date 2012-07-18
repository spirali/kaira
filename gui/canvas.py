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
from events import EventSource
from drawing import VisualConfig

class NetCanvas(gtk.DrawingArea, EventSource):
    """
        Widget that draws a network, configurated by instance of class VisualConfig
        Events: button_down, button_up, mouse_move
    """
    def __init__(self, net, draw_cb, vconfig = None, zoom = 1.0):
        gtk.DrawingArea.__init__(self);
        EventSource.__init__(self)
        self.net = net
        self.zoom = zoom
        self.viewport = None

        if vconfig is None:
            vconfig = VisualConfig()
        self.vconfig = vconfig
        self.draw_cb = draw_cb
        self.set_events(gtk.gdk.BUTTON_PRESS_MASK | gtk.gdk.BUTTON_RELEASE_MASK | gtk.gdk.POINTER_MOTION_MASK)
        self.connect("expose_event", self._expose)
        self.connect("button_press_event", self._button_down)
        self.connect("button_release_event", self._button_up)
        self.connect("motion_notify_event", self._mouse_move)

    def set_net(self, net):
        self.net = net
        self.set_viewport_to_net_center()
        self.redraw()

    def set_viewport_to_net_center(self):
        self.viewport = None

    def set_viewport(self, viewport):
        self.viewport = viewport
        self.redraw()

    def get_viewport(self):
        return self.viewport

    def set_vconfig(self, vconfig):
        if vconfig is None:
            vconfig = VisualConfig()
        self.vconfig = vconfig
        self.redraw()

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
            if self.net is not None:
                ((l,t), (r,b)) = self.net.corners(cr)
                self.viewport = (l + (r - l) / 2, t + (b - t) / 2)
            else:
                self.viewport = (0, 0)


        cr.translate(width / 2, height / 2)
        cr.scale(self.zoom, self.zoom)
        cr.translate(-self.viewport[0], -self.viewport[1])

        if self.net:
            self.net.draw(cr, self.vconfig)
        if self.draw_cb:
            self.draw_cb(cr, width, height)

    def _mouse_to_canvas(self, event):
        return self.cr.device_to_user(event.x, event.y)

    def _button_down(self, w, event):
        self.emit_event("button_down", event, self._mouse_to_canvas(event))

    def _button_up(self, w, event):
        self.emit_event("button_up", event, self._mouse_to_canvas(event))

    def _mouse_move(self, w, event):
        self.emit_event("mouse_move", event, self._mouse_to_canvas(event))
