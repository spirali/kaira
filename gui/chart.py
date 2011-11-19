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

import cairo
import math
import gtk
import utils

LEFT = 0
TOP = 1
RIGHT = 2
BOTTOM = 3
color_names =  [
 'red', 'green', 'blue',
 '#8A2BE2',
 '#A52A2A',
 '#DEB887',
 '#5F9EA0',
 '#7FFF00',
 '#D2691E',
 '#FF7F50',
 '#6495ED',
 '#FFF8DC',
 '#DC143C',
 '#00FFFF',
 '#00008B',
 '#008B8B',
 '#B8860B',
 '#A9A9A9',
 '#A9A9A9',
 '#006400',
 '#BDB76B',
 '#8B008B',
 '#556B2F',
 '#FF8C00',
 '#9932CC',
 '#8B0000',
 '#E9967A',
 '#8FBC8F',
 '#483D8B',
 '#2F4F4F',
 '#2F4F4F',
 '#00CED1',
 '#9400D3',
 '#FF1493',
 '#00BFFF',
 '#696969',
 '#696969',
 '#1E90FF',
 '#B22222',
 '#FFFAF0',
 '#228B22',
 '#FF00FF',
 '#DCDCDC',
 '#F8F8FF',
 '#FFD700',
 '#DAA520',
 '#808080',
 '#808080',
 '#008000',
 '#ADFF2F',
 '#F0FFF0',
 '#FF69B4',
 '#CD5C5C',
 '#4B0082',
 '#FFFFF0',
 '#F0E68C',
 '#E6E6FA',
 '#FFF0F5',
 '#7CFC00',
 '#FFFACD',
 '#ADD8E6',
 '#F08080',
 '#E0FFFF',
 '#FAFAD2',
 '#D3D3D3',
 '#D3D3D3',
 '#90EE90',
 '#FFB6C1',
 '#FFA07A',
 '#20B2AA',
 '#87CEFA',
 '#778899',
 '#778899',
 '#B0C4DE',
 '#FFFFE0',
 '#00FF00',
 '#32CD32',
 '#FAF0E6',
 '#FF00FF',
 '#800000',
 '#66CDAA',
 '#0000CD',
 '#BA55D3',
 '#9370D8',
 '#3CB371',
 '#7B68EE',
 '#00FA9A',
 '#48D1CC',
 '#C71585',
 '#191970',
 '#F5FFFA',
 '#FFE4E1',
 '#FFE4B5',
 '#FFDEAD',
 '#000080',
 '#FDF5E6',
 '#808000',
 '#6B8E23',
 '#FFA500',
 '#FF4500',
 '#DA70D6',
 '#EEE8AA',
 '#98FB98',
 '#AFEEEE',
 '#D87093',
 '#FFEFD5',
 '#FFDAB9',
 '#CD853F',
 '#FFC0CB',
 '#DDA0DD',
 '#B0E0E6',
 '#800080',
 '#FF0000',
 '#BC8F8F',
 '#4169E1',
 '#8B4513',
 '#FA8072',
 '#F4A460',
 '#2E8B57',
 '#FFF5EE',
 '#A0522D',
 '#C0C0C0',
 '#87CEEB',
 '#6A5ACD',
 '#708090',
 '#708090',
 '#FFFAFA',
 '#00FF7F',
 '#4682B4',
 '#D2B48C',
 '#008080',
 '#D8BFD8',
 '#FF6347',
 '#40E0D0',
 '#EE82EE',
 '#F5DEB3',
 '#FFFFFF',
 '#F5F5F5',
 '#FFFF00',
 '#9ACD32']

def color_values():
    for name in color_names:
        c = gtk.gdk.color_parse(name)
        yield (c.red / 65535.0, c.green / 65535.0, c.blue / 65535.0)

class TwoAxisChart:

    borders = [30,30,60,60]
    xlabel_format = str
    ylabel_format = str

    def __init__(self, xbounds, ybounds):
        self.ybounds = ybounds
        self.xbounds = xbounds

        self.ylabels = self.labels(ybounds)
        self.xlabels = self.labels(xbounds)

    def _delta(self, bounds):
        mn, mx = bounds
        delta = mx - mn
        if delta % 10 > 0:
            delta += 10 - (delta % 10)
        return delta

    def xdelta(self):
        return self._delta(self.xbounds)

    def ydelta(self):
        return self._delta(self.ybounds)

    def labels(self, bounds):
        delta = self._delta(bounds)
        return [ int(i * (delta / 10.0) + bounds[0]) for i in xrange(11) ]


    def render(self, width, height, cr):
        cr.set_source_rgb(1.0, 1.0, 1.0)
        cr.rectangle(0, 0, width, height)
        cr.fill()

        self._render_axis(width, height, cr)

    def _render_axis(self, width, height, cr):
        labelw, labelh = get_label_sizes(cr, map(self.ylabel_format, self.ylabels))
        cr.set_source_rgb(0.0, 0.0, 0.0)
        cr.move_to(self.borders[LEFT], height - self.borders[BOTTOM])
        cr.line_to(width - self.borders[RIGHT], height - self.borders[BOTTOM])
        cr.stroke()
        cr.move_to(self.borders[LEFT], height - self.borders[BOTTOM])
        cr.line_to(self.borders[LEFT], self.borders[TOP])
        cr.stroke()

        unit = (height - (self.borders[TOP] + self.borders[BOTTOM])) / 10.0
        y = height - self.borders[BOTTOM]
        x = self.borders[LEFT]
        for i in xrange(11):
            yy = y - i * unit
            cr.move_to(x - 4, yy)
            cr.line_to(x + 4, yy)
            cr.stroke()
            cr.move_to(x - 10 - labelw, yy + labelh / 2)
            cr.show_text(self.ylabel_format(self.ylabels[i]))

        unit = (width - (self.borders[LEFT] + self.borders[RIGHT])) / 10.0
        for i in xrange(11):
            xx = x + i * unit
            cr.move_to(xx, y - 4)
            cr.line_to(xx, y + 4)
            cr.stroke()
            cr.move_to(xx + 5, y + 10)
            cr.save()
            cr.rotate(0.5)
            cr.show_text(self.xlabel_format(self.xlabels[i]))
            cr.restore()


class TimeChart(TwoAxisChart):

    def __init__(self, values, min_time, max_time, min_value = None, max_value = None):
        self.min_time = min_time
        self.max_time = max_time
        self.values = values

        if min_value is None:
            min_value = min(min(x[1] for x in timeline) for timeline in values)
        if max_value is None:
            max_value = max(max(x[1] for x in timeline) for timeline in values)
        TwoAxisChart.__init__(self, (min_time, max_time), (min_value, max_value))

    def render(self, width, height, cr):
        def coords(time, value):
            return (((time - self.xbounds[0])/xdelta) * w + x, y - ((value - self.ybounds[0]) / ydelta) * h)

        TwoAxisChart.render(self, width, height, cr)
        xdelta = float(self.xdelta())
        ydelta = float(self.ydelta())

        y = height - self.borders[BOTTOM]
        x = self.borders[LEFT]

        w = width - (self.borders[LEFT] + self.borders[RIGHT])
        h = height - (self.borders[TOP] + self.borders[BOTTOM])
        cr.set_line_width(0.5)
        colors = list(color_values())
        for i, line in enumerate(self.values[:]):
            cr.set_source_rgb(*colors[i % len(colors)])
            for j in xrange(len(line) - 1):
                time1, value1 = line[j]
                time2, value2 = line[j + 1]
                s, v = coords(time1, value1)
                e, z = coords(time2, value2)
                cr.move_to(s, v)
                cr.line_to(e, v)
                cr.stroke()
                cr.move_to(e, v)
                cr.line_to(e, z)
                cr.stroke()
                cr.arc(s, v, 2, 0, 2 * math.pi)
                cr.fill()
            s, v = coords(line[-1][0], line[-1][1])
            cr.arc(s, v, 2, 0, 2 * math.pi)
            cr.fill()

class UtilizationChart:

    xlabel_format = str

    def __init__(self, values, labels, legend, colors, timebounds):
        self.values = values
        self.labels = labels
        self.legend = legend
        self.colors = colors
        self.timebounds = timebounds

    def get_size_request(self):
        return 0, 40 * len(self.values) + 40 + 75 # legend_space + reserved space at the end

    def render(self, width, height, cr):
        labelw, labelh = get_label_sizes(cr, self.labels)
        boxh = 40
        legend_space = 40
        timespan = self.timebounds[1] - self.timebounds[0]
        start = 30 + labelw
        areaw = width - start - 40
        for i in xrange(len(self.values)):
            self._gradient_box(cr, 0, i * boxh + legend_space, width, boxh, (1.0,1.0,1.0), (0.9,0.9,0.9))

        cr.set_source_rgb(0.2,0.2,0.2)
        for i, label in enumerate(self.labels):
            cr.move_to(10, i * boxh + (boxh + labelh) / 2 + legend_space)
            cr.show_text(label)

        cr.set_line_width(1.0)
        min_time = self.timebounds[0]
        for i in xrange(11):
            t = i * timespan / 10 + min_time
            x = i * areaw / 10 + start
            bottom = len(self.values) * boxh + legend_space
            cr.move_to(x, legend_space)
            cr.line_to(x, bottom)
            cr.stroke()
            cr.save()
            cr.move_to(x, bottom + 10)
            cr.rotate(1.2)
            cr.show_text(self.xlabel_format(t))
            cr.restore()

        for i, u in enumerate(self.values):
            posy = i * boxh + legend_space + 10
            for j, v in enumerate(u):
                posyy = (float(boxh) - 20) / len(u) * j + posy
                h = (float(boxh) - 20) / len(u)
                for (begin, t), (end, x) in utils.pairs_generator(v):
                    if t is None:
                        continue
                    s = areaw * (begin - min_time) / timespan + start
                    e = areaw * (end - min_time) / timespan + start
                    color1, color2 = self.colors[t]
                    self._gradient_box(cr, s, posyy, e - s, h, color1, color2)

        x = start
        for color, label in self.legend:
            self._gradient_box(cr, x, 12, 15, 15, self.colors[color][0], self.colors[color][1])
            sx, sy = utils.text_size(cr, label)
            cr.move_to(x + 20, 19 + sy / 2)
            cr.show_text(label)
            x += 40 + sx

    def _gradient_box(self, cr, x, y, w, h, color1, color2):
        gradient = cairo.LinearGradient(x, y, x, y + h)
        gradient.add_color_stop_rgb(0.0, *color1)
        gradient.add_color_stop_rgb(1.0, *color2)
        cr.set_source(gradient)
        cr.rectangle(x, y, w, h)
        cr.fill()

class ChartWidget(gtk.DrawingArea):

    def __init__(self, chart):
        gtk.DrawingArea.__init__(self)
        self.chart = chart
        self.connect("expose_event", self._expose)

    def _expose(self, w, event):
        cr = self.window.cairo_create()
        cr.rectangle(event.area.x, event.area.y,
                event.area.width, event.area.height)
        cr.clip()
        size = self.window.get_size()
        self.chart.render(size[0], size[1], cr)



def get_label_sizes(cr, labels):
    label_sizes = [ utils.text_size(cr, label) for label in labels ]
    return max(x[0] for x in label_sizes), max(x[1] for x in label_sizes)
