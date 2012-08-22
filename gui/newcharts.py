#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                  2012       Martin Surkovsky
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
import numpy as np
import matplotlib.pyplot as plt

class ChartWidget(gtk.VBox):

    def __init__(self, chart):
        gtk.VBox.__init__(self)
        self.chart = chart
        toolbar = self._chart_toolbar()
        self.pack_start(toolbar, False, False)


    def _chart_toolbar(self):
        toolbar = gtk.Toolbar()
        toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        toolbar.set_tooltips(True)

        btn_save = gtk.ToolButton()
        btn_save.set_stock_id(gtk.STOCK_SAVE)
        toolbar.add(btn_save)

        toolbar.add(gtk.SeparatorToolItem())

        btn_zoom_100 = gtk.ToolButton()
        btn_zoom_100.set_stock_id(gtk.STOCK_ZOOM_100)
        toolbar.add(btn_zoom_100)

        btn_zoom_in = gtk.ToolButton()
        btn_zoom_in.set_stock_id(gtk.STOCK_ZOOM_IN)
        toolbar.add(btn_zoom_in)

        btn_zoom_out = gtk.ToolButton()
        btn_zoom_out.set_stock_id(gtk.STOCK_ZOOM_OUT)
        toolbar.add(btn_zoom_out)

        toolbar.add(gtk.SeparatorToolItem())

        btn_hide_legend = gtk.ToggleToolButton(gtk.STOCK_NO)
        toolbar.add(btn_hide_legend)

        return toolbar

class Chart:

    def __init__(self, title, fontsize=12):
        self.title = title
        self.fontsize = fontsize
        self.zoom = 1.0

    def get_title(self):
        return self.title

    def set_title(self, title):
        self.title = title

    def get_fontsize(self):
        return self.fontsize

    def get_fontsize(self, fontsize):
        self.fontsize = fontsize

    def get_zoom(self):
        return self.zoom

    def set_zoom(self, zoom):
        self.zoom = zoom

class TwoAxesChart(Chart):

    def __init__(self, values, min_value = None, max_value = None, 
            xlabel = None, ylabel = None, xlabel_formatter = None, 
            ylabel_formatter = None):

       self.values = values
       self.min_value = min_value
       self.max_value = max_value
       self.ylabel = None
       self.xlabel = None
       self.xlabel_formatter = xlabel_formatter
       self.ylabel_formatter = ylabel_formatter
    
    def draw(self, figure):
        pass

