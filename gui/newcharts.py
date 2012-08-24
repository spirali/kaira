#
#    Copyright (C) 2012 Martin Surkovsky
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
import matplotlib
import matplotlib.pyplot as plt
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg as FigureCanvas

class ChartWidget(gtk.VBox):

    def __init__(self):
        gtk.VBox.__init__(self)
        self.axes = []
        self.current_axes_id = 1

        toolbar = self._chart_toolbar()
        self.pack_start(toolbar, False, False)

        self.fig = plt.Figure()
#        self.fig.subplots_adjust(bottom=0.14)

#        sw = gtk.ScrolledWindow()
#        sw.set_shadow_type(gtk.SHADOW_ETCHED_IN)
#        sw.set_policy(gtk.POLICY_ALWAYS, gtk.POLICY_ALWAYS)
#        self.pack_start(sw, True, True, 0)

        canvas = FigureCanvas(self.fig)
#        self.fig.canvas.mpl_connect('button_press_event', self.onpress)
        self.pack_start(canvas, True, True)
#        sw.add_with_viewport(canvas)

    def get_current_axes(self):
        if self.current_axes_id >= len(self.axes):
            ax = self.fig.add_subplot(self.current_axes_id, 1,
                    self.current_axes_id)
            self.axes.append(ax)
            return ax
        elif self.current_axes_id >= 0:
            return self.axes[self.current_axes_id]
        else:
            print 'Error in \'get_current_axes\' method!'
            return None

    def add_new_axes(self):
        self.current_axes_id += 1;

    def get_figure(self):
        return self.fig;

    def _chart_toolbar(self):
        toolbar = gtk.Toolbar()
        toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        toolbar.set_tooltips(True)

        btn_save = gtk.ToolButton()
        btn_save.connect("clicked", self.save_action)
        btn_save.set_stock_id(gtk.STOCK_SAVE)
        btn_save.set_tooltip_text("Save graph")
        toolbar.add(btn_save)

        toolbar.add(gtk.SeparatorToolItem())

        btn_zoom_in = gtk.ToolButton()
        btn_zoom_in.set_stock_id(gtk.STOCK_ZOOM_IN)
        btn_zoom_in.set_tooltip_text("Zoom in")
        toolbar.add(btn_zoom_in)

        btn_zoom_out = gtk.ToolButton()
        btn_zoom_out.set_stock_id(gtk.STOCK_ZOOM_OUT)
        btn_zoom_out.set_tooltip_text("Zoom out")
        toolbar.add(btn_zoom_out)

        toolbar.add(gtk.SeparatorToolItem())

        btn_hide_legend = gtk.ToggleToolButton(gtk.STOCK_NO)
        btn_hide_legend.set_tooltip_text("Hide legend")
        toolbar.add(btn_hide_legend)

        return toolbar

    def save_action(self, widget):
        dialog = gtk.FileChooserDialog("Save graph", None, gtk.FILE_CHOOSER_ACTION_SAVE,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL, gtk.STOCK_SAVE, gtk.RESPONSE_OK))
        dialog.set_default_response(gtk.RESPONSE_OK)

        svg_filter = gtk.FileFilter()
        svg_filter.set_name("Vector images")
        svg_filter.add_mime_type("image/svg")
        svg_filter.add_pattern("*.svg")
        dialog.add_filter(svg_filter)

        raster_filter = gtk.FileFilter()
        raster_filter.set_name("Raster images")
        raster_filter.add_mime_type("image/png")
        raster_filter.add_mime_type("image/jpeg")
        raster_filter.add_mime_type("image/gif")
        raster_filter.add_pattern("*.png")
        raster_filter.add_pattern("*.jpg")
        raster_filter.add_pattern("*.gif")
        dialog.add_filter(raster_filter)

        response = dialog.run()
        if response == gtk.RESPONSE_OK:
            self.fig.savefig(dialog.get_filename())

        dialog.destroy()

    def onpress(self, event):
        if event.button != 1: return
        x, y = event.xdata, event.ydata
        ax = self.get_current_axes()
        ax.set_xlim(x - 0.1, x + 0.1)
        ax.set_ylim(y - 0.1, y + 0.1)
        self.fig.canvas.draw()

class TwoAxesChart:

    def __init__(self, axes, data, min_value=None, max_value=None, 
            title=None, xlabel=None, ylabel=None):

        self.zoom = 1.0
        self.zoom_stack = []
        self.zoom_step = 1.25
        self.zoom_pb = ""
        xlabel_formatter = None
        ylabel_formatter = None
        
        self.ax = axes
        self.data = data

        self.min_value = min_value
        self.max_value = max_value
        if title is not None:
            self.ax.set_title(title)
        if xlabel is not None:
            self.ax.set_xlabel(xlabel)
        if ylabel is not None:
            self.ax.set_ylabel(ylabel)
    
    def draw(self):
        for ldata in self.data:
            self.ax.plot(ldata.get_xvalues(), ldata.get_yvalues(),
                    'o-', drawstyle='steps-post')

        # toto lepe umistit
        if self.min_value is not None:
            self.ax.set_xlim(xmin=self.min_value)
            
        for label in self.ax.xaxis.get_ticklabels():
            label.set_rotation(-35)
            label.set_horizontalalignment('left')
        

    def _put(self, stack, event):
        def new_bounds(min, max, value, zoom):
            diff_min = value - min
            diff_max = max - value
            return (diff_min * zoom, diff_max * zoom)

        self.zoom = self.put_zoom(self.zoom, self.zoom_step)
        vmin_x, vmax_x = self.ax.xaxis.get_view_interval()
        x = event.xdata
        nmin_x, nmax_x = new_bounds(vmin_x, vmax_x, x, self.zoom)
        nmin_x, nmax_x = x - nmin_x, x + nmax_x
        #TODO: nejak odstranit podminku, je zde kvuli 
        #      tomu aby se nezoomoval graf do nekonecna
        if abs(nmax_x - nmin_x) < 0.001:
            self.zoom = self.pop_zoom(self.zoom, self.zoom_step)
            return stack[-1]
        else:
            stack.append((vmin_x, vmax_x))
            return (nmin_x, nmax_x)
        
    def _pop(self, stack, event):
        ''' 
            *event* is there only, because _put and _pop must have same arite.
        '''
        self.zoom = self.pop_zoom(self.zoom, self.zoom_step)
        return stack.pop()

    def _zoom(self, event, figure):
        if len(self.zoom_stack) == 0:
            #pb is previous_button
            self.zoom_pb = event.button
            self.manipulate = self._put
            if event.button == 'up':
                self.put_zoom = lambda zoom, step: zoom / step
                self.pop_zoom = lambda zoom, step: zoom * step
            elif event.button == 'down':
                self.put_zoom = lambda zoom, step: zoom * step
                self.pop_zoom = lambda zoom, step: zoom / step
            else:
                print 'Unexpected mouse button:', event.button
        else:
            if self.zoom_pb != event.button:
                if self.manipulate == self._put:
                    self.manipulate = self._pop
                elif self.manipulate == self._pop:
                    self.manipulate = self._put
                else:
                    print 'error'

        self.zoom_pb = event.button
        xmin, xmax = self.manipulate(self.zoom_stack, event)
        self.ax.set_xlim(xmin, xmax)
        figure.canvas.draw()
        print len(self.zoom_stack) 

    def OLD_zoom(self, event, figure):
        def new_bounds(min, max, value, zoom):
            diff_min = value - min
            diff_max = max - value
            return (diff_min * zoom, diff_max * zoom)

        if event.button == 'up':
            self.zoom_in /= 1.25
            self.zoom_out = 1.0
            vmin_x, vmax_x = self.ax.xaxis.get_view_interval()
            x = event.xdata
            nmin_x, nmax_x = new_bounds(vmin_x, vmax_x, x, self.zoom_in)
            self.ax.set_xlim(x - nmin_x, x + nmax_x)

            vmin_y, vmax_y = self.ax.yaxis.get_view_interval()
            y = event.ydata
            nmin_y, nmax_y = new_bounds(vmin_y, vmax_y, y, self.zoom_in)
            self.ax.set_ylim(y - nmin_y, y + nmax_y)
            figure.canvas.draw()
        elif event.button == 'down':
            self.zoom_out *= 1.25
            self.zoom_in = 1.0
            vmin_x, vmax_x = self.ax.xaxis.get_view_interval()
            x = event.xdata
            nmin_x, nmax_x = new_bounds(vmin_x, vmax_x, x, self.zoom_out)
            self.ax.set_xlim(x - nmin_x, x + nmax_x)

            vmin_y, vmax_y = self.ax.yaxis.get_view_interval()
            y = event.ydata
            nmin_y, nmax_y = new_bounds(vmin_y, vmax_y, y, self.zoom_out)
            self.ax.set_ylim(y - nmin_y, y + nmax_y)
            figure.canvas.draw()


    def set_xlabel_formatter(self, xlabel_formatter):
        self.ax.xaxis.set_major_formatter(self._create_label_formatter(xlabel_formatter))

    def set_ylabel_formatter(self, ylabel_formatter):
        self.ax.yaxis.set_major_formatter(self._create_label_formatter(ylabel_formatter))

    def _create_label_formatter(self, label_formatter):
        return matplotlib.ticker.FuncFormatter(
                lambda val, pos: label_formatter(val))

class Data2DChart:
    """ Data structure for a 2D charts. """
    def __init__(self, data):
        self.lines = []
        self.parse_data(data)

    def __iter__(self):
        self.idx = 0
        return self

    def next(self):
        if self.idx >= len(self.lines):
            raise StopIteration
        else:
            idx = self.idx
            self.idx += 1 
            return self.lines[idx]

    def parse_data(self, data):
        """ Parsing input data in to this structure. """
        names, values = data
        for line, name in enumerate(names):
            xvalues, yvalues = zip(*values[line])
            self.lines.append(Line(name, xvalues, yvalues))

class Line:

    def __init__(self, name, xvalues, yvalues):
        self.name = name
        self.xvalues = xvalues
        self.yvalues = yvalues

    def get_name(self):
        return self.name

    def get_xvalues(self):
        return self.xvalues

    def get_yvalues(self):
        return self.yvalues
