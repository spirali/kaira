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
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg \
                                               as FigureCanvas
from matplotlib.projections import register_projection
#import matplotlib.pyplot as plt

class ChartWidget(gtk.VBox):

    def __init__(self):
        gtk.VBox.__init__(self)
        self.axes = []
        self.current_axes_id = 1
        # registr new type of axes
#        register_projection(TwoAxisChart)

        toolbar = self._chart_toolbar()
        self.pack_start(toolbar, False, False)

        self.fig = matplotlib.figure.Figure()
        canvas = FigureCanvas(self.fig)
#        # set size of canvas
        w, h = self.fig.get_figwidth(), self.fig.get_figheight()
        dpi = self.fig.get_dpi()
        canvas.set_size_request(int(w * dpi), int(h * dpi))

        sc = gtk.ScrolledWindow()
#        sc.set_shadow_type(gtk.SHADOW_ETCHED_IN)
        sc.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sc.add_with_viewport(canvas)
#        self.pack_start(canvas, True, True)

        self.pack_start(sc, True, True, 0)

    def get_current_axes(self):
        if self.current_axes_id >= len(self.axes):
#            ax = self.fig.add_subplot(111, projection="twoaxeschart")
            ax = self.fig.add_subplot(111)
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

        btn_restore = gtk.ToolButton()
        btn_restore.connect("clicked", lambda w: self._restore_view_action(
            self.get_current_axes()))
        btn_restore.set_stock_id(gtk.STOCK_ZOOM_100)
        btn_restore.set_tooltip_text("Restore view")
        toolbar.add(btn_restore)

        toolbar.add(gtk.SeparatorToolItem())

        btn_hide_legend = gtk.ToggleToolButton(gtk.STOCK_NO)
        btn_hide_legend.set_tooltip_text("Hide legend")
        toolbar.add(btn_hide_legend)

        return toolbar

    def _restore_view_action(self, ax):
        restore = False
        if ax.original_view_dim is not None:
            xmin, xmax, ymin, ymax = ax.original_view_dim
            restore = True
        elif len(ax.zoom_stack) > 0:
            xmin, xmax, ymin, ymax = ax.zoom_stack[0]
            restore = True

        if restore:
            ax.original_view_dim = None
            ax.zoom_stack = []
            ax.set_xlim(xmin, xmax)
            ax.set_ylim(ymin, ymax)
            ax.figure.canvas.draw_idle()

    def save_action(self, widget):
        dialog = gtk.FileChooserDialog("Save graph", 
                                       None, gtk.FILE_CHOOSER_ACTION_SAVE,
                                       (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                                        gtk.STOCK_SAVE, gtk.RESPONSE_OK))
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

class TwoAxisChart(matplotlib.axes.Axes):

    name = 'twoaxeschart'

    def __init__(self, fig, rec,
                 axisbg  = None, # defaults to rc axes.facecolor
                 frameon = True,
                 sharex  = None,
                 sharey  = None,
                 label   = "",
                 xscale  = None,
                 yscale  = None,
                 **kwargs
                 ):

        matplotlib.axes.Axes.__init__(self, fig, rec, axisbg, frameon,
                sharex, sharey, label, xscale, yscale, **kwargs)

        # Zoom properties
        self.zoom = 1.0
        self.zoom_stack = []
        # Move properties
        self.xypress = None
        self.original_view_dim = None

        # coonect standard features
        fig.canvas.mpl_connect("scroll_event", self._zoom)
        fig.canvas.mpl_connect("button_press_event", self._move_start)
        fig.canvas.mpl_connect("button_release_event", self._move_end)
        fig.canvas.mpl_connect("motion_notify_event", self._moving)

        # ?? PROC TOTO NEFUNGUJE??
        def press(event):
            print "Press", event.key
        fig.canvas.mpl_connect('key_press_event', press)

#        fig.canvas.mpl_connect('key_release_event', press)

    # TODO: vymyslet lepsi nekolidujici nazev
    def vytvor_graf(self, data):
        for ldata in data:
            self.plot(ldata.get_xvalues(), ldata.get_yvalues(),
                    'o-', drawstyle='steps-post')

        # TODO: toto lepe umistit
        for label in self.xaxis.get_ticklabels():
            label.set_rotation(-35) #TODO: rotation do prommene
            label.set_horizontalalignment('left') #TODO: taktez do promene,
                                                  # nejlepe udelat tridni 
                                                  # vlastnost.
        
    # --> methods for zooming chart
    def _zoom_in(self, stack, event):
        def new_bounds(min, max, value, zoom):
            if value is None: return (min, max)
            diff_min, diff_max = value - min, max - value
            zdiff_min, zdiff_max = diff_min * zoom, diff_max * zoom
            return (value - zdiff_min, value + zdiff_max)

        self.zoom /= 1.05
        vmin_x, vmax_x = self.xaxis.get_view_interval()
        vmin_y, vmax_y = self.yaxis.get_view_interval()
        x, y = event.xdata, event.ydata

        xmin, xmax = new_bounds(vmin_x, vmax_x, x, self.zoom)
        ymin, ymax = new_bounds(vmin_y, vmax_y, y, self.zoom)

        bad_xrange, bad_yrange = False, False
        if abs(xmax - xmin) < 0.000001:
            bad_xrange = True
            xmin, xmax = stack[-1][:2]

        if abs(ymax - ymin) < 0.000001:
            bad_yrange = True
            ymin, ymax = stack[-1][2:]

        if bad_xrange and bad_yrange:
            # Return the last known window dimension, it means stop zoom in.
            self.zoom *= 1.05
            return stack[-1]
        else:
            stack.append((vmin_x, vmax_x, vmin_y, vmax_y))
            return (xmin, xmax, ymin, ymax)
        
    def _zoom_out(self, stack):
        self.zoom *= 1.05
        return stack.pop()

    def _zoom(self, event):
        xmin, xmax = None, None
        ymin, ymax = None, None
        if event.button == "up":
            xmin, xmax, ymin, ymax = self._zoom_in(self.zoom_stack,event)
        elif event.button == "down":
            if len(self.zoom_stack) == 0:
                if self.original_view_dim is not None:
                    # TODO: is it correct in the sense of zooming, perhaps 
                    # this feature belongs to reset button?? (for Standa)
                    xmin, xmax, ymin, ymax = self.original_view_dim
                    self.original_view_dim = None
                else:
                    xmin, xmax = self.xaxis.get_view_interval()
                    ymin, ymax = self.yaxis.get_view_interval()
            else:
                xmin, xmax, ymin, ymax = self._zoom_out(self.zoom_stack)

        if (xmin is not None and xmax is not None and 
                ymin is not None and ymax is not None):
            self.set_xlim(xmin, xmax)
            self.set_ylim(ymin, ymax)
            self.figure.canvas.draw_idle()
    # <-- methods for zooming chart

    # --> methods for mooving graph
    def _move_start(self, event):
        if event.button == 1:
            self.xypress = (event.x, event.y)
            #save original view for restore view
            if len(self.zoom_stack) > 0 and self.original_view_dim is None:
                self.original_view_dim = self.zoom_stack[0]
            elif self.original_view_dim is None:
                xmin, xmax = self.xaxis.get_view_interval()
                ymin, ymax = self.yaxis.get_view_interval()
                self.original_view_dim = (xmin, xmax, ymin, ymax)

    def _move_end(self, event):
        if event.button == 1:
            self.xypress = None

    def _moving(self, event):
        ''' Moving with chart. Coordinates must be transform
        bettween two coordinates system, because using pixel
        coordinates is better for moving with chart. '''
        if self.xypress is not None: # "mouse drag"
            xpress, ypress = self.xypress
            x, y = event.x, event.y
            diffx = xpress - x 
            diffy = ypress - y
            # coordinates in display (pixels) view
            # TODO: timto ziskanim ohraniceni je mozne, ze vznikne
            # problem pri zmenseni plochy platna, napr. dva
            # grafy vedle sebe => OVERIT! K realnym hodnotam by se snad dalo
            # dostat pres nejakou metodu v Axes.
            xmin, ymin = self.transAxes.transform((0,0))
            xmax, ymax = self.transAxes.transform((1,1))
            shift_xmin, shift_xmax = xmin + diffx, xmax + diffx
            shift_ymin, shift_ymax = ymin + diffy, ymax + diffy
            # coordinates in data view
            inv = self.transData.inverted()
            data_xmin, data_ymin = inv.transform((shift_xmin, shift_ymin))
            data_xmax, data_ymax = inv.transform((shift_xmax, shift_ymax))
            # set new view dimension
            self.set_xlim(data_xmin, data_xmax)
            self.set_ylim(data_ymin, data_ymax)
            self.xypress = (x, y) # shift for next step
            self.figure.canvas.draw_idle()

    # <-- methods for mooving graph

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
