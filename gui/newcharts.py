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
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg \
                                               as FigureCanvas

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

    def _restore_view_action(self, ax): #TODO zrusit vnitrek
        xmin, xmax = ax.xaxis.get_data_interval()
        ymin, ymax = ax.yaxis.get_data_interval()
        ax.set_xlim(0, xmax) # TODO: spatne reseni, chce to jit uvnitr axes pres
                             # pouzity zasobnik => TwoAxisChart nejspise bude 
                             # muset rozsirit klasickou funkcionalitu Axes, 
                             # tzn. TwoAxisChart(matplotlib.axes.Axes)
        ax.set_ylim(ymin, ymax)
#        ax.figure.canvas.draw_idle()

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

class TwoAxesChart:

    def __init__(self, axes, data, min_value=None, max_value=None, 
            title="", xlabel="", ylabel=""):

        self.zoom = 1.0
        self.zoom_stack = []
        
        self.xypress = None
        self.original_view_dim = None

        self.ax = axes
        self.data = data

        self.min_value = min_value
        self.max_value = max_value
        self.ax.set_title(title)
        self.ax.set_xlabel(xlabel)
        self.ax.set_ylabel(ylabel)

        # coonect standard features
        #?? Proc nefunguje volani self._zoom a musim jit pres lambdu??
        self.ax.figure.canvas.mpl_connect(
                "scroll_event", lambda e: self._zoom(e))
        self.ax.figure.canvas.mpl_connect(
                "button_press_event", lambda e: self._move_start(e))
        self.ax.figure.canvas.mpl_connect(
                "button_release_event", lambda e: self._move_end(e))
        self.ax.figure.canvas.mpl_connect(
                "motion_notify_event", lambda e: self._moving(e))

    def redraw(self):
        self.ax.figure.canvas.draw_idle()

    def draw(self):
        for ldata in self.data:
            self.ax.plot(ldata.get_xvalues(), ldata.get_yvalues(),
                    'o-', drawstyle='steps-post')

        # toto lepe umistit
        if self.min_value is not None: #Doresit: musi zde byt pro kazdou
                                       #souradnici min a max hodnota.
            self.ax.set_xlim(xmin=self.min_value)
            
        for label in self.ax.xaxis.get_ticklabels():
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
        vmin_x, vmax_x = self.ax.xaxis.get_view_interval()
        vmin_y, vmax_y = self.ax.yaxis.get_view_interval()
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
                    xmin, xmax = self.ax.xaxis.get_view_interval()
                    ymin, ymax = self.ax.yaxis.get_view_interval()
            else:
                xmin, xmax, ymin, ymax = self._zoom_out(self.zoom_stack)

        if (xmin is not None and xmax is not None and 
                ymin is not None and ymax is not None):
            self.ax.set_xlim(xmin, xmax)
            self.ax.set_ylim(ymin, ymax)
            self.redraw()
    # <-- methods for zooming chart

    # --> methods for mooving graph
    def _move_start(self, event):
        if event.button == 1:
            self.xypress = (event.x, event.y)
            #save original view for restore view
            if len(self.zoom_stack) > 0 and self.original_view_dim is None:
                self.original_view_dim = self.zoom_stack[0]
            elif self.original_view_dim is None:
                xmin, xmax = self.ax.xaxis.get_view_interval()
                ymin, ymax = self.ax.yaxis.get_view_interval()
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
            trans = self.ax.transAxes
            # TODO: timto ziskanim ohraniceni je mozne, ze vznikne
            # problem pri zmenseni plochy platna, napr. dva
            # grafy vedle sebe => OVERIT! K realnym hodnotam by se snad dalo
            # dostat pres nejakou metodu v Axes.
            xmin, ymin = trans.transform((0,0))
            xmax, ymax = trans.transform((1,1))
            shift_xmin, shift_xmax = xmin + diffx, xmax + diffx
            shift_ymin, shift_ymax = ymin + diffy, ymax + diffy
            # coordinates in data view
            inv = self.ax.transData.inverted()
            data_xmin, data_ymin = inv.transform((shift_xmin, shift_ymin))
            data_xmax, data_ymax = inv.transform((shift_xmax, shift_ymax))
            # set new view dimension
            self.ax.set_xlim(data_xmin, data_xmax)
            self.ax.set_ylim(data_ymin, data_ymax)
            self.xypress = (x, y) # shift for next step
            self.redraw()

    # <-- methods for mooving graph

    # --> methods for formatting x, y labels
    def set_xlabel_formatter(self, xlabel_formatter):
        self.ax.xaxis.set_major_formatter(
                self._create_label_formatter(xlabel_formatter))

    def set_ylabel_formatter(self, ylabel_formatter):
        self.ax.yaxis.set_major_formatter(
                self._create_label_formatter(ylabel_formatter))

    def _create_label_formatter(self, label_formatter):
        return matplotlib.ticker.FuncFormatter(
                lambda v, pos: label_formatter(v))
    # <-- methods for formating x, y labels

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
