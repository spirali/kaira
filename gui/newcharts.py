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
import os
import paths
import events as evt
import numpy as np
import matplotlib
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg \
                                               as FigureCanvas

from matplotlib.projections import register_projection
#import matplotlib.pyplot as plt

class BasicChart(matplotlib.axes.Axes, evt.EventSource):

    name = 'basic_chart'

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
        evt.EventSource.__init__(self)

        # chart data
        self.data = None
        # zoom properties
        self.zoom = 1.0
        self.zoom_stack = []
        self.zoom_rec = None
        # move properties
        self.xypress = None
        self.original_view_dim = None # TODO: ??
        # mask keys
        self.keymask = None           # TODO: ??
        # legend
        self.plegend = None

        # redraw properties
        self.xlock = False
        self.ylock = True
        # backgrounds
        self.cross_bg = None
        self.rec_bg = None

        # coonect standard features
        fig.canvas.mpl_connect("scroll_event", self._zoom)
        fig.canvas.mpl_connect("button_press_event", self._move_start)
        fig.canvas.mpl_connect("button_release_event", self._move_end)
        fig.canvas.mpl_connect("motion_notify_event", self._moving)

        # block the standard scroll window event if it is use CTRL mask
        fig.canvas.connect("scroll_event", 
                lambda w, e: e.state & gtk.gdk.CONTROL_MASK)
        self.figure.canvas.mpl_connect( # updade background after change window
                "draw_event", self._update_background)
        self.figure.canvas.mpl_connect(
                "motion_notify_event", self._draw_cross)
            
    def _update_background(self, event):
        self.cross_bg = self.figure.canvas.copy_from_bbox(self.bbox)

    def _draw_cross(self, event, select_bg=None):
        if self.xypress is None or select_bg is not None:
            if self.cross_bg is None:
                self.cross_bg = self.figure.canvas.copy_from_bbox(self.bbox)

            if self.in_axes(event):
                if select_bg is not None:
                    self.figure.canvas.restore_region(select_bg)
                elif self.cross_bg is not None:
                    self.figure.canvas.restore_region(self.cross_bg)

                inv = self.transAxes.inverted()
                x, y = inv.transform((event.x, event.y))

                xtext = time_to_string(event.xdata)[:-6]
                # the coefficient 7 is good result from an experiment :)
                xtext_pos = -7 * len(xtext) - 10 if x > 0.5 else 10
                ytext_pos = -20 if y > 0.5 else 30

                if not self.xlock:
                    l1 = matplotlib.lines.Line2D([x, x], [0, 1], c="#ff0000",
                            lw=1, transform=self.transAxes, figure=self.figure)
                    self.draw_artist(l1)

                    a1 = matplotlib.text.Annotation(
                            xtext, 
                            xy=(x, y), xycoords='axes fraction', 
                            xytext=(xtext_pos, ytext_pos), 
                            textcoords='offset points', 
                            bbox=dict(boxstyle="round", fc="#ffff00"))
                    a1.set_transform(matplotlib.transforms.IdentityTransform())
                    self._set_artist_props(a1)
                    self.draw_artist(a1)

                if not self.ylock:
                    l2 = matplotlib.lines.Line2D([0, 1], [y, y], c="#ff0000",
                            lw=1, transform=self.transAxes, figure=self.figure)
                    self.draw_artist(l2)

                    if self.xlock:
                        ytext_pos = -20 if y > 0.5 else 10
                    else:
                        ytext_pos -= 20
                    a2 = matplotlib.text.Annotation(
                            event.ydata,
                            xy=(x, y), xycoords='axes fraction',
                            xytext=(xtext_pos, ytext_pos), 
                            textcoords='offset points',
                            bbox=dict(boxstyle="round", fc="#ffff00"))
                    a2.set_transform(matplotlib.transforms.IdentityTransform())
                    self._set_artist_props(a2)
                    self.draw_artist(a2)

                self.figure.canvas.blit(self.bbox)
            else:
                if self.cross_bg is not None:
                    self.figure.canvas.restore_region(self.cross_bg)
                    self.figure.canvas.blit(self.bbox)
                    self.cross_bg = None
    

    def __convertAxesToData(self, x, y):
        # vypada ze se nemusi optimalizovat
        xdisplay, ydisplay = self.transAxes.transform((x,y))
        return self.transData.inverted().transform((xdisplay, ydisplay))

    def _draw_rectangle(self, event):
        if self.xypress is not None and self.in_axes(event):
            x_start, y_start = self.xypress
            x_end, y_end = event.x, event.y

            if self.rec_bg is None:
                self.rec_bg = self.figure.canvas.copy_from_bbox(self.bbox)
            else:
                self.figure.canvas.restore_region(self.rec_bg)

            inv = self.transData.inverted() 
            ax_x_start, ax_y_start = inv.transform((x_start, y_start))
            ax_x_end, ax_y_end = inv.transform((x_end, y_end))

            if self.xlock:
                ax_x_start = self.__convertAxesToData(0, 0)[0]
                ax_x_end = self.__convertAxesToData(1, 1)[0]

            if self.ylock:
                ax_y_start = self.__convertAxesToData(0, 0)[1]
                ax_y_end = self.__convertAxesToData(1, 1)[1]

            self.zoom_rec = (
                    min(ax_x_start, ax_x_end),
                    min(ax_y_start, ax_y_end),
                    max(ax_x_start, ax_x_end),
                    max(ax_y_start, ax_y_end))

            rec = matplotlib.patches.Rectangle((ax_x_start, ax_y_start),
                    width=(ax_x_end - ax_x_start),
                    height=(ax_y_end - ax_y_start),
                    fc="#0000ff", ec="#000000", alpha=0.2, lw=1,
                    transform=self.transData, figure=self.figure)

            self.draw_artist(rec)
            self.figure.canvas.blit(self.bbox)

            # draw ending cross
            select_bg = self.figure.canvas.copy_from_bbox(self.bbox)
            self._draw_cross(event, select_bg)

    def registr_pick_legend(self, legend, lines):
        lined = dict()
        for legline, originale in zip(legend.get_lines(), lines):
            legline.set_picker(5)
            lined[legline] = originale

        def on_pick(event):
            legline = event.artist
            [originale] = lined[legline]
            vis = not originale.get_visible()
            originale.set_visible(vis)

            if vis:
                legline.set_alpha(1.0)
            else:
                legline.set_alpha(0.2)

            self.figure.canvas.draw_idle()

        self.figure.canvas.mpl_connect('pick_event', on_pick)

    def _hide_legend(self, hide):
        if self.plegend is not None:
            self.plegend.set_visible(not(hide))
            self.figure.canvas.draw_idle()

    def _zoom(self, event):
        if event.key == "control": # CTRL mask
            xmin, xmax = None, None
            ymin, ymax = None, None

            if event.button == "down":
                if len(self.zoom_stack) == 0:
                    if self.original_view_dim is not None:
                        # TODO: is it correct in the sense of zooming, perhaps 
                        # this feature belongs to reset button?? (for Standa)
                        xmin, xmax, ymin, ymax = self.original_view_dim
                        self.original_view_dim = None
                else:
                    xmin, xmax, ymin, ymax = self.zoom_stack.pop()

            if (xmin is not None and xmax is not None and 
                    ymin is not None and ymax is not None):
                self.set_xlim(xmin, xmax)
                self.set_ylim(ymin, ymax)
                self.figure.canvas.draw_idle()

    # <-- methods for zooming chart

    # --> methods for mooving graph
    def _move_start(self, event):
        if event.button == 1:
            self.emit_event("change_slider", event.xdata)
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

        if self.cross_bg is not None and self.zoom_rec is not None:
            self.figure.canvas.restore_region(self.cross_bg)
            self.figure.canvas.blit(self.bbox)

            vmin_x, vmax_x = self.xaxis.get_view_interval()
            vmin_y, vmax_y = self.yaxis.get_view_interval()

            xmin, ymin, xmax, ymax = self.zoom_rec
            self.set_xlim(xmin, xmax)
            self.set_ylim(ymin, ymax)
            self.zoom_stack.append((vmin_x, vmax_x, vmin_y, vmax_y))
            self.zoom /= 1.05

            self.zoom_rec = None
            self.cross_bg = None
            self.rec_bg = None

            self.figure.canvas.draw_idle()

    def _moving(self, event):
        if self.xypress is not None and event.key == "control":
            self._draw_rectangle(event)

        elif self.xypress is not None: # "mouse drag"

            ''' Moving with chart. Coordinates must be transform
            bettween two coordinates system, because using pixel
            coordinates is better for moving with chart. '''
            xpress, ypress = self.xypress
            x, y = event.x, event.y
            diffx = xpress - x 
            diffy = ypress - y
            # coordinates in display (pixels) view
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

class PlaceChart(BasicChart):

    name = 'place_chart'

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
        BasicChart.__init__(self, fig, rec, axisbg, frameon, 
                sharex, sharey, label, xscale, yscale, **kwargs)

    def fill_data(self, data):
        self.data = data
        lines = []
        for ldata in data:
            line = self.plot(ldata.get_xvalues(), ldata.get_yvalues(),
                    'o-', drawstyle='steps-post', label=ldata.get_name())
            lines.append(line)

        for label in self.xaxis.get_ticklabels():
            label.set_rotation(-35) 
            label.set_horizontalalignment('left') 

        self.plegend = self.legend(loc="upper left", fancybox=True, shadow=True)
        self.registr_pick_legend(self.plegend, lines)

class HistogramChart(BasicChart):

    name = 'histogram_chart'

    def fill_data(self, names, values, color):

        lines = []

        max = 4200000
        for i, vals in enumerate(values):
            if not vals: return #if it's values list empty
            x = [key for key, val in vals.items()]
#
#            max = 0
#            for time in x:
#                if time > max:
#                    max = time

            #if are all values zero, doesn't make a sense to create the histogram
#            if max == 0: return

            step = max // 250 # Takovehle urcovani kroku dava celkem pekne vysledky
            newx_size = max // step + 1

            new_x = xrange(newx_size)
            new_y = [0] * newx_size
            for time in x:
                range = time // step
                new_y[range] += vals[time]

#            self.bar(new_x, new_y, color=color[i % len(color)], alpha=0.6)
            line = self.plot(new_x, new_y, color=color[i%len(color)],
                    lw=1, label=names[i])
            lines.append(line)

        for label in self.xaxis.get_ticklabels():
            label.set_fontsize(9)
            label.set_rotation(-35)
            label.set_horizontalalignment('left')

        self.plegend = self.legend(loc="upper right", fancybox=True, shadow=True)
        self.registr_pick_legend(self.plegend, lines)


class TimeSumChart(BasicChart):

    name = 'time_sum_chart'

    def fill_data(self, names, values):
        size = len(values)
        x = xrange(size)
        n, bins, patches = self.hist(x, size,
                range=(0, size), weights=values, facecolor='green', alpha=0.75)

        xticks = map(lambda x: x+0.5, x)
        self.set_xticks(xticks)
        self.set_xticklabels(names)

        for label in self.xaxis.get_ticklabels():
            label.set_fontsize(7)
            label.set_rotation(-45)
            label.set_horizontalalignment('left')

class UtilizationChart(BasicChart):

    name = 'utilization_chart'

#    def _moving(self, event):
#        """ move only on x axes. """
#        if self.xypress is not None: # drag canvas
#            xpress = self.xypress[0]
#            x = event.x
#            diffx = xpress - x
#            xmin = self.transAxes.transform((0,0))[0]
#            xmax = self.transAxes.transform((1,1))[0]
#            shift_xmin, shift_xmax = xmin + diffx, xmax + diffx
#            # coordinates in data view
#            inv = self.transData.inverted()
#            data_xmin = inv.transform((shift_xmin, 0))[0]
#            data_xmax = inv.transform((shift_xmax, 1))[0]
#            # set new view dimension
#            self.set_xlim(data_xmin, data_xmax)
#            self.xypress = (x, event.y) # shift for next step
#            self.figure.canvas.draw_idle()
##            self.draw_artist(self)
##            self.figure.canvas.blit(self.bbox)
#
#    def _zoom_in(self, stack, event):
#        """ zoom only on x axes. """
#        def new_bounds(min, max, value, zoom):
#            if value is None: return (min, max)
#            diff_min, diff_max = value - min, max - value
#            zdiff_min, zdiff_max = diff_min * zoom, diff_max * zoom
#            return (value - zdiff_min, value + zdiff_max)
#
#        self.zoom /= 1.05
#        vmin_x, vmax_x = self.xaxis.get_view_interval()
#        vmin_y, vmax_y = self.yaxis.get_view_interval()
#        x, y = event.xdata, event.ydata
#
#        xmin, xmax = new_bounds(vmin_x, vmax_x, x, self.zoom)
#
#        bad_xrange = False
#        if abs(xmax - xmin) < 0.000001:
#            bad_xrange = True
#            xmin, xmax = stack[-1][:2]
#
#        if bad_xrange:
#            # Returns the last known window dimension, it means stop zoom in.
#            self.zoom *= 1.05
#            return stack[-1]
#        else:
#            stack.append((vmin_x, vmax_x, vmin_y, vmax_y))
#            return (xmin, xmax, vmin_y, vmax_y)

    def fill_data(self, data, names, colors):
        self.data = data
        ywidth = 2
        yticks = []

        for i, ldata in enumerate(data):
            y = ((i+1) * ywidth) + (i+1)
            yticks.append(y + ywidth/2)
            self.broken_barh(ldata, (y, ywidth), edgecolor='face', 
                    facecolor=colors[0])

        self.set_yticks(yticks)
        self.set_yticklabels(names)

        for label in self.xaxis.get_ticklabels():
            label.set_rotation(-35) 
            label.set_horizontalalignment('left') 
        for i, label in enumerate(self.yaxis.get_ticklabels()):
            # add 3 white space on the begining of name
            names[i] = "   %s" % names[i] 
            label.set_horizontalalignment("left")
            label.set_verticalalignment('center')

        p = matplotlib.patches.Rectangle((0, 0), 1, 1, edgecolor=colors[0], 
                fc=colors[0]) 
        self.plegend = self.legend([p], ["Running"], loc="upper left",
                fancybox=True, shadow=True)


class ChartWidget(gtk.VBox):

    PLACE_CHART = PlaceChart.name
    UTILIZATION_CHART = UtilizationChart.name
    TIME_SUM_CHART = TimeSumChart.name
    HISTOGRAM_CHART = HistogramChart.name

    supported_charts_types = {
            PlaceChart.name : PlaceChart,
            UtilizationChart.name : UtilizationChart,
            TimeSumChart.name : TimeSumChart,
            HistogramChart.name : HistogramChart}

    def __init__(self, with_legend=True):
        gtk.VBox.__init__(self)
        self.with_legend = with_legend

        self.charts = []
        self.current_chart_id = None

        # registr new type of axes
        for name in ChartWidget.supported_charts_types:
            register_projection(ChartWidget.supported_charts_types[name])
        # chart toolbar
        toolbar = self._chart_toolbar()
        self.pack_start(toolbar, False, False)

        self.figure = matplotlib.figure.Figure()
        canvas = FigureCanvas(self.figure)

        # set size of canvas
        w, h = self.figure.get_figwidth(), self.figure.get_figheight()
        dpi = self.figure.get_dpi()
        canvas.set_size_request(int(w * dpi), int(h * dpi))

        sc = gtk.ScrolledWindow()
        sc.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sc.add_with_viewport(canvas)

        self.pack_start(sc, True, True, 0)

    def create_new_chart(self, type):
        if type in ChartWidget.supported_charts_types:
            ax = self.figure.add_subplot(111, projection=type)
            self.charts.append(ax)
            self.current_chart_id = len(self.charts) - 1
            return self.current_chart_id
        else:
            print "Unsupported type of chart '%s'!" % type
        
    def get_current_chart(self):
        return self.get_chart(self.current_chart_id)

    def get_chart(self, id):
        if id >= 0 and id < len(self.charts):
            self.current_chart_id = id
            return self.charts[id]
        else:
            print "Bad id '%d'!" % id
            return None

    def get_figure(self):
        return self.figure;

    def _chart_toolbar(self):
        toolbar = gtk.Toolbar()
        toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        toolbar.set_tooltips(True)

        btn_save = gtk.ToolButton()
        btn_save.connect("clicked", self._save_action)
        btn_save.set_stock_id(gtk.STOCK_SAVE)
        btn_save.set_tooltip_text("Save graph")
        toolbar.add(btn_save)

        toolbar.add(gtk.SeparatorToolItem())

        btn_restore = gtk.ToolButton()
        btn_restore.connect("clicked", lambda w: self._restore_view_action(
            self.get_current_chart()))
        btn_restore.set_stock_id(gtk.STOCK_ZOOM_100)
        btn_restore.set_tooltip_text("Restore view")
        toolbar.add(btn_restore)

        toolbar.add(gtk.SeparatorToolItem())

        icon_hide_legend = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "hide_legend.svg"))
        btn_hide_legend = gtk.ToggleToolButton()
        btn_hide_legend.set_icon_widget(icon_hide_legend)
        btn_hide_legend.set_tooltip_text("Hide legend")
        btn_hide_legend.connect("toggled", self._hide_legend)
        btn_hide_legend.set_sensitive(self.with_legend)
        toolbar.add(btn_hide_legend)

        btn_zoom = gtk.ToggleToolButton("Zoom")
        btn_zoom.set_tooltip_text("Resize (Keep CTRL)")
        toolbar.add(btn_zoom)

        return toolbar

    def _hide_legend(self, widget):
        ax = self.get_current_chart()
        hide = widget.get_active()
        ax._hide_legend(hide)

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

    def _save_action(self, widget):
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
            self.figure.savefig(dialog.get_filename())

        dialog.destroy()

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

def time_to_string(nanosec):
    s = int(nanosec) / 1000000000
    nsec = nanosec % 1000000000
    sec = s % 60
    minutes = (s / 60) % 60
    hours = s / 60 / 60
    return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)
