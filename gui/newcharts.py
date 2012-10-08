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
from matplotlib.axes        import Axes as mpl_Axes
from matplotlib.lines       import Line2D as mpl_Line
from matplotlib.patches     import Rectangle as mpl_Rectangle
from matplotlib.text        import Annotation as mpl_Annotation
from matplotlib.transforms  import IdentityTransform as mpl_IdentityTransform
from matplotlib.ticker      import FuncFormatter as mpl_FuncFormatter
from matplotlib.projections import register_projection \
                                   as mpl_register_projection
from matplotlib.figure      import Figure as mpl_Figure
from matplotlib.backends.backend_gtkagg import FigureCanvasGTKAgg \
                                               as mpl_FigureCanvas

class BasicChart(mpl_Axes, evt.EventSource):

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

        mpl_Axes.__init__(self, fig, rec, axisbg, frameon,
                sharex, sharey, label, xscale, yscale, **kwargs)
        evt.EventSource.__init__(self)

        # chart data
        self.data = None
        # zoom properties
        self.zoom_stack = []
        self.zoom_rect = None
        # move properties
        self.xypress = None 
        self.original_view_dim = None
        # legend
        self.plegend = None

        # locking axes
        self.xlock = False
        self.ylock = False
        # move with canvas
        self.moving_flag = False

        # redraw properties (backgrounds)
        self.cross_bg = None
        self.rect_bg = None

        # coonect standard features, for Kaira graphs

        # updade background after change window
        fig.canvas.mpl_connect("draw_event", self._update_background)
        # register left button click
        fig.canvas.mpl_connect("button_press_event", self._drag_point)
        fig.canvas.mpl_connect("button_release_event", self._drop_point)
        # register drawing of position cross
        fig.canvas.mpl_connect("motion_notify_event", self._draw_cross)
        # register zooming methods
        fig.canvas.mpl_connect("motion_notify_event", self._draw_rectangle)
        fig.canvas.mpl_connect("button_release_event", self._zoom_in)
        fig.canvas.mpl_connect("button_press_event", self._zoom_out)
        # register moving events
        fig.canvas.mpl_connect("button_press_event", self._move_start)
        fig.canvas.mpl_connect("motion_notify_event", self._moving)
        fig.canvas.mpl_connect(
                "key_press_event", self._switch_moving_flag_action)
        # register axes locking events
        fig.canvas.mpl_connect("key_press_event", self._switch_xlock_action)
        fig.canvas.mpl_connect("key_release_event", self._switch_xlock_action)
        fig.canvas.mpl_connect("key_press_event", self._switch_ylock_action)
        fig.canvas.mpl_connect("key_release_event", self._switch_ylock_action)

    def __convertAxesToData(self, x, y):
        xdisplay, ydisplay = self.transAxes.transform((x,y))
        return self.transData.inverted().transform((xdisplay, ydisplay))

    def _update_background(self, event):
        self.cross_bg = self.figure.canvas.copy_from_bbox(self.bbox)
 
    def _drag_point(self, event):
        if event.button == 1:
            self.xypress = (event.x, event.y)

    def _drop_point(self, event):
        if event.button == 1:
            self.xypress = None

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
                    l1 = mpl_Line([x, x], [0, 1], c="#ff0000",
                            lw=1, transform=self.transAxes, figure=self.figure)
                    self.draw_artist(l1)

                    a1 = mpl_Annotation(
                            xtext, 
                            xy=(x, y), xycoords='axes fraction', 
                            xytext=(xtext_pos, ytext_pos), 
                            textcoords='offset points', 
                            bbox=dict(boxstyle="round", fc="#ffff00"))
                    a1.set_transform(mpl_IdentityTransform())
                    self._set_artist_props(a1)
                    self.draw_artist(a1)

                if not self.ylock:
                    l2 = mpl_Line([0, 1], [y, y], c="#ff0000",
                            lw=1, transform=self.transAxes, figure=self.figure)
                    self.draw_artist(l2)

                    if self.xlock:
                        ytext_pos = -20 if y > 0.5 else 10
                    else:
                        ytext_pos -= 20
                    a2 = mpl_Annotation(
                            event.ydata,
                            xy=(x, y), xycoords='axes fraction',
                            xytext=(xtext_pos, ytext_pos), 
                            textcoords='offset points',
                            bbox=dict(boxstyle="round", fc="#ffff00"))
                    a2.set_transform(mpl_IdentityTransform())
                    self._set_artist_props(a2)
                    self.draw_artist(a2)

                self.figure.canvas.blit(self.bbox)
            else:
                if self.cross_bg is not None:
                    self.figure.canvas.restore_region(self.cross_bg)
                    self.figure.canvas.blit(self.bbox)
                    self.cross_bg = None
    
    def _draw_rectangle(self, event):
        if not self.moving_flag and \
            self.xypress is not None and self.in_axes(event):

            x_start, y_start = self.xypress
            x_end, y_end = event.x, event.y

            if self.rect_bg is None:
                self.rect_bg = self.figure.canvas.copy_from_bbox(self.bbox)
            else:
                self.figure.canvas.restore_region(self.rect_bg)

            inv = self.transData.inverted() 
            ax_x_start, ax_y_start = inv.transform((x_start, y_start))
            ax_x_end, ax_y_end = inv.transform((x_end, y_end))

            if self.xlock:
                ax_x_start = self.__convertAxesToData(0, 0)[0]
                ax_x_end = self.__convertAxesToData(1, 1)[0]

            if self.ylock:
                ax_y_start = self.__convertAxesToData(0, 0)[1]
                ax_y_end = self.__convertAxesToData(1, 1)[1]

            self.zoom_rect = (
                    min(ax_x_start, ax_x_end),
                    min(ax_y_start, ax_y_end),
                    max(ax_x_start, ax_x_end),
                    max(ax_y_start, ax_y_end))

            rec = mpl_Rectangle((ax_x_start, ax_y_start),
                    width=(ax_x_end - ax_x_start),
                    height=(ax_y_end - ax_y_start),
                    fc="#0000ff", ec="#000000", alpha=0.1, lw=1,
                    transform=self.transData, figure=self.figure)

            self.draw_artist(rec)
            self.figure.canvas.blit(self.bbox)

            # draw ending cross
            select_bg = self.figure.canvas.copy_from_bbox(self.bbox)
            self._draw_cross(event, select_bg)

    def _zoom_in(self, event):
        if self.zoom_rect is not None:
            vmin_x, vmax_x = self.xaxis.get_view_interval()
            vmin_y, vmax_y = self.yaxis.get_view_interval()
            self.zoom_stack.append((vmin_x, vmax_x, vmin_y, vmax_y))

            xmin, ymin, xmax, ymax = self.zoom_rect
            self.set_xlim(xmin, xmax)
            self.set_ylim(ymin, ymax)

            self.zoom_rect = None
            self.cross_bg = None
            self.rect_bg = None

            self.figure.canvas.draw_idle()

    def _zoom_out(self, event):
        if event.button == 3:
            xmin, xmax = None, None
            ymin, ymax = None, None

            if len(self.zoom_stack) == 0:
                if self.original_view_dim is not None:
                    xmin, xmax, ymin, ymax = self.original_view_dim
                    self.original_view_dim = None
            else:
                xmin, xmax, ymin, ymax = self.zoom_stack.pop()

            if (xmin is not None and xmax is not None and 
                ymin is not None and ymax is not None):
                self.set_xlim(xmin, xmax)
                self.set_ylim(ymin, ymax)
                self.figure.canvas.draw_idle()

    def _move_start(self, event):
        ''' Save original view dimension, if it's still possible, if it's 
        still unused zoom.'''
        if len(self.zoom_stack) == 0 and self.original_view_dim is None:
            # Save original view for restoring a chart.
            vmin_x, vmax_x = self.xaxis.get_view_interval()
            vmin_y, vmax_y = self.yaxis.get_view_interval()
            self.original_view_dim = (vmin_x, vmax_x, vmin_y, vmax_y)

    def _moving(self, event):
        ''' Moving with chart. Coordinates must be transform
        bettween two coordinates system, because using pixel
        coordinates is better for moving with chart. '''

        if self.moving_flag and self.xypress is not None: # "mouse drag"
        
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
            # shift for next step
            self.xypress = (x, y) 
            self.figure.canvas.draw_idle()

    def _switch_xlock_action(self, event):
        # hint: ctrl+control is returned after release ctrl key. 
        # It coul'd be a bug of the matplotlib.
        if not self.moving_flag and \
            (event.key == 'control' or event.key == 'ctrl+control'):

            self.set_xlock(not self.xlock)
            if event.x is not None and event.y is not None:
                self._draw_cross(event)

    def _switch_ylock_action(self, event):
        if not self.moving_flag and event.key == 'shift':
            self.set_ylock(not self.ylock)
            if event.x is not None and event.y is not None:
                self._draw_cross(event)

    def _switch_moving_flag_action(self, event):
        if event.key == 'm':
            self.set_moving_flag(not self.moving_flag)

    def set_xlock(self, lock):
        self.xlock = lock
        self.emit_event("xlock_changed", lock)

    def set_ylock(self, lock):
        self.ylock = lock
        self.emit_event("ylock_changed", lock)

    def set_moving_flag(self, move):
        self.moving_flag = move
        self.emit_event("moving_flag_changed", move)

    def hide_legend(self, hide):
        if self.plegend is not None:
            self.plegend.set_visible(not(hide))
            self.figure.canvas.draw_idle()

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

class TimeChart(BasicChart):

    '''This chart is connect to replay. It's realize through  the 
    'x or y (time) axis'. It's important so that the axis of time coresponds
    with the replay slider!'''

    name = 'time_chart'

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
        self.__init__(self, fig, rec, axisbg, frameon, sharex, sharey,
                label, xscale, yscale, kwargs)

        # Connect the connection to replay slider. Event is connected through
        # gtk connect not mpl_connect, because canvas extends gtk.DrawingArea.
        fig.canvas.mpl_connect("button_press_event", self._double_click)

    def set_time_axis(self, axis):
        pass

    def _double_click(self, event):
        '''Connect to the replay window.'''
        if event.button == 1 and event.guiEvent.type == gtk.gdk._2BUTTON_PRESS:
            print 'double click'
#            self.emit_event("change_slider", event.xdata) 


class ChartWidget(gtk.VBox):

    def __init__(self, figure, with_legend=True, xlock=False, ylock=False):
        gtk.VBox.__init__(self)

        self.figure = figure
        ax = figure.gca() # TODO: Is it corrent??

        # chart toolbar
        toolbar = self._chart_toolbar(ax, with_legend)
        self.pack_start(toolbar, False, False)

        # It's necessary to set thouse lock arguments after creating a toolbar.
        ax.set_xlock(xlock)
        ax.set_ylock(ylock)

        # set size of canvas
        w, h = self.figure.get_figwidth(), self.figure.get_figheight()
        dpi = self.figure.get_dpi()
        self.figure.canvas.set_size_request(int(w * dpi), int(h * dpi))

        sc = gtk.ScrolledWindow()
        sc.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_AUTOMATIC)
        sc.add_with_viewport(self.figure.canvas)

        self.pack_start(sc, True, True, 0)

    def get_figure(self):
        return self.figure;

    def _chart_toolbar(self, ax, with_legend):
        toolbar = gtk.Toolbar()
        toolbar.set_icon_size(gtk.ICON_SIZE_SMALL_TOOLBAR)
        toolbar.set_tooltips(True)

        btn_save = gtk.ToolButton()
        btn_save.connect("clicked", self._btn_save_action)
        btn_save.set_stock_id(gtk.STOCK_SAVE)
        btn_save.set_tooltip_text("Save graph")
        toolbar.add(btn_save)

        toolbar.add(gtk.SeparatorToolItem())

        btn_restore = gtk.ToolButton()
        btn_restore.connect("clicked", 
                lambda w: self._btn_restore_view_action(self.figure.gca()))
        btn_restore.set_stock_id(gtk.STOCK_ZOOM_100)
        btn_restore.set_tooltip_text("Restore view")
        toolbar.add(btn_restore)

        toolbar.add(gtk.SeparatorToolItem())

        icon_hide_legend = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "hide_legend.svg"))
        btn_hide_legend = gtk.ToggleToolButton()
        btn_hide_legend.set_icon_widget(icon_hide_legend)
        btn_hide_legend.set_tooltip_text("Hide legend")
        btn_hide_legend.connect("toggled", self._btn_hide_legend_action)
        btn_hide_legend.set_sensitive(with_legend)
        toolbar.add(btn_hide_legend)

        toolbar.add(gtk.SeparatorToolItem())

        icon_xlock = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "xlock.svg"))
        btn_xlock = gtk.ToggleToolButton()
        btn_xlock.set_icon_widget(icon_xlock)
        btn_xlock.set_tooltip_text("Lock X-axis (keep CTRL)")
        btn_xlock.connect("toggled", self._btn_xlock_action)
        ax.set_callback("xlock_changed", 
                lambda xlock: btn_xlock.set_active(xlock))
        toolbar.add(btn_xlock)

        icon_ylock = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "yunlock.svg"))
        btn_ylock = gtk.ToggleToolButton()
        btn_ylock.set_icon_widget(icon_ylock)
        btn_ylock.set_tooltip_text("Lock Y-axis (keep CTRL)")
        btn_ylock.connect("toggled", self._btn_ylock_action)
        ax.set_callback("ylock_changed",
                lambda ylock: btn_ylock.set_active(ylock))
        toolbar.add(btn_ylock)

        icon_moving = gtk.image_new_from_file(
                os.path.join(paths.ICONS_DIR, "moving.svg"))
        btn_moving = gtk.ToggleToolButton()
        btn_moving.set_icon_widget(icon_moving)
        btn_moving.set_tooltip_text("Catch canvas (press key 'm')")
        btn_moving.connect("toggled", self._btn_moving_action)
        ax.set_callback("moving_flag_changed",
                lambda moving_flag: self._moving_flag_changed(
                    moving_flag, btn_moving, btn_xlock, btn_ylock))
        toolbar.add(btn_moving)

        return toolbar

    def _moving_flag_changed(self, moving_flag, btn_moving, btn_xlock, btn_ylock):
        btn_moving.set_active(moving_flag)
        btn_xlock.set_sensitive(not moving_flag)
        btn_ylock.set_sensitive(not moving_flag)

    def _btn_moving_action(self, widget):
        ax = self.figure.gca()
        moving_flag = widget.get_active()
        ax.set_moving_flag(moving_flag)

    def _btn_xlock_action(self, widget):
        ax = self.figure.gca()
        lock = widget.get_active()
        ax.set_xlock(lock)

    def _btn_ylock_action(self, widget):
        ax = self.figure.gca()
        lock = widget.get_active()
        ax.set_ylock(lock)

    def _btn_hide_legend_action(self, widget):
        ax = self.figure.gca()
        hide = widget.get_active()
        ax.hide_legend(hide)

    def _btn_restore_view_action(self, ax):
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

    def _btn_save_action(self, widget):
        # TODO: poradne navrhnout ukladaci okno!!
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

#*******************************************************************************
# Defined method for "standard" graphs:

def time_sum_chart(names, values, title, xlabel, ylabel):
    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    ax = figure.add_subplot(111, projection=BasicChart.name)

    size = len(values)
    x = xrange(size)
    n, bins, patches = ax.hist(x, size,
            range=(0, size), weights=values, facecolor='green', alpha=0.75)

    xticks = map(lambda x: x+0.5, x)
    ax.set_xticks(xticks)
    ax.set_xticklabels(names)

    for label in ax.xaxis.get_ticklabels():
        label.set_fontsize(7)
        label.set_rotation(-45)
        label.set_horizontalalignment('left')

    ax.yaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: time_to_string(time)[:10]))
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure, with_legend=False, xlock=True)

def utilization_chart(names, data, colors, title, xlabel, ylabel):
    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    # TODO: Change it to TimeChart
    ax = figure.add_subplot(111, projection=BasicChart.name)

    ywidth = 2
    yticks = []

    for i, ldata in enumerate(data):
        y = ((i+1) * ywidth) + (i+1)
        yticks.append(y + ywidth/2)
        ax.broken_barh(
                ldata, (y, ywidth), edgecolor='face', facecolor=colors[0])

    ax.set_yticks(yticks)
    ax.set_yticklabels(names)

    for label in ax.xaxis.get_ticklabels():
        label.set_rotation(-35) 
        label.set_horizontalalignment('left') 
    for i, label in enumerate(ax.yaxis.get_ticklabels()):
        # add 3 white space on the begining of name
        names[i] = "   %s" % names[i] 
        label.set_horizontalalignment("left")
        label.set_verticalalignment('center')

    p = mpl_Rectangle((0, 0), 1, 1, edgecolor=colors[0], 
            fc=colors[0]) 
    ax.plegend = ax.legend([p], ["Running"], loc="upper left",
            fancybox=True, shadow=True)

    ax.xaxis.grid(True, linestyle="-", which='major', color='black', alpha=0.7)
    ax.xaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: time_to_string(time)[:-7]))
    ax.set_xlim(xmin=0)
    ax.get_figure().tight_layout()

    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure, ylock=True)


def place_chart(names, values, title, xlabel, ylabel):

    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    ax = figure.add_subplot(111, projection=BasicChart.name)
    
    llines = []
    for line, name in enumerate(names):
        xvalues, yvalues = zip(*values[line])
        llines.append((name, xvalues, yvalues))

    # fill data
    lines = []
    for ldata in llines:
        name, xvalues, yvalues = ldata
        line = ax.plot(xvalues, yvalues,
                'o-', drawstyle='steps-post', label=name)
        lines.append(line)

    for label in ax.xaxis.get_ticklabels():
        label.set_rotation(-35) 
        label.set_horizontalalignment('left') 

    # set legend
    ax.plegend = ax.legend(loc="upper left", fancybox=True, shadow=True)
    ax.registr_pick_legend(ax.plegend, lines)
    ax.xaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: time_to_string(time)[:-7]))

    # set basic properties
    ax.set_xlim(xmin = 0)
    ax.get_figure().tight_layout()
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure)

#******************************************************************************
# TMP functions 

def time_to_string(nanosec):
    s = int(nanosec) / 1000000000
    nsec = nanosec % 1000000000
    sec = s % 60
    minutes = (s / 60) % 60
    hours = s / 60 / 60
    return "{0}:{1:0>2}:{2:0>2}:{3:0>9}".format(hours, minutes, sec, nsec)

def _register_new_types_charts():
    mpl_register_projection(BasicChart)
    mpl_register_projection(TimeChart)

_register_new_types_charts()

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
