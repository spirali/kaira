#
#    Copyright (C) 2012 Martin Surkovsky,
#                       Stanislav Bohm
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


import gtk
import os
import paths
import utils
import events as evt
import matplotlib.cm as cm
from matplotlib.axes        import Axes as mpl_Axes
from matplotlib.container   import Container as mpl_Container
from matplotlib.artist      import Artist as mpl_Artist
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

class LineConfig:

    """ Information about concrete line in a chart.
    Keyword arguments:
    mpl_line1 -- an instance of matplotlib.lines.Line2D, which is used in
                 the original chart.
    x_values  -- x-values of the line.
    y_values  -- y-values of the line.
    color     -- used color.
    """

    def __init__(
            self,
            mpl_line1,
            x_values,
            y_values,
            color):

        self.mpl_line1 = mpl_line1
        self.x_values = x_values
        self.y_values = y_values
        self.color = color

        # instance of line used in legend
        self.mpl_legline = None
        # alternative view of line1. If they are same, then line2 = line1.
        self.mpl_line2 = None
        # line1 is default visible
        self.mpl_line1_visible = True
        # line2 is default imvisible. It is switching between them.
        self.mpl_line2_visible = False

    def get_mpl_line1(self):
        return self.mpl_line1

    def copy_mpl_line1(self):
        # TODO: make more general. Not allways is first line of this type!
        l = mpl_Line(
            self.x_values, self.y_values, marker='o',
            drawstyle='steps-post', color=self.color)
        return l

    def get_x_values(self):
        return self.x_values

    def get_y_values(self):
        return self.y_values

    def get_color(self):
        return self.color

    def get_mpl_legline(self):
        return self.mpl_legline

    def set_mpl_legline(self, mpl_legline):
        self.mpl_legline = mpl_legline

    def get_mpl_line2(self):
        return self.mpl_line2

    def set_mpl_line2(self, mpl_line2):
        self.mpl_line2 = mpl_line2

    def get_mpl_line1_visible(self):
        return self.mpl_line1_visible

    def set_mpl_line1_visible(self, visible):
        self.mpl_line1_visible = visible
        self.__set_visible(visible, self.mpl_line1)

    def get_mpl_line2_visible(self):
        return self.mpl_line2_visible;

    def set_mpl_line2_visible(self, visible):
        if self.mpl_line2 is not None:
            self.mpl_line2_visible = visible
            self.__set_visible(visible, self.mpl_line2)
        else:
            raise Exception("Line 2 is not created!")

    def __set_visible(self, visible, line):
        if line is None:
            raise Exception("Line is None!")

        if isinstance(line, mpl_Artist):
            line.set_visible(visible)
        elif isinstance(line, mpl_Container):
            for child in line.get_children():
                self.__set_visible(visible, child)

class DrawLinesConfig:

    ''' The class which stores information about how to show selected lines.'''
    def __init__(self, lines_config):
        self.lines_config = lines_config
        self.count_of_changes = 0

    def change_lines_config(self, change, change_legline_fn=utils.empty_fn):

        old_count_of_changes = self.count_of_changes

        if not change.get_mpl_line2_visible():
            self.count_of_changes += 1
            change_legline_fn(change.get_mpl_legline(), 'on')
        else:
            self.count_of_changes -= 1
            change_legline_fn(change.get_mpl_legline(), 'off')

        # all lines will be shown (default state)
        if old_count_of_changes == 1 and self.count_of_changes == 0:
            for line_config in self.lines_config:
                line_config.set_mpl_line1_visible(True)
                legline = line_config.get_mpl_legline()
                change_legline_fn(legline, 'original')
                if line_config.get_mpl_line2() is not None:
                    line_config.set_mpl_line2_visible(False)

        # one selected lines will be shown and other lines will be hide
        elif old_count_of_changes == 0 and self.count_of_changes == 1:
            for line_config in self.lines_config:
                line_config.set_mpl_line1_visible(False)
                legline = line_config.get_mpl_legline()
                change_legline_fn(legline, 'off')
            change_legline_fn(change.get_mpl_legline(), 'on')

        # will be shown next selected line
        if self.count_of_changes > 0:
            if change.get_mpl_line2() is not None:
                change.set_mpl_line2_visible(not change.get_mpl_line2_visible())
            else:
                change.set_mpl_line2('create_new')

class BasicChart(mpl_Axes, evt.EventSource):

    name = 'basic_chart'

    def __init__(self,
                 fig,
                 rec,
                 axisbg=None, # defaults to rc axes.facecolor
                 frameon=True,
                 sharex=None,
                 sharey=None,
                 label="",
                 xscale=None,
                 yscale=None,
                 **kwargs):

        mpl_Axes.__init__(
            self, fig, rec, axisbg, frameon, sharex, sharey, label,
            xscale, yscale, **kwargs)
        evt.EventSource.__init__(self)

        # zoom properties
        self.zoom_stack = []
        self.zoom_rect = None
        # move properties
        self.xypress = None
        self.original_view_dim = None
        # legend
        self.plegend = None
        self.mouse_on_legend = False

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
        # register event which stop is drawing cross if it's cursorn over legend
        fig.canvas.mpl_connect("motion_notify_event", self._mouse_over_legend)

    def __convert_axes_to_data(self, x, y):
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

        def crop_coordinate(c_axes, flag):
            """
                It crops the coordinate and returns two values:
                    - coordinate in axes format <0, 1>
                    - coordinate in data format

                c_axes -- cordinate in axes format
                flag -- flag says which coordinate is required, possible
                        values: 'x' or 'y'.
                returns (<0,1>, <min_value, max_value>)
            """
            idx = 0 if flag == 'x' else 1
            if c_axes < 0:
                return (0, self.__convert_axes_to_data(0, 0)[idx])
            elif c_axes > 1:
                return (1, self.__convert_axes_to_data(1, 1)[idx])
            else:
                return (c_axes,
                        self.__convert_axes_to_data(c_axes, c_axes)[idx])

        def format_value(value, formatter):
            if value is not None and isinstance(formatter, mpl_FuncFormatter):
                return formatter.format_data(value)
            else:
                return str(value)

        if not self.mouse_on_legend and \
                (self.xypress is None or select_bg is not None):

            if self.cross_bg is None:
                self.cross_bg = self.figure.canvas.copy_from_bbox(self.bbox)

            if select_bg is not None:
                self.figure.canvas.restore_region(select_bg)
            elif self.cross_bg is not None:
                self.figure.canvas.restore_region(self.cross_bg)

            inv = self.transAxes.inverted()
            x, y = inv.transform((event.x, event.y))
            (x, xdata) = crop_coordinate(x, 'x')
            (y, ydata) = crop_coordinate(y, 'y')

            xtext = format_value(xdata, self.xaxis.get_major_formatter())
            ytext = format_value(ydata, self.yaxis.get_major_formatter())

            # the coefficient 7 is good result from an experiment :)
            xtext_pos = -7 * len(xtext) - 10 if x > 0.5 else 10
            ytext_pos = -20 if y > 0.5 else 30

            if not self.xlock:
                l1 = mpl_Line(
                    [x, x], [0, 1], c="#ff0000", lw=1,
                    transform=self.transAxes, figure=self.figure)
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
                l2 = mpl_Line(
                   [0, 1], [y, y], c="#ff0000", lw=1,
                   transform=self.transAxes, figure=self.figure)
                self.draw_artist(l2)

                if self.xlock:
                    ytext_pos = -20 if y > 0.5 else 10
                else:
                    ytext_pos -= 20
                a2 = mpl_Annotation(
                    ytext,
                    xy=(x, y), xycoords='axes fraction',
                    xytext=(xtext_pos, ytext_pos),
                    textcoords='offset points',
                    bbox=dict(boxstyle="round", fc="#ffff00"))
                a2.set_transform(mpl_IdentityTransform())
                self._set_artist_props(a2)
                self.draw_artist(a2)

            self.figure.canvas.blit(self.bbox)

    def _draw_rectangle(self, event):
        if not self.moving_flag \
                and not self.mouse_on_legend \
                and self.xypress is not None:

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
                ax_x_start = self.__convert_axes_to_data(0, 0)[0]
                ax_x_end = self.__convert_axes_to_data(1, 1)[0]

            if self.ylock:
                ax_y_start = self.__convert_axes_to_data(0, 0)[1]
                ax_y_end = self.__convert_axes_to_data(1, 1)[1]

            self.zoom_rect = (
                min(ax_x_start, ax_x_end),
                min(ax_y_start, ax_y_end),
                max(ax_x_start, ax_x_end),
                max(ax_y_start, ax_y_end))

            rec = mpl_Rectangle(
                (ax_x_start, ax_y_start),
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

            if xmin is not None and xmax is not None and \
                    ymin is not None and ymax is not None:
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

        if self.moving_flag and self.xypress is not None:
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
                event.guiEvent.keyval == gtk.keysyms.Control_L:
            self.set_xlock(not self.xlock)
            if event.x is not None and event.y is not None:
                self._draw_cross(event)

    def _switch_ylock_action(self, event):
        if not self.moving_flag and \
                event.guiEvent.keyval == gtk.keysyms.Shift_L:
            self.set_ylock(not self.ylock)
            if event.x is not None and event.y is not None:
                self._draw_cross(event)

    def _switch_moving_flag_action(self, event):
        if event.key == 'm':
            self.set_moving_flag(not self.moving_flag)

    def _mouse_over_legend(self, event):
        if self.plegend is not None and self.plegend.get_visible():
            bbox = self.plegend.get_frame()
            x, y = bbox.get_x(), bbox.get_y()
            width, height = bbox.get_width(), bbox.get_height()
            if event.x >= x and event.x <= x + width and \
                    event.y >= y and event.y <= y + height:
                if self.cross_bg is not None:
                    self.figure.canvas.restore_region(self.cross_bg)
                    self.figure.canvas.blit(self.bbox)

                self.mouse_on_legend = True
            else:
                self.mouse_on_legend = False

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

    def register_pick_legend(self, legend, lines_config):

        def change_legline_fn(legline, action='original'):

            if action == 'original' or action == 'on':
                legline.set_alpha(1.0)
                legline._legmarker.set_alpha(1.0)
            elif action == 'off':
                legline.set_alpha(0.3)
                legline._legmarker.set_alpha(0.3)
            else:
                raise Exception(
                    'Unexpected parameter \'which_use={0}\''.format(action))

        lined = dict()
        for legline, line_config in zip(legend.get_lines(), lines_config):
            legline.set_picker(5)
            lined[legline] = line_config
            line_config.set_mpl_legline(legline)

        dlc = DrawLinesConfig(lines_config)
        def on_pick(event):
            legline = event.artist
            line_config = lined[legline]
            dlc.change_lines_config(line_config, change_legline_fn)

            if line_config.get_mpl_line2() == 'create_new':
                line2 = line_config.copy_mpl_line1()
                self.add_line(line2)
                line_config.set_mpl_line2(line2)
                line_config.set_mpl_line2_visible(True)
            self.figure.canvas.draw_idle()

        self.figure.canvas.mpl_connect('pick_event', on_pick)

class TimeChart(BasicChart):

    '''This chart is connect to replay. It's realize through  the
    'x or y (time) axis'. It's important so that the axis of time corresponds
    with the replay slider!'''

    name = 'time_chart'

    def __init__(self,
                 fig,
                 rec,
                 axisbg=None, # defaults to rc axes.facecolor
                 frameon=True,
                 sharex=None,
                 sharey=None,
                 label="",
                 xscale=None,
                 yscale=None,
                 **kwargs):

        self.__init__(
            self, fig, rec, axisbg, frameon, sharex, sharey, label,
            xscale, yscale, kwargs)

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
        ax = figure.gca()

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
        btn_restore.connect(
            "clicked",
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
        ax.set_callback(
            "xlock_changed", lambda xlock: btn_xlock.set_active(xlock))
        toolbar.add(btn_xlock)

        icon_ylock = gtk.image_new_from_file(
            os.path.join(paths.ICONS_DIR, "ylock.svg"))
        btn_ylock = gtk.ToggleToolButton()
        btn_ylock.set_icon_widget(icon_ylock)
        btn_ylock.set_tooltip_text("Lock Y-axis (keep CTRL)")
        btn_ylock.connect("toggled", self._btn_ylock_action)
        ax.set_callback(
            "ylock_changed", lambda ylock: btn_ylock.set_active(ylock))
        toolbar.add(btn_ylock)

        icon_moving = gtk.image_new_from_file(
            os.path.join(paths.ICONS_DIR, "moving.svg"))
        btn_moving = gtk.ToggleToolButton()
        btn_moving.set_icon_widget(icon_moving)
        btn_moving.set_tooltip_text("Catch canvas (press key 'm')")
        btn_moving.connect("toggled", self._btn_moving_action)
        ax.set_callback(
            "moving_flag_changed",
            lambda moving_flag: self._moving_flag_changed(
                moving_flag, btn_moving, btn_xlock, btn_ylock))
        toolbar.add(btn_moving)

        return toolbar

    def _moving_flag_changed(
            self, moving_flag, btn_moving, btn_xlock, btn_ylock):
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
        dialog = gtk.FileChooserDialog(
            "Save graph", None, gtk.FILE_CHOOSER_ACTION_SAVE,
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

def _empty_chart(title="", xlabel="", ylabel=""):
    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    ax = figure.add_subplot(111, projection=BasicChart.name)

    ax.text(0.5, 0.5, 'No measured data.', color='#aa0000', fontsize=36,
        ha='center', va='center', alpha=1.0, transform=ax.transAxes)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure)

def _register_histogram_pick_legend(ax, legend, lines_config):

    def change_legline_fn(legline, action='orig'):
        if action == 'original':
            legline.set_linewidth(1.0)
            legline.set_alpha(1.0)
        elif action == 'on':
            legline.set_linewidth(6.0)
            legline.set_alpha(1.0)
        elif action == 'off':
            legline.set_linewidth(6.0)
            legline.set_alpha(0.3)
        else:
            raise Exception('Unexpected parameter \'{0}\''.format(action))

    lined = dict()

    for legline, line_config in zip(legend.get_lines(), lines_config):
        legline.set_picker(5)
        lined[legline] = line_config
        line_config.set_mpl_legline(legline)

    dlc = DrawLinesConfig(lines_config)
    def on_pick(event):
        legline = event.artist
        line_config = lined[legline]
        dlc.change_lines_config(line_config, change_legline_fn)
        if line_config.get_mpl_line2() == 'create_new':
            bar = ax.bar(line_config.get_x_values(), line_config.get_y_values(),
                    color=line_config.get_color(), alpha=0.6)
            line_config.set_mpl_line2(bar)
            line_config.set_mpl_line2_visible(True)
        ax.figure.canvas.draw_idle()

    ax.figure.canvas.mpl_connect('pick_event', on_pick)

def histogram(names, values, title="", xlabel="", ylabel=""):

    if not names or not values:
        return _empty_chart(title, xlabel, ylabel)

    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    ax = figure.add_subplot(111, projection=BasicChart.name)

    colors = [cm.hsv(float(i)/len(values)) for i in xrange(len(values))]
    n, bins, patches = ax.hist(
        values, 10, normed=0, histtype="bar", label=names, color=colors)

    for label in ax.xaxis.get_ticklabels():
        label.set_rotation(-35)
        label.set_horizontalalignment('left')

    ax.plegend = ax.legend(loc="upper right", fancybox=True, shadow=True)

    ax.xaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: utils.time_to_string(time)[:-7]))
    ax.set_xlim(xmin=0)
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure, xlock=True)

def utilization_chart(names,
                      values,
                      title="",
                      xlabel="",
                      ylabel="",
                      idles=None):

    if not names or not values:
        return _empty_chart(title, xlabel, ylabel)

    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    # TODO: Change it to TimeChart
    ax = figure.add_subplot(111, projection=BasicChart.name)

    ywidth = 2
    yticks = []

    if idles is not None:
        for i, lidle in enumerate(idles):
            y = ((i+1) * ywidth) + (i+1)
            ax.broken_barh(
                lidle, (y, ywidth),
                edgecolor='face', facecolor='#EAA769')

    for i, ldata in enumerate(values):
        y = (ywidth+1) * (i+ 1)
        yticks.append(y + ywidth/2)
        ax.broken_barh(
            ldata, (y, ywidth),
            edgecolor='face', facecolor='green')

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

    p = mpl_Rectangle((0, 0), 1, 1, edgecolor='green', fc='green', alpha=0.75)
    if idles is not None:
        idle_leg = mpl_Rectangle((0,0), 1, 1, edgecolor='#eaa769', fc='#eaa769', alpha=0.75)
        ax.plegend = ax.legend(
            [p,idle_leg], ["Running", "Idle"], loc="upper left", fancybox=True, shadow=True)
    else:
        ax.plegend = ax.legend(
            [p], ["Running"], loc="upper left", fancybox=True, shadow=True)

    ax.xaxis.grid(True, linestyle="-", which='major', color='black', alpha=0.7)
    ax.xaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: utils.time_to_string(time)[:-7]))
    ax.set_xlim(xmin=0)
    ax.get_figure().tight_layout()

    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    # resize figure
    w, h = figure.get_size_inches()
    figure.set_size_inches(w, len(values) * 0.4)
    return ChartWidget(figure, ylock=True)

def place_chart(names, values, title="", xlabel="", ylabel=""):

    if not names or not values:
        return _empty_chart(title, xlabel, ylabel)

    figure = mpl_Figure()
    canvas = mpl_FigureCanvas(figure)
    figure.set_canvas(canvas)

    ax = figure.add_subplot(111, projection=BasicChart.name)

    # fill data
    lines_config = []
    for i, (xvalues, yvalues) in enumerate(values):
        line, = ax.plot(
            xvalues, yvalues, 'o-', drawstyle="steps-post", label=names[i])
        lines_config.append(
            LineConfig(line, xvalues, yvalues, line.get_color()))

    for label in ax.xaxis.get_ticklabels():
        label.set_rotation(-35)
        label.set_horizontalalignment('left')

    # set legend
    ax.plegend = ax.legend(loc="upper left", fancybox=True, shadow=True)
    ax.register_pick_legend(ax.plegend, lines_config)
    ax.xaxis.set_major_formatter(mpl_FuncFormatter(
        lambda time, pos: utils.time_to_string(time)[:-7]))

    # set basic properties
    ax.set_xlim(xmin = 0)
    ax.get_figure().tight_layout()
    ax.set_title(title)
    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)

    return ChartWidget(figure)

def _register_new_types_charts():
    mpl_register_projection(BasicChart)
    mpl_register_projection(TimeChart)

_register_new_types_charts()
