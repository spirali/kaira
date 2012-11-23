#
#    Copyright (C) 2010, 2011 Stanislav Bohm
#                  2011       Ondrej Garncarz
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
import pango

class MainWindow(gtk.Window):

    def __init__(self, app):
        gtk.Window.__init__(self)
        self.app = app
        self.set_title("Kaira")
        self.connect("destroy", gtk.main_quit)
        self.tablist = []

        vbox = gtk.VBox()
        self.add(vbox)
        vbox.pack_start(self._create_main_menu(), False, False)

        paned = gtk.VPaned()
        vbox.pack_start(paned)
        self.notebook = gtk.Notebook()
        self.notebook.set_scrollable(True)
        paned.pack1(self.notebook, True)

        self.notebook.connect("switch-page", self._on_tab_switch)

        self.console = Console()
        self.console.set_size_request(300,100)
        paned.pack2(self.console, False)

        vbox.show_all()

    def add_tab(self, tab, switch=True):
        if tab.has_close_button():
            button = gtk.Button()
            button.set_relief(gtk.RELIEF_NONE)
            button.set_focus_on_click(False)
            icon = gtk.image_new_from_stock(gtk.STOCK_CLOSE, gtk.ICON_SIZE_MENU)

            style = gtk.RcStyle()
            style.xthickness = 0
            style.ythickness = 0
            button.modify_style(style)
            button.add(icon)
            button.connect("clicked", lambda w: tab.close())
            w = gtk.HBox(False, 0)
            w.pack_start(gtk.Label(tab.get_name()))
            w.pack_start(button, False, False)
            w.show_all()
        else:
            w = gtk.Label(tab.get_name())
        self.notebook.append_page(tab.get_widget(), w)
        self.notebook.set_tab_reorderable(tab.get_widget(), True)
        tab.get_widget().show()
        tab.window = self
        self.tablist.append(tab)
        if switch:
            self.switch_to_tab(tab)

    def foreach_tab(self, fn):
        for tab in self.tablist[:]:
            fn(tab)

    def switch_to_tab(self, tab):
        num = self.notebook.page_num(tab.get_widget())
        self.notebook.set_current_page(num)

    def switch_to_tab_by_key(self, key, fn=None):
        for tab in self.tablist:
            if tab.get_key() == key:
                self.switch_to_tab(tab)
                if fn:
                    fn(tab)
                return True
        return False

    def close_tab(self, tab):
        num = self.notebook.page_num(tab.get_widget())
        self.notebook.remove_page(num)
        self.tablist.remove(tab)

    def close_all_tabs(self):
        for tab in self.tablist[:]:
            tab.close()

    def get_current_tab(self):
        widget = self.notebook.get_nth_page(self.notebook.get_current_page())
        for tab in self.tablist:
            if tab.get_widget() == widget:
                return tab

    def _create_main_menu(self):
        ag = gtk.AccelGroup()
        self.add_accel_group(ag)
        menu = None
        main_menu = gtk.MenuBar()
        self.mainmenu_groups = {}

        def add_accelerator(item, key, ctrl=False, shift=False):
            mask = 0
            if ctrl:
                mask |= gtk.gdk.CONTROL_MASK
            if shift:
                mask |= gtk.gdk.SHIFT_MASK
            item.add_accelerator("activate",
                                 ag,
                                 gtk.gdk.keyval_from_name(key),
                                 mask,
                                 gtk.ACCEL_VISIBLE)

        def add(label,
                callback,
                event_group=None,
                key=None,
                ctrl=False,
                shift=False):
            item = gtk.MenuItem(label)
            item.connect("activate", lambda w: callback())
            menu.append(item)
            if key is not None:
                add_accelerator(item, key, ctrl, shift)
            if event_group is not None:
                self.mainmenu_groups.setdefault(event_group, [])
                self.mainmenu_groups[event_group].append(item)
            return item

        menu = gtk.Menu()
        item = gtk.MenuItem("_Project")
        item.set_submenu(menu)
        main_menu.append(item)

        add("_New project", self.app.new_project)
        add("_Open project", self.app.load_project)
        add("I_mport project", self.app.import_project)
        add("_Save project", self.app.save_project, "project")
        add("Save project _as", self.app.save_project, "project")
        menu.append(gtk.SeparatorMenuItem())
        add("_Quit", gtk.main_quit)

        menu = gtk.Menu()
        item = gtk.MenuItem("_View")
        item.set_submenu(menu)
        main_menu.append(item)

        item = gtk.RadioMenuItem(None, "No grid")
        item.connect("activate", lambda w: self.app.set_grid_size(1))
        item.set_active(True)
        menu.append(item)

        item = gtk.RadioMenuItem(item, "Small grid (5x5)")
        item.connect("activate", lambda w: self.app.set_grid_size(5))
        menu.append(item)

        item = gtk.RadioMenuItem(item, "Big grid (25x25)")
        item.connect("activate", lambda w: self.app.set_grid_size(25))
        menu.append(item)

        menu.append(gtk.SeparatorMenuItem())
        add("Hide error messages", self.app.hide_error_messages, "project")
        menu.append(gtk.SeparatorMenuItem())
        self.close_tab_item = add("Close tab", self.app.close_current_tab, key="W", ctrl=True)

        menu = gtk.Menu()
        item = gtk.MenuItem("_Edit")
        item.set_submenu(menu)
        main_menu.append(item)

        add("Undo", self.app.undo, "undo", key="Z", ctrl=True)
        add("Redo", self.app.redo, "undo", key="Z", ctrl=True, shift=True)
        menu.append(gtk.SeparatorMenuItem())
        add("Edit _project details", self.app.project_config, "project")
        add("Edit _head code", self.app.edit_head, "project")
        add("Edit _tests", self.app.edit_code_tests, "project")
        menu.append(gtk.SeparatorMenuItem())
        add("Edit _settings", self.app.edit_settings)

        menu = gtk.Menu()
        item = gtk.MenuItem("_Build")
        item.set_submenu(menu)
        main_menu.append(item)

        add("Build project (_relea_se)", lambda: self.app.build_project("release"), "project")
        add("Build project (_traced)", lambda: self.app.build_project("traced"), "project")
        add("Build project (_statespace)", lambda: self.app.build_project("statespace"), "project")

        menu = gtk.Menu()
        item = gtk.MenuItem("_Simulation")
        item.set_submenu(menu)
        main_menu.append(item)

        add("_Run simulation", self.app.simulation_start, "project", key="F7")
        add("Confi_gure simulation", self.app.open_simconfig_dialog, "project", key="F8")
        menu.append(gtk.SeparatorMenuItem())
        add("Run _simulation in Valgrind",
            lambda: self.app.simulation_start(valgrind=True),
            "project")

        menu = gtk.Menu()
        item = gtk.MenuItem("_Analysis")
        item.set_submenu(menu)
        main_menu.append(item)

        add("Open tracelo_g", self.app.load_tracelog)
        add("_Connect to application", self.app.connect_to_application)
        menu.append(gtk.SeparatorMenuItem())

        add("Run state space _analysis", self.app.run_statespace_analysis, "project")
        add("Open _report", self.app.load_report)

        return main_menu

    def _on_tab_switch(self, w, page, page_index):
        widget = self.notebook.get_nth_page(page_index)
        tab = None
        for t in self.tablist:
            if t.get_widget() == widget:
                tab = t
                break

        if tab is None:
            self.close_tab_item.set_sensitive(False)
            groups = ()
        else:
            self.close_tab_item.set_sensitive(tab.has_close_button())
            groups = tab.mainmenu_groups

        for group in self.mainmenu_groups:
             sensitive = group in groups
             for item in self.mainmenu_groups[group]:
                 item.set_sensitive(sensitive)


class Console(gtk.ScrolledWindow):

    def __init__(self):
        gtk.ScrolledWindow.__init__(self)
        self.id_counter = 0
        self.link_callbacks = {}
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
        self.set_shadow_type(gtk.SHADOW_IN)
        self.textview = gtk.TextView()
        self.textview.set_editable(False)
        self.textview.connect("button-press-event", self._button_down)
        self.textview.connect("motion_notify_event", self._mouse_move)
        font_desc = pango.FontDescription('monospace')
        if font_desc:
            self.textview.modify_font(font_desc)
        self.buffer = self.textview.get_buffer()
        self.buffer.create_tag("normal")
        self.buffer.create_tag("output", foreground="blue")
        self.buffer.create_tag("success", foreground="darkgreen")
        self.buffer.create_tag("error", foreground="red")

        # Do not use tag "link" directly in method write, use always method "write_link"
        self.link_tag = self.buffer.create_tag("link", underline=True)
        self.link_hidden_tag = self.buffer.create_tag("link_hidden", invisible = True)
        self.add(self.textview)

    def reset(self):
        self.buffer.set_text("")
        self.id_counter = 0
        self.link_callbacks = {}

    def write(self, text, tag_name="normal"):
        self.buffer.insert_with_tags_by_name(self.buffer.get_end_iter(), text, tag_name)
        self.textview.scroll_to_iter(self.buffer.get_end_iter(),0.0)

    def write_link(self, text, callback):
        new_id = str(self.id_counter)
        self.link_callbacks[new_id] = callback
        self.id_counter += 1
        self.write(text, "link")
        self.write(new_id, "link_hidden")

    def _iter_at_position(self, px, py):
        px, py = self.textview.window_to_buffer_coords(gtk.TEXT_WINDOW_WIDGET, int(px), int(py))
        return self.textview.get_iter_at_location(px, py)

    def _button_down(self, w, event):
        i = self._iter_at_position(event.x, event.y)
        if i.has_tag(self.link_tag):
            i.forward_to_tag_toggle(self.link_tag)
            j = i.copy()
            j.forward_to_tag_toggle(self.link_hidden_tag)
            self.link_callbacks[self.buffer.get_text(i, j, True)]()
            return True
        else:
            return False

    def _mouse_move(self, w, event):
        i = self._iter_at_position(event.x, event.y)
        if i.has_tag(self.link_tag):
            cursor = gtk.gdk.Cursor(gtk.gdk.FLEUR)
        else:
            cursor = None
        w = self.textview.get_window(gtk.TEXT_WINDOW_TEXT)
        w.set_cursor(cursor)


class Tab:

    window = None

    def __init__(self,
                 name,
                 widget,
                 key=None,
                 mainmenu_groups=(),
                 has_close_button=True):
        self.name = name
        self.widget = widget
        self.key = key
        self.mainmenu_groups = mainmenu_groups
        self.close_button = has_close_button

    def get_widget(self):
        return self.widget

    def get_key(self):
        return self.key

    def get_name(self):
        return self.name

    def has_close_button(self):
        return self.close_button

    def close(self):
        self.window.close_tab(self)

    def project_save(self):
        pass

    def project_export(self):
        pass


class SaveTab(Tab):

    def project_save(self):
        self.widget.save()

    def project_export(self):
        self.widget.save()

    def close(self):
        self.widget.save()
        Tab.close(self)
