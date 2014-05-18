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
import textview
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

    def close_all_tabs(self, groups=None, predicate_fn=None):
        for tab in self.tablist[:]:
            if ((not groups or set(tab.mainmenu_groups).intersection(set(groups))) and
                    (predicate_fn is None or predicate_fn(tab))):
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

        def add_menu(label):
            menu = gtk.Menu()
            item = gtk.MenuItem(label)
            item.set_submenu(menu)
            main_menu.append(item)
            return menu

        menu = add_menu("_Project")

        add("_New project", self.app.new_project)
        add("_Open project", self.app.load_project)
        add("I_mport project", self.app.import_project)
        add("_Save project", self.app.save_project, "project")
        add("Save project _as", self.app.save_project_as, "project")
        menu.append(gtk.SeparatorMenuItem())
        add("_Quit", gtk.main_quit)

        menu = add_menu("_View")

        item = gtk.RadioMenuItem(None, "No grid")
        item.connect("activate", lambda w: self.app.set_grid_size(1))
        item.set_active(True)
        menu.append(item)

        item = gtk.RadioMenuItem(item, "Small grid (6x6)")
        item.connect("activate", lambda w: self.app.set_grid_size(6))
        menu.append(item)

        item = gtk.RadioMenuItem(item, "Big grid (16x16)")
        item.connect("activate", lambda w: self.app.set_grid_size(16))
        menu.append(item)

        menu.append(gtk.SeparatorMenuItem())
        add("Hide error messages", self.app.hide_error_messages, "project")
        menu.append(gtk.SeparatorMenuItem())
        self.close_tab_item = add("Close tab", self.app.close_current_tab, key="W", ctrl=True)
        add("Close all simulation tabs",
            self.app.close_simulation_tabs,
            key="W",
            ctrl=True,
            shift=True)

        menu = add_menu("_Edit")

        add("Undo", self.app.undo, "undo", key="Z", ctrl=True)
        add("Redo", self.app.redo, "undo", key="Z", ctrl=True, shift=True)
        menu.append(gtk.SeparatorMenuItem())
        add("Edit _project config", self.app.project_config, "project")
        add("Edit _head code", self.app.edit_head, "project")
        add("Edit control se_qencies", self.app.edit_control_sequences, "project")
        add("Edit simulated run", self.app.edit_simrun, "project")
        add("Edit _tests", self.app.edit_code_tests, "project")
        menu.append(gtk.SeparatorMenuItem())
        add("Edit _settings", self.app.edit_settings)

        menu = add_menu("_Build")

        add("Build relea_se", lambda: self.app.build_project("release"), "project")
        add("Build _traced", lambda: self.app.build_project("traced"), "project")
        add("Build _statespace", lambda: self.app.build_project("statespace"), "project")
        add("Build s_imrun", lambda: self.app.build_project("simrun"), "project")
        menu.append(gtk.SeparatorMenuItem())
        add("Build library", lambda: self.app.build_project("lib"), "project")
        add("Build traced library", lambda: self.app.build_project("libtraced"), "project")

        menu = add_menu("_Simulation")

        add("_Run simulation", self.app.simulation_start, "project", key="F7")
        add("Confi_gure simulation", self.app.open_simconfig_dialog, "project", key="F8")
        menu.append(gtk.SeparatorMenuItem())
        add("Run _simulation in Valgrind",
            lambda: self.app.simulation_start(valgrind=True),
            "project")
        add("_Connect to application", self.app.connect_to_application)

        menu = add_menu("_Tools")

        add("_Run tool", self.app.run_tool_window)
        menu.append(gtk.SeparatorMenuItem())
        add("Run state space _analysis", self.app.run_statespace_analysis, "project")
        add("Open rep_ort", self.app.load_report)

        menu = add_menu("_Others")
        add("Save net as SV_G", self.app.save_as_svg, "screenshot")

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
        self.set_policy(gtk.POLICY_AUTOMATIC, gtk.POLICY_ALWAYS)
        self.set_shadow_type(gtk.SHADOW_IN)
        self.textview = textview.TextViewWithLinks()
        font_desc = pango.FontDescription('monospace')
        if font_desc:
            self.textview.modify_font(font_desc)
        self.textview.create_tag("output", foreground="blue")
        self.textview.create_tag("success", foreground="darkgreen")
        self.textview.create_tag("error", foreground="red")
        self.textview.create_tag("warn", foreground="chocolate")
        self.textview.create_tag("info", foreground="dark blue")
        self.add(self.textview)

    def scroll_to_end(self):
        i = self.textview.buffer.get_end_iter()
        # We move also with selection_bound otherwise click into console causes
        # causes unwanted selections
        mark = self.textview.buffer.get_selection_bound()
        self.textview.buffer.move_mark(mark, i)
        mark = self.textview.buffer.get_insert()
        self.textview.buffer.move_mark(mark, i)
        self.textview.scroll_to_mark(mark, 0)

    def write(self, text, tag_name="normal"):
        self.textview.write(text, tag_name)
        self.scroll_to_end()

    def write_link(self, text, callback):
        self.textview.write_link(text, callback)
        self.scroll_to_end()

    def reset(self):
        self.textview.reset()


class Tab:

    window = None

    def __init__(self,
                 name,
                 widget,
                 key=None,
                 mainmenu_groups=(),
                 has_close_button=True,
                 call_close=False):
        self.name = name
        self.widget = widget
        self.key = key
        self.mainmenu_groups = mainmenu_groups
        self.close_button = has_close_button
        self.call_close = call_close

    def get_widget(self):
        return self.widget

    def get_key(self):
        return self.key

    def get_name(self):
        return self.name

    def has_close_button(self):
        return self.close_button

    def close(self):
        if self.call_close:
            self.widget.close()
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
