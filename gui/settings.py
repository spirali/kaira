
import gtk

class SettingsWidget(gtk.Notebook):

    def __init__(self, app):
        gtk.Notebook.__init__(self)
        self.app = app
        self.set_tab_pos(gtk.POS_LEFT)
        self.append_page(self._generic_settings(), gtk.Label("Generic"))
        self.append_page(self._completion_settings(), gtk.Label("Completion"))
        self.show_all()

    def _generic_settings(self):
        def set(section, name, value):
            self.app.settings.set(section, name, str(value))
            self.app.save_settings()

        def settings_button(section, name, descritpion):
            button = gtk.CheckButton(descritpion)
            button.set_active(self.app.settings.getboolean(section, name))
            button.connect("toggled",
                lambda w: set(section, name, w.get_active()))
            vbox.pack_start(button, False, False)

        vbox = gtk.VBox()
        settings_button("main", "save-before-build", "Save project before build")
        settings_button("main", "ptp-debug", "PTP debugging")
        return vbox

    def _completion_settings(self):
        def set(section, name, value):
            self.app.settings.set(section, name, str(value))
            self.app.save_settings()
            
        def add_check_button(section, name, description):
            button = gtk.CheckButton(description)
            button.set_active(self.app.settings.getboolean(section,name))
            button.connect("toggled",
                           lambda w: set(section, name, button.get_active()))
            vbox.pack_start(button, False, False)

        def add_spin_box(section,name,description, numeric = False, digits = 1, range = (1, 12)):
            hbox = gtk.HBox()
            spin = gtk.SpinButton()
            spin.set_digits(digits)
            spin.set_increments(1,2)
            spin.set_range(range[0], range[1])
            spin.set_numeric(numeric)
            spin.set_value(self.app.settings.getfloat("code_completion", name))
            spin.connect("value-changed", lambda w: set(section, name, str(spin.get_value())))
            hbox.pack_start(gtk.Label(description), False, False)
            hbox.pack_start(spin, False, False)
            vbox.pack_start(hbox, False, False)
            
        vbox = gtk.VBox()
        add_check_button("code_completion","enable_highlight_current_line","Highlight current line")
        add_check_button("code_completion", "enable_show_line_numbers", "Show line numbers")
        add_spin_box("code_completion", "tab_width", "Tab size", numeric = True)
        add_check_button("code_completion","enable_info_box","Show info box")
        add_spin_box("code_completion", "delay_info_box", "Delay for info box in ms",
                     numeric = True,
                     digits = 0,
                     range = (0, 3000))
        return vbox