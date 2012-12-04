
import gtk

class SettingsWidget(gtk.Notebook):

    def __init__(self, app):
        gtk.Notebook.__init__(self)
        self.app = app
        self.set_tab_pos(gtk.POS_LEFT)
        self.append_page(self._generic_settings(), gtk.Label("Generic"))
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
