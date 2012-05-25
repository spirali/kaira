
import gtk

class SettingsWidget(gtk.Notebook):

    def __init__(self, app):
        gtk.Notebook.__init__(self)
        self.app = app
        self.set_tab_pos(gtk.POS_LEFT)
        self.append_page(self._generic_settings(), gtk.Label("Generic"))
        self.show_all()

    def _generic_settings(self):
        def settings_button(value, descritpion):
            button = gtk.CheckButton(descritpion)
            button.set_active(self.app.get_settings(value))
            button.connect("toggled",
                lambda w: self.app.set_settings(value, w.get_active()))
            vbox.pack_start(button, False, False)

        vbox = gtk.VBox()
        settings_button("save-before-build", "Save project before build")
        settings_button("ptp-debug", "PTP debugging")
        return vbox
