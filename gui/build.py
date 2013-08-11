#
#    Copyright (C) 2010, 2011, 2013 Stanislav Bohm
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
import gtkutils

class BuildOptionsWidget(gtk.VBox):

    def __init__(self, project, app):
        gtk.VBox.__init__(self)
        self.project = project
        self.app = app

        hbox = gtk.HBox()

        self.pack_start(hbox, False, False)

        self.table = gtk.Table()
        self.table.set_border_width(5)
        self.table.set_row_spacings(5)
        self.table.set_col_spacings(5)

        options = [ o for o in self.project.build_options.keys()
                    if o != "OTHER_FILES"
                    if o != "USE_OCTAVE" ]
        for i, option in enumerate(options):
            self.add_line(i, option)

        self.pack_start(self.table, False, False)
        self.pack_start(self._octave(), False, False)

        self.filelist, controls = self._filelist()
        self.pack_start(controls, False, False)
        self.pack_start(self.filelist, True, True)

    def add_line(self, line, text):
        label = gtk.Label(text)
        label.set_alignment(1.0,0.5)
        entry = gtk.Entry()
        entry.set_text(self.project.get_build_option(text))
        entry.connect("changed", lambda w: self.project.set_build_option(text, w.get_text()))
        self.table.attach(label, 0, 1, line, line + 1, gtk.SHRINK | gtk.FILL)
        self.table.attach(entry, 1, 2, line, line + 1)

    def _filelist(self):
        filelist = gtkutils.SimpleList((("Filename", str),))
        hbox = gtk.HBox()

        button = gtk.Button("Add files")
        button.connect("clicked", self._add_files)
        hbox.pack_start(button, False, False)

        button = gtk.Button("Remove file")
        button.connect("clicked", self._remove_file)
        hbox.pack_start(button, False, False)

        if self.project.get_build_option("OTHER_FILES"):
            for filename in self.project.get_build_option("OTHER_FILES").split("\n"):
                filelist.append((filename,))

        return filelist, hbox

    def _add_files(self, w):
        dialog = gtk.FileChooserDialog("Add source files", self.get_toplevel(), gtk.FILE_CHOOSER_ACTION_OPEN,
                (gtk.STOCK_CANCEL, gtk.RESPONSE_CANCEL,
                 gtk.STOCK_OPEN, gtk.RESPONSE_OK))
        dialog.set_current_folder(self.project.get_directory())
        dialog.set_default_response(gtk.RESPONSE_OK)
        dialog.set_select_multiple(True)
        try:

            ffilter = gtk.FileFilter()
            ffilter.set_name("Source files")

            for ext in self.project.get_source_file_patterns():
                ffilter.add_pattern(ext)

            dialog.add_filter(ffilter)

            ffilter = gtk.FileFilter()
            ffilter.set_name("All files")
            ffilter.add_pattern("*")
            dialog.add_filter(ffilter)

            if dialog.run() == gtk.RESPONSE_OK:
                filenames = dialog.get_filenames()
                directory = self.project.get_directory()

                other_files = self.project.get_build_option("OTHER_FILES") \
                                .split("\n")

                for filename in filenames:
                    if not filename.startswith(directory):
                        self.app.show_error_dialog(
                            "File '{0}' is not in the project directory."
                            .format(filename))
                        return

                    f = filename[len(directory) + 1:]
                    if f in other_files:
                        continue
                    self.filelist.append((f,))
                self._update_project()

        finally:
            dialog.destroy()

    def _octave(self):
        button = gtk.CheckButton("Build with Octave C++ API")
        button.set_active(self.project.get_build_option("USE_OCTAVE") == "True")
        button.connect("clicked", lambda w: self.project.set_build_option("USE_OCTAVE", str(w.get_active())))
        return button

    def _remove_file(self, w):
        self.filelist.remove_selection()
        self._update_project()

    def _update_project(self):
        filenames = self.filelist.get_column(0)
        self.project.set_build_option("OTHER_FILES", "\n".join(filenames))
