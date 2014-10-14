import gtksourceview2 as gtksource
import gtk

class View(gtksource.View):

    def __init__(self,buffer = None):
        gtksource.View.__init__(self,buffer)
        self.buffer = buffer
        self.codeCompletion = None
        self.set_property("highlight-current-line",True)
        self.set_wrap_mode(gtk.WRAP_CHAR)
