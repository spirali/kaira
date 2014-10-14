import gtksourceview2 as gtksource
import gtk
import pango

class Buffer(gtksource.Buffer):

    def __init__(self):
        gtksource.Buffer.__init__(self)
        self.errortag = gtk.TextTag("error")
        #self.errortag.set_property("background","red")
        #self.errortag.set_property("background-full-height",True)
        self.errortag.set_property("underline-set",True)
        self.errortag.set_property("underline",pango.UNDERLINE_ERROR)
        self.get_property("tag_table").add(self.errortag)

    def getAllText(self):
        return self.get_text(self.get_start_iter(),self.get_end_iter())

    def setTagAt(self,tagname,line,startcol,endcol):
        iter = self.get_iter_at_line(line)
        iter.set_line_offset(startcol)

        enditer = iter.copy()
        if endcol is not None:
            if endcol >= startcol:
                enditer.set_line_offset(endcol)
        else:
            enditer.forward_visible_word_ends(1)
        print "char",iter.get_char(),"-",enditer.get_char()

        self.apply_tag_by_name(tagname,iter,enditer)

    def getCursorPosition(self):
        return self.get_property("cursor-position")

    def getLineAndColOfCursor(self):
        position = self.getCursorPosition()
        iter = self.get_iter_at_offset(position)
        line = iter.get_line()+1
        pos = iter.get_line_offset()+1
        return (line,pos)
