import gtksourceview2 as gtksource
import gtk
import pango

class Buffer(gtksource.Buffer):

    def __init__(self):
        gtksource.Buffer.__init__(self)
        self.errortag = gtk.TextTag("error")
        self.errortag.set_property("underline-set",True)
        self.errortag.set_property("underline",pango.UNDERLINE_ERROR)
        self.get_property("tag_table").add(self.errortag)

    def get_all_text(self):
        return self.get_text(self.get_start_iter(),self.get_end_iter())

    def set_tag_at(self, tagname, line, startcol, endcol):
        iter = self.get_iter_at_line(line)
        iter.set_line_offset(startcol)

        enditer = iter.copy()
        if endcol is not None:
            if endcol >= startcol:
                enditer.set_line_offset(endcol)
        else:
            enditer.forward_visible_word_ends(1)
        self.apply_tag_by_name(tagname,iter,enditer)

    def get_cursor_position(self):
        return self.get_property("cursor-position")

    def get_cursor_position_tuple(self):
        position = self.get_cursor_position()
        iter = self.get_iter_at_offset(position)
        line = iter.get_line()+1
        pos = iter.get_line_offset()+1
        return (line,pos)
