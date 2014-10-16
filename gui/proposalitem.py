import gtksourceview2 as gtksource

class ProposalItem(gtksource.CompletionItem):

    def __init__(self, label, typedtext, icon):
        gtksource.CompletionItem.__init__(self, label, typedtext, icon)
        self.placeholders = None
        self.infotext = ""

    def set_placeholders(self, list):
        self.placeholders = list

    def get_placeholders(self):
        return self.placeholders

    def set_info_text(self, text):
        self.infotext = text

    def get_info_text(self):
        return self.infotext