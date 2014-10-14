import gtksourceview2 as gtksource

class ProposalItem(gtksource.CompletionItem):

    def __init__(self,label,typedtext,icon):
        gtksource.CompletionItem.__init__(self, label, typedtext, icon)
        self.placeholders = None
        self.infotext = ""

    def setPlaceholders(self,list):
        self.placeholders = list

    def getPlaceholders(self):
        return self.placeholders

    def setInfoText(self,text):
        self.infotext = text

    def getInfoText(self):
        return self.infotext
