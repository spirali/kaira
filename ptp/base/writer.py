
class Writer(object):

    def __init__(self):
        self.lines = []
        self.indent = ""

    def raw_line(self, string):
        self.lines.append(self.indent + string)

    def emptyline(self):
        self.lines.append("")

    def line(self, string, *args, **kw):
        self.raw_line(string.format(*args, **kw))

    def indent_push(self):
        self.indent += "\t"

    def indent_pop(self):
        self.indent = self.indent[:-1]

    def add_writer(self, writer):
        writer.write_to_writer(self)

    def raw_text(self, text):
        if len(text) == 0:
            return
        lines = text.split("\n")
        if text[-1] == "\n":
            lines = lines[:-1]
        for line in lines:
            self.raw_line(line)

    def get_string(self):
        return "\n".join(self.lines) + "\n"

    def write_to_file(self, filename):
        with open(filename, "w") as f:
            for line in self.lines:
                f.write(line + "\n")

    def write_to_writer(self, writer):
        for line in self.lines:
            writer.raw_line(line)
