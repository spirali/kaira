import os
import sys
import subprocess
import shutil

PATH = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(PATH)

class DataTree:

    def __init__(self):
        self.data = {}
        self.datapointer = self.data

    def pick(self, key):
        if key not in self.datapointer:
            self.datapointer[key] = {}
        self.datapointer = self.datapointer[key]

    def set(self, file):
        if "data" not in self.datapointer:
            self.datapointer["data"] = (0, 0, 0)
        data = self.datapointer["data"]
        for line in open(file, "r"):
            if "total verification time" in line:
                time = float(line.split(":")[1])
            if "total number of explored states" in line:
                states = float(line.split(":")[1])
        self.datapointer["data"] = (data[0] + time, data[1] + states, data[2] + 1)
        self.datapointer = self.data

    def get(self):
        data = self.datapointer["data"]
        data = (data[0] / data[2], data[1] / data[2])
        self.datapointer = self.data
        return data

if __name__ == '__main__':

    params = {}
    values = DataTree()

    for output in os.listdir(PATH):
        if os.path.isfile(os.path.join(PATH, output)) and output.endswith(".out"):
            if len(output.split(".")) != len(sys.argv) + 1:
                print "Invalid number of parameters"
                exit()
            break

    for param in sys.argv[1:]:
        params[param.split("=")[0]] = []

    for output in os.listdir(PATH):
        if os.path.isfile(os.path.join(PATH, output)) and output.endswith(".out"):
            for param in output.split(".")[:-2]:
                name = "_".join(param.split("_")[:-1])
                value = param.split("_")[-1]
                values.pick(value)
                if name in params and value not in params[name]:
                    params[name].append(value)

            values.set(output)

    for param in params:
        if all(v.replace('.', '', 1).isdigit() for v in params[param]):
            params[param].sort(key=lambda x: float(x))
        else:
            params[param].sort()

    table = []
    row = column = -1
    for param in sys.argv[1:]:
        name, value = param.split("=")
        if value == "*":
            if row == -1:
                row = len(table)
            else:
                column = len(table)
            table.append((name, params[name]))
        else:
            table.append((name, [ value ]))

    name = ".".join("_".join([name, value[0]]) for (name, value) in table if len(value) == 1)
    result = open(name + ".tab", "w")
    if column == -1:
        result.write("{0:^10}".format(table[row][0]))
        for r in range(len(table[row][1])):
            for i, (name, value) in enumerate(table):
                if i == row:
                    values.pick(value[r])
                else:
                    values.pick(value[0])

            time, states = values.get()
            result.write("{0:^10} {1}\n".format(table[row][1][r], time))
    else:
        result.write("{0:>10} / {1:10}".format(table[row][0], table[column][0]))
        for c in range(len(table[column][1])):
            result.write("{0:^10}".format(table[column][1][c]))
        result.write("\n")
        for r in range(len(table[row][1])):
            result.write("{0:^20}".format(table[row][1][r]))
            for c in range(len(table[column][1])):
                for i, (name, value) in enumerate(table):
                    if i == row:
                        values.pick(value[r])
                    elif i == column:
                        values.pick(value[c])
                    else:
                        values.pick(value[0])

                time, states = values.get()
                result.write("{0:10}".format(time))
            result.write("\n")

