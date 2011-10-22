#!/usr/bin/env python
import sys
import base.project as project
from gen.codegen import Codegen

def main(args):
    if len(args) == 0:
        print "ptp <project.xml> <action>"
    p = project.load_project_from_file(args[0])
    if len(args) == 3 and args[1] == "--build":
        gen = Codegen(p)
        gen.build().write_to_file(args[2])
    
if __name__ == '__main__':
    main(sys.argv[1:])
