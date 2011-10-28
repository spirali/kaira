#!/usr/bin/env python
import sys
import base.project as project
from gen.codegen import Codegen

def main(args):

    p = project.load_project_from_file(args[0])
    if len(args) == 3 and args[1] == "--build":
        gen = Codegen(p)
        gen.build().write_to_file(args[2])
        return
    if len(args) == 3 and args[1] == "--place-type":
        place = p.get_place(int(args[2]))
        gen = Codegen(p)
        print gen.get_place_user_fn_header(place)
        return

    print "Usage: ptp <project.xml> <action>"

if __name__ == '__main__':
    main(sys.argv[1:])
