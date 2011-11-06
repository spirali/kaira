#!/usr/bin/env python
import sys
import base.project as project
import base.utils as utils
from gencpp.builder import Builder
from gencpp.builder import get_place_user_fn_header, get_transition_user_fn_header


def main(args):

    p = project.load_project_from_file(args[0])

    if len(args) == 3 and args[1] == "--build":
        builder = Builder(p)
        builder.build()
        builder.write_to_file(args[2])
        return

    if len(args) == 3 and args[1] == "--place-user-fn":
        place = p.get_place(int(args[2]))
        print get_place_user_fn_header(p, place),
        return

    if len(args) == 3 and args[1] == "--transition-user-fn":
        transition = p.get_transition(int(args[2]))
        print get_transition_user_fn_header(p, transition),
        return

    print "Usage: ptp <project.xml> <action>"

if __name__ == '__main__':
    try:
        main(sys.argv[1:])
    except utils.PtpException, e:
        print e
        sys.exit(1)
