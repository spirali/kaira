#!/usr/bin/env python
import sys
import base.project as project
from base.utils import PtpException
from gencpp.generator import CppProgramGenerator, CppLibGenerator
import traceback

debug_mode = False

generators = {
    "C++" : CppProgramGenerator,
    "C++ library" : CppLibGenerator
}

def get_generator(project):
    g = generators.get(project.get_extenv())
    if g is None:
        raise PtpException("Unknown extenv '{0}'".format(project.get_extenv()))
    else:
        return g(project)

def get_generator_from_xml(element):
    return get_generator(project.load_project(element))

def main(args):

    if args[0] == "--debug":
        global debug_mode
        debug_mode = True
        del args[0]

    p = project.load_project_from_file(args[0])
    generator = get_generator(p)

    if len(args) == 3 and args[1] == "--build":
        generator.build(args[2])
        return

    if len(args) == 3 and args[1] == "--place-user-fn":
        print generator.get_place_user_fn_header(int(args[2])),
        return

    if len(args) == 3 and args[1] == "--transition-user-fn":
        print generator.get_transition_user_fn_header(int(args[2])),
        return

    print "Usage: ptp [--debug] <project.xml> <action>"

if __name__ == '__main__':
    try:
        main(sys.argv[1:])
    except PtpException, e:
        print e
        if debug_mode:
            traceback.print_exc(file=sys.stdout)
        sys.exit(1)
