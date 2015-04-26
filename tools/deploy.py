
# A tool for exporting .proj files into a source codes
# and makefiles without running gui of Kaira. Designed
# for deploying applications on a server.'

import ConfigParser
import argparse
import os
import subprocess
import sys


KAIRA_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
PTP = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")
CONFIG_INI = os.path.join(KAIRA_ROOT, "build", "config.ini")

def show_warning(msg):
    print "Kaira is not correctly installed."
    print "Run './waf configure' in Kaira top directory"
    print msg
    sys.exit(1)

config = ConfigParser.RawConfigParser()
if not config.read(CONFIG_INI):
    show_warning("File '{0}' was not found.".format(CONFIG_INI))

def get_config(section, name):
    try:
        return config.get(section, name)
    except ConfigParser.NoOptionError:
        show_warning("{0}/{1} not found in config.ini".format(section, name))

PYTHON = get_config("Main", "PYTHON")

def main():

    parser = argparse.ArgumentParser(
            description='A tool for exporting .proj files into a source codes '
                        'and makefiles without running gui of Kaira. Designed '
                        'for deploying applications on a server.')
    parser.add_argument('filename', type=str)
    parser.add_argument('--output', metavar="DIRECTORY", type=str, default="deploy")
    parser.add_argument("--trace", action='store_true')
    args = parser.parse_args()

    callargs = [ PYTHON, CMDUTILS,
                 "--export", args.filename,
                 "--output", args.output ]

    if args.trace:
        callargs.append("--trace")
    subprocess.check_output(callargs, stderr=subprocess.STDOUT)

    def run_ptp(self, operation=None):
        if operation is None:
            operation = "build"

    xmlfile = os.path.join(args.output, os.path.basename(args.filename).split(".")[0] + ".xml")
    subprocess.check_output(
            [PYTHON, PTP, "build", xmlfile, "--output", args.output ],
            stderr=subprocess.STDOUT)

if __name__ == "__main__":
    try:
        main()
    except subprocess.CalledProcessError, e:
        print "Command FAILED:", e.cmd
        print "Output: "
        print e.output
