
# A tool for exporting .proj files into a source codes
# and makefiles without running gui of Kaira. Designed
# for deploying applications on a server.'

import subprocess
import os
import sys
import argparse

KAIRA_ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
KAIRA_GUI = os.path.join(KAIRA_ROOT,"gui")
PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")
CMDUTILS = os.path.join(KAIRA_GUI, "cmdutils.py")

def main():

    parser = argparse.ArgumentParser(
            description='A tool for exporting .proj files into a source codes '
                        'and makefiles without running gui of Kaira. Designed '
                        'for deploying applications on a server.')
    parser.add_argument('filename', type=str)
    parser.add_argument('--output', metavar="DIRECTORY", type=str, default="deploy")
    parser.add_argument("--trace", action='store_true')
    args = parser.parse_args()

    callargs = [ "python" , CMDUTILS, "--export", args.filename, "--output", args.output ]
    if args.trace:
        callargs.append("--trace")
    subprocess.check_output(callargs)

    def run_ptp(self, operation=None):
        if operation is None:
            operation = "build"

    xmlfile = os.path.join(args.output, os.path.basename(args.filename).split(".")[0] + ".xml")
    subprocess.check_output([PTP_BIN, "build", xmlfile, "--output", args.output ])

if __name__ == "__main__":
    main()
