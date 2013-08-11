#
#    Copyright (C) 2012 Stanislav Bohm
#
#    This file is part of Kaira.
#
#    Kaira is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, version 3 of the License, or
#    (at your option) any later version.
#
#    Kaira is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with Kaira.  If not, see <http://www.gnu.org/licenses/>.
#

import os

KAIRA_PTP = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
KAIRA_ROOT = os.path.dirname(KAIRA_PTP)

KAIRA_CONFIG_INI = os.path.join(KAIRA_ROOT, "build", "config.ini")

CAILIE_INCLUDE_DIR = "libs/cailie"
CAILIE_LIB_DIR = "build/libs/cailie"

CASERVER_INCLUDE_DIR = "libs/caserver"
CASERVER_LIB_DIR = "build/libs/caserver"
CACLIENT_INCLUDE_DIR = "libs/caclient"
CACLIENT_LIB_DIR = "build/libs/caclient"

CAVERIF_INCLUDE_DIR = "libs/caverif"
CAVERIF_LIB_DIR = "build/libs/caverif"

CASIMRUN_INCLUDE_DIR = "libs/casimrun"
CASIMRUN_LIB_DIR = "build/libs/casimrun"

CAOCTAVE_INCLUDE_DIR = "libs/caoctave"
