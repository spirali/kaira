#
#    Copyright (C) 2010 Stanislav Bohm
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

KAIRA_GUI = os.path.dirname(os.path.abspath(__file__))
KAIRA_ROOT = os.path.dirname(KAIRA_GUI)

ICONS_DIR = os.path.join(KAIRA_GUI, "icons")
UI_DIR = os.path.join(KAIRA_GUI, "ui")

PTP_DIR = os.path.join(KAIRA_ROOT, "ptp")
PTP_BIN = os.path.join(KAIRA_ROOT, "ptp", "ptp.py")

PACKAGES_DIR = os.path.join(KAIRA_ROOT, "packages")
EXTENSIONS_DIR = os.path.join(KAIRA_GUI, "extensions")
