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

class EventSource:

	def __init__(self):
		self.__callbacks = {}

	def set_callback(self, event_name, callback):
		lst = self.__callbacks.setdefault(event_name, [])
		lst.append(callback)

	def emit_event(self, event_name, *params):
		if event_name in self.__callbacks:
			for cb in self.__callbacks[event_name]:
				cb(*params)

	def event_emitter(self, event_name):
		return lambda *x: self.emit_event(event_name, *x)
