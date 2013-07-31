#
#    Copyright (C) 2010 Stanislav Bohm,
#                  2013 Martin Surkovsky
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


class EventCallback:

    def __init__(self, source, event_name, callback, *args, **kw):
        self.source = source
        self.event_name = event_name
        self.callback = callback
        self.args = args
        self.kw = kw

    def remove(self):
        self.source.remove_callback(
            self.event_name, self.callback, *self.args, **self.kw)


class EventCallbacksList:

    def __init__(self):
        self.list = []

    def set_callback(self, source, event_name, callback, *args, **kw):
        self.list.append(source.set_callback(event_name, callback, *args, **kw))

    def remove_all(self):
        for callback in self.list:
            callback.remove()
        self.list = []


class EventSource:

    def __init__(self):
        self.__callbacks = {}

    def set_callback(self, event_name, callback, *args, **kw):
        lst = self.__callbacks.setdefault(event_name, [])
        lst.append((callback, args, kw))
        return EventCallback(self, event_name, callback, *args, **kw)

    def emit_event(self, event_name, *params):
        if event_name in self.__callbacks:
            for cb, args, kw in self.__callbacks[event_name]:
                cb(*(args + params), **kw)

    def event_emitter(self, event_name):
        return lambda *x: self.emit_event(event_name, *x)

    def remove_callback(self, event_name, callback, *args, **kw):
        self.__callbacks[event_name].remove((callback, args, kw))
        if not self.__callbacks[event_name]:
            del self.__callbacks[event_name]
