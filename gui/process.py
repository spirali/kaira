#
#    Copyright (C) 2010, 2011, 2014 Stanislav Bohm
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

import gtk
import socket
from subprocess import Popen, PIPE, STDOUT
from threading import Thread, Lock


class ReadLineThread(Thread):
    def __init__(self, stream):
        Thread.__init__(self)
        self.stream = stream
        self.lock = Lock()
        self.exit_flag = False
        self.daemon = True

    def start(self):
        Thread.start(self)

    def run(self):
        while True:
            line = self.stream.readline()
            if line == "":
                self.on_exit()
                return
            with self.lock:
                if self.exit_flag:
                    return
            if not self.on_line(line, self.stream):
                return

    def readline(self):
        return self.stream.readline()

    def safe_call(self, callback, *params):
        if callback is None:
            return
        gtk.gdk.threads_enter()
        try:
            return callback(*params)
        finally:
            gtk.gdk.threads_leave()

    def set_exit_flag(self):
        with self.lock:
            self.exit_flag = True

class ProcessThread(ReadLineThread):

    def __init__(self, process, line_callback, exit_callback):
        ReadLineThread.__init__(self, process.stdout)
        self.process = process
        self.line_callback = line_callback
        self.exit_callback = exit_callback

    def on_exit(self):
        self.process.wait()
        return self.safe_call(self.exit_callback, self.process.returncode)

    def on_line(self, line, stream):
        return self.safe_call(self.line_callback, line, stream)


class ConnectionThread(ReadLineThread):

    def __init__(self, host, port, line_callback, exit_callback, connect_callback):
        ReadLineThread.__init__(self, None)
        self.host = host
        self.port = port
        self.line_callback = line_callback
        self.exit_callback = exit_callback
        self.connect_callback = connect_callback
        self.sock = None

    def run(self):
        try:
            self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.sock.connect((self.host, self.port))
            self.stream = self.sock.makefile("r")
            self.safe_call(self.connect_callback, self.stream)
            ReadLineThread.run(self)
        except socket.error, e:
            self.on_exit(str(e))

    def on_exit(self, message = None):
        if self.stream:
            self.stream.close()
        return self.safe_call(self.exit_callback, message)

    def on_line(self, line, stream):
        return self.safe_call(self.line_callback, line, stream)


class Process:

    def __init__(self, filename, line_callback = None, exit_callback = None):
        self.filename = filename
        self.line_callback = line_callback
        self.exit_callback = exit_callback
        self.cwd = None

    def start(self, params = []):
        self._start_process(params)
        self.pipe_in = self.process.stdin
        self._start_thread()

    def start_and_get_first_line(self, params = []):
        self._start_process(params)
        self.pipe_in = self.process.stdin
        line = self.process.stdout.readline()
        self._start_thread()
        return line

    def _start_process(self, params):
        self.process = Popen([ self.filename ] + params, bufsize = 0, stdin = PIPE, stdout = PIPE, stderr = STDOUT, cwd = self.cwd)

    def _start_thread(self):
        self.thread = ProcessThread(self.process, self.line_callback, self.exit_callback)
        self.thread.start()

    def write(self, string):
        self.pipe_in.write(string)

    def shutdown(self, silent = True):
        self.thread.set_exit_flag()
        if silent:
            try:
                self.process.terminate()
            except OSError:
                pass
        else:
            self.process.terminate()

class Connection:

    def __init__(self, hostname, port, line_callback = None, exit_callback = None, connect_callback = None):
        self.hostname = hostname
        self.port = port
        self.line_callback = line_callback
        self.exit_callback = exit_callback
        self.connect_callback = connect_callback

    def start(self):
        self.thread = ConnectionThread(self.hostname, self.port, self.line_callback, self.exit_callback, self.connect_callback)
        self.thread.start()

    def write(self, text):
        self.thread.sock.send(text)

class CommandWrapper:

    def __init__(self, backend):
        self.backend = backend
        self.callbacks = []
        self.lock = Lock()

    def start(self, *params):
        self.backend.line_callback = self._line_callback
        self.backend.start(*params)

    def run_command(self, command, callback, lines=None):

        if callback:
            with self.lock:
                self.callbacks.append((callback, lines))

        if command is not None:
            self.backend.write(command + "\n")

    def run_command_expect_ok(self,
                              command,
                              ok_callback=None,
                              fail_callback=None,
                              finalize_callback=None):
        def callback(line):
            if finalize_callback:
                finalize_callback()
            if line != "Ok\n":
                if fail_callback:
                    fail_callback()
                else:
                    print "Command '{0}' returns '{1}'".format(command, line)
            else:
                if ok_callback:
                    ok_callback()
        self.run_command(command, callback)

    def shutdown(self):
        self.backend.shutdown()

    def readline(self):
        """ Read line from backned. !! You can use this only if you are in "callback" !! """
        return self.backend.readline()

    def _line_callback(self, line, stream):
        if line.startswith("ERROR:"):
            print line
            return False

        with self.lock:
            assert self.callbacks, line
            cb, lines = self.callbacks[0]
            del self.callbacks[0]
        if lines is None:
            cb(line)
        else:
            buffer = [ line ] + [ stream.readline() for i in xrange(lines - 1) ]
            cb(buffer)
        return True
