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

import gtk
import os
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
			if not self.on_line(line):
				return

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

	def on_line(self, line):
		return self.safe_call(self.line_callback, line)


class ConnectionThread(ReadLineThread):

	def __init__(self, stream, line_callback, exit_callback):
		ReadLineThread.__init__(self, stream)
		self.line_callback = line_callback
		self.exit_callback = exit_callback

	def on_exit(self):
		self.stream.close()
		return self.safe_call(self.exit_callback)

	def on_line(self, line):
		return self.safe_call(self.line_callback, line)



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

	def shutdown(self):
		self.thread.set_exit_flag()
		self.process.terminate()

class Connection:

	def __init__(self, hostname, port, line_callback = None, exit_callback = None):
		self.hostname = hostname
		self.port = port
		self.line_callback = line_callback
		self.exit_callback = exit_callback

	def start(self):
		self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
		self.sock.connect((self.hostname, self.port))
		stream = os.fdopen(self.sock.fileno(),"r")
		self.thread = ConnectionThread(stream, self.line_callback, self.exit_callback)
		self.thread.start()

	def write(self, text):
		self.sock.send(text)

class CommandWrapper:

	def __init__(self, backend):
		self.backend = backend
		self.callbacks = []
		self.lock = Lock()

	def start(self, *params):
		self.backend.line_callback = self._line_callback
		self.backend.start(*params)

	def run_command(self, command, callback):
		with self.lock:
			self.callbacks.append(callback)
		self.backend.write(command + "\n")

	def run_command_expect_ok(self, command):
		def expect_ok(line):
			if line != "Ok\n":
				print "Command '" + command + "' returns: " + line
		self.run_command(command, expect_ok)

	def shutdown(self):
		self.backend.shutdown()

	def _line_callback(self, line):
		if line.startswith("ERROR:"):
			print line
			return False

		with self.lock:
			assert self.callbacks
			cb = self.callbacks[0]
			del self.callbacks[0]
		cb(line)
		return True
