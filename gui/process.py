
import gtk
from subprocess import Popen, PIPE, STDOUT
from threading import Thread, Lock


class ProcessThread(Thread):
	def __init__(self, process, line_callback, exit_callback):
		Thread.__init__(self)
		self.process = process
		self.line_callback = line_callback
		self.exit_callback = exit_callback
		self.lock = Lock()
		self.exit_flag = False
		self.daemon = True

	def start(self):
		Thread.start(self)

	def run(self):
		while True:
			line = self.process.stdout.readline()
			if line == "":
				self.process.wait()
				gtk.gdk.threads_enter()
				try:
					if self.exit_callback:
						self.exit_callback(self.process.returncode)
				finally:
					gtk.gdk.threads_leave()
				with process_list_lock:
					process_list.remove(self.process)
				return
			with self.lock:
				if self.exit_flag:
					return
			gtk.gdk.threads_enter()
			try:
				if not self.line_callback(line):
					self.process.terminate()
					return
			finally:
				gtk.gdk.threads_leave()

	def set_exit_flag(self):
		with self.lock:
			self.exit_flag = True

class Process:
	
	def __init__(self, filename, line_callback, exit_callback):
		self.filename = filename
		self.line_callback = line_callback
		self.exit_callback = exit_callback
		self.cwd = None

	def start(self, params = []):
		self.process = Popen([ self.filename ] + params, bufsize = 0, stdin = PIPE, stdout = PIPE, stderr = STDOUT, cwd = self.cwd)
		with process_list_lock:
			process_list.append(self.process)
		self.pipe_in = self.process.stdin
		self.thread = ProcessThread(self.process, self.line_callback, self.exit_callback)
		self.thread.start()

	def _write(self, string):
		self.pipe_in.write(string)

	def shutdown(self):
		self.thread.set_exit_flag()
		self.process.terminate()

class CommandProcess(Process):

	def __init__(self, filename):
		Process.__init__(self,filename, self._callback, None)
		self.callbacks = []
		self.lock = Lock()

	def run_command(self, command, callback):
		with self.lock:
			self.callbacks.append(callback)
		self._write(command + "\n")

	def _callback(self, line):
		if line.startswith("ERROR:"):
			print "Process " + self.filename + ": " + line
			return False
		with self.lock:
			assert self.callbacks
			cb = self.callbacks[0]
			del self.callbacks[0]
		cb(line)
		return True

def terminate_all_processes():
	with process_list_lock:
		for process in process_list:
			process.terminate()

process_list_lock = Lock()
process_list = []
