
import gtk
from subprocess import Popen, PIPE
from threading import Thread, Lock

class ProcessThread(Thread):
	def __init__(self, process, callback):
		Thread.__init__(self)
		self.process = process
		self.callback = callback
		self.lock = Lock()
		self.exit_flag = False

	def start(self):
		Thread.start(self)

	def run(self):
		while True:
			line = self.process.stdout.readline()
			if line == "":
				return
			with self.lock:
				if self.exit_flag:
					return
			gtk.gdk.threads_enter()
			try:
				if not self.callback(line):
					self.processes.terminate()
					return
			finally:
				gtk.gdk.threads_leave()

	def set_exit_flag(self):
		with self.lock:
			self.exit_flag = True

class Process:
	
	def __init__(self, filename, callback):
		self.filename = filename
		self.callback = callback

	def start(self, params):
		self.process = Popen([ self.filename ] + params, bufsize = 0, stdin = PIPE, stdout = PIPE)
		with process_list_lock:
			process_list.append(self.process)
		self.pipe_in = self.process.stdin
		self.thread = ProcessThread(self.process, self.callback)
		self.thread.start()

	def _write(self, string):
		self.pipe_in.write(string)

	def shutdown(self):
		with process_list_lock:
			self.process_list_lock.remove(self.process)
		self.thread.set_exit_flag()
		self.process.terminate()

class CommandProcess(Process):

	def __init__(self, filename):
		Process.__init__(self,filename, self._callback)
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
