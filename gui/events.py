
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
