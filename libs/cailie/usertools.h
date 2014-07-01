
#ifndef CA_USERTOOLS_H
#define CA_USERTOOLS_H

#include "thread.h"
#include "net.h"

#include <vector>

namespace ca {

class Context {
	public:
		Context(ThreadBase *thread, NetBase *net) : thread(thread), net(net) {}

		void quit() {
			thread->quit_all();
		}

		int process_id() const {
			return thread->get_process_id();
		}

		int process_count() const {
			return thread->get_process_count();
		}

		void trace_value(const std::string &str) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(str);
			}
		}
		void trace(const int value) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(value);
			}
		}
		void trace(const double value) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(value);
			}
		}

	protected:
		ThreadBase *thread;
		NetBase *net;
};

	std::vector<int> range(int from, int upto);
	inline std::vector<int> all_processes(Context &ctx) {
		return range(0, ctx.process_count());
	}

}

#endif // CA_USERTOOLS_H
