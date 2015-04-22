
#ifndef CA_USERTOOLS_H
#define CA_USERTOOLS_H

#include "thread.h"
#include "net.h"
#include "cabuf.h"

#include <vector>

namespace ca {

std::vector<int> range(int from, int upto);

class Context {

	public:
		Context(ThreadBase *thread, NetBase *net) : thread(thread), net(net) {}

		void quit() {
			thread->quit_all();
		}

		int pid() const {
			return thread->get_process_id();
		}

		int count() const {
			return thread->get_process_count();
		}

		int process_id() const {
			return pid();
		}

		int process_count() const {
			return count();
		}

		std::vector<int> all_processes() const {
		    return range(0, count());
		}

	protected:
		ThreadBase *thread;
		NetBase *net;
};
}

#endif // CA_USERTOOLS_H
