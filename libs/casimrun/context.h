#ifndef CASIMRUN_CONTEXT_H
#define CASIMRUN_CONTEXT_H

#include "tracelog.h"
#include "state.h"
#include <net.h>

namespace casr {

	extern State *state;

	class Context : public ca::Context {
		public:
			Context(ca::ThreadBase *thread) :
				ca::Context(thread, NULL) {};

			ca::IntTime time() {
				return state->get_global_time();
			}
	};

}

#endif // CASIMRUN_TRANSTION_H
