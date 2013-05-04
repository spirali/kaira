
#include "simrun.h"
#include <cailie.h>
#include <state.h>
#include <net.h>
#include <packet.h>

namespace ca {
	extern ca::NetDef **defs;
	extern int defs_count;
	extern int process_count;
	extern char *project_description_string;
}

using namespace casr;

class State : public ca::StateBase<ca::Net, ca::Activation, ca::Packet>
{
	public:
	State(ca::NetDef *net_def) : StateBase(net_def) {
		time = 0;
		for (int p = 0; p < ca::process_count; p++) {
			free_threads.push_back(ca::threads_count);
		}
	}

	void run() {
		for (;;) {
			for (int p = 0; p < ca::process_count; p++) {
				run_process(p);
				if (quit) {
					return;
				}
			}
			check_activations();
			if (quit) {
				return;
			}
		}
	}

	void run_process(int process) {
		if (free_threads[process] == 0) {
			return;
		}
		for (int p = 0; p < ca::process_count; p++) {
			if (receive(process, p)) {
				return;
			}
		}
		ca::Net *n = nets[process];
		ca::Transition *tr = n->pick_active_transition();
		if (tr == NULL) {
			/*n->unlock();
			if (!in_idle && tracelog) {
				tracelog->event_idle();
			}
			in_idle = true;
			continue;*/
			return;
		}
	    //in_idle = false;
		if (!fire_transition_phase1(process, tr->get_def())) {
			tr->set_active(false);
		}
	}

	void check_activations()
	{
		for (int i = 0; i < activations.size(); i++) {
			finish_transition(i);
			if (quit) {
				return;
			}
			i--;
		}
	}

	protected:
	uint64_t time;
	std::vector<int> free_threads;
};

void casr::main()
{
	ca::check_parameters();
	ca::NetDef *net_def = ca::defs[0]; // Take first definition
	State state(net_def);
	state.run();
}
