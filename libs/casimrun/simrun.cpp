
#include "simrun.h"
#include "tracelog.h"
#include <cailie.h>
#include <state.h>
#include <net.h>
#include <packet.h>
#include <iostream>

namespace ca {
	extern ca::NetDef **defs;
	extern int defs_count;
	extern int process_count;
	extern char *project_description_string;
	extern size_t tracelog_size;
}


namespace casr {

struct ThreadInfo
{
	ControlledTimeTraceLog *tracelog;
	ca::IntTime release_time;
	bool idle;
};

struct Packet : public ca::Packet
{
	ca::IntTime release_time;
};

class State : public ca::StateBase<ca::Net, ca::Activation, Packet>
{
	public:
	State(RunConfiguration &run_configuration, ca::NetDef *net_def)
		: thread_info(ca::process_count * ca::threads_count),
          run_configuration(run_configuration) {
		global_time = 0;
		for (int p = 0; p < ca::process_count; p++) {
			free_threads.push_back(ca::threads_count);
			for (int t = 0; t < ca::threads_count; t++) {
				ThreadInfo &ti = get_thread_info(p, t);
				ti.release_time = 0;
				ti.idle = false;
				if (ca::tracelog_size > 0) {
					ti.tracelog = new ControlledTimeTraceLog(p, t, ca::tracelog_size);
				} else {
					ti.tracelog = NULL;
				}
			}
		}
		spawn(net_def);
	}

	~State()
	{
		for (int p = 0; p < ca::process_count; p++) {
			free_threads.push_back(ca::threads_count);
			for (int t = 0; t < ca::threads_count; t++) {
				ThreadInfo &ti = get_thread_info(p, t);
				if (ti.tracelog) {
					delete ti.tracelog;
				}
			}
		}
	}

	void run() {
		for (;;) {
			ca::IntTime next_time = ca::MAX_INT_TIME;
			for (int p = 0; p < ca::process_count; p++) {
				ca::IntTime t = run_process(p);
				if (t < next_time) {
					next_time = t;
				}
			}
			if (quit) {
				global_time = next_time;
				return;
			}
			if (next_time == ca::MAX_INT_TIME) {
				fprintf(stderr, "Deadlock detected\n");
				return;
			}
			global_time = next_time;
		}
	}

	ca::IntTime get_global_time() {
		return global_time;
	}

	bool is_process_free(int process_id) {
		return get_thread_info(process_id, 0).release_time <= global_time;
	}

	ca::IntTime run_process(int process_id) {
		ThreadInfo &ti = get_thread_info(process_id, 0);

		if (ti.release_time > global_time) {
			// This process is still working
			return ti.release_time;
		}

	    ControlledTimeTraceLog *tracelog =
			(ControlledTimeTraceLog*) get_tracelog(process_id, 0);

		if (tracelog) {
			tracelog->set_basetime(global_time);
		}

		// Minimal time when some packet can be received
		ca::IntTime next_time = ca::MAX_INT_TIME;
		// Was at least one packet received?
		bool received = false;

		// Packet processing
		for (int p = 0; p < ca::process_count; p++) {
			PacketQueue &pq = get_packets(process_id, p);
			if (pq.empty()) {
				continue;
			}
			Packet &packet = pq.front();
			if (packet.release_time > global_time) {
				if (packet.release_time < next_time) {
					next_time = packet.release_time;
				}
				continue;
			}
			receive(process_id, p);
			p--; // Run receive once again
			received = true;
		}
		if (received) {
			tracelog->event_end();
			ti.release_time = tracelog->get_time();
			return ti.release_time;
		}

		// Transitions
		ca::Net *n = nets[process_id];
		ca::Transition *tr = n->pick_active_transition();
		if (tr == NULL) {
			if (!ti.idle && tracelog) {
				tracelog->event_idle();
			}
			ti.idle = true;
			return next_time;
		}
	    ti.idle = false;
		if (!fire_transition_full(process_id, tr->get_def())) {
			tr->set_active(false);
		}
		ti.release_time = tracelog->get_time();
		return ti.release_time;
	}

	void packet_preprocess(int origin_id, int target_id, Packet &packet, size_t fake_size) {
	    ControlledTimeTraceLog *tracelog =
			(ControlledTimeTraceLog*) get_tracelog(origin_id, 0);
		packet.release_time = tracelog->get_time() +
			run_configuration.packet_time(origin_id, target_id, fake_size);
	}

	protected:
	ca::TraceLog* get_tracelog(int process_id, int thread_id) {
		return get_thread_info(process_id, thread_id).tracelog;
	}

	ThreadInfo& get_thread_info(int process_id, int thread_id) {
			return thread_info[process_id * ca::threads_count + thread_id];
	}

	ca::IntTime global_time;
	std::vector<int> free_threads;
	std::vector<ThreadInfo> thread_info;
	RunConfiguration& run_configuration;
};

void main(RunConfiguration &run_configuration)
{
	ControlledTimeTraceLog::init();
	ca::check_parameters();
	ca::NetDef *net_def = ca::defs[0]; // Take first definition
	State state(run_configuration, net_def);
	state.run();
	std::cerr << "Kaira: Time = " << state.get_global_time() / 1e6 << "ms\n";
}

}
