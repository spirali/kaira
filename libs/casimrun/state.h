#ifndef CASIMRUN_STATE_H
#define CASIMRUN_STATE_H

#include <state.h>
#include <packet.h>
#include <cailie.h>
#include "tracelog.h"
#include "runconfiguration.h"

namespace casr {

struct Packet : public ca::Packet
{
	ca::IntTime release_time;
};

struct ThreadInfo
{
	ControlledTimeTraceLog *tracelog;
	ca::IntTime release_time;
	bool idle;
};


class State : public ca::StateBase<ca::Net, ca::Activation, Packet>
{
	public:
	State(RunConfiguration &run_configuration, ca::NetDef *net_def);
	~State();
	void run();
	ca::IntTime run_process(int process_id);
	void packet_preprocess(
			int origin_id, int target_id, Packet &packet, size_t fake_size);

	ca::IntTime get_global_time() {
		return global_time;
	}

	bool is_process_free(int process_id) {
		return get_thread_info(process_id, 0).release_time <= global_time;
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

}

#endif
