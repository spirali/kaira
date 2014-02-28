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
	ca::IntTime start_time;
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
		return thread_info[process_id].release_time <= global_time;
	}

	PacketQueue get_current_packets(int process_id1, int process_id2);
	int get_packets_count(int process_id1, int process_id2);
	size_t get_data_size(int process_id1, int process_id2);

	protected:
	ca::TraceLog* get_tracelog(int process_id) {
		return thread_info[process_id].tracelog;
	}

	ca::IntTime global_time;
	ca::IntTime quit_time;
	std::vector<ThreadInfo> thread_info;
	RunConfiguration& run_configuration;
};

}

#endif
