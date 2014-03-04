
#include "state.h"
#include "context.h"

namespace ca {
	extern int process_count;
	extern size_t tracelog_size;
}

casr::State::State(RunConfiguration &run_configuration, ca::NetDef *net_def)
	: thread_info(ca::process_count),
  run_configuration(run_configuration) {
	global_time = 0;
	for (int p = 0; p < ca::process_count; p++) {
		ThreadInfo &ti = thread_info[p];
		ti.release_time = 0;
		ti.idle = false;
		if (ca::tracelog_size > 0) {
			ti.tracelog = new ControlledTimeTraceLog(p, 0, ca::tracelog_size);
		} else {
			ti.tracelog = NULL;
		}
	}
	spawn(net_def);
}

casr::State::~State()
{
	for (int p = 0; p < ca::process_count; p++) {
		ThreadInfo &ti = thread_info[p];
		if (ti.tracelog) {
			delete ti.tracelog;
		}
	}
}

void casr::State::run() {

	quit_time = ca::MAX_INT_TIME;
	for (;;) {
		ca::IntTime next_time = ca::MAX_INT_TIME;
		for (int p = 0; p < ca::process_count; p++) {
			ca::IntTime t = run_process(p);
			if (t < next_time) {
				next_time = t;
			}
		}
		if (quit_time <= next_time && quit_time != ca::MAX_INT_TIME) {
			global_time = quit_time;
			return;
		}
		if (next_time == ca::MAX_INT_TIME) {
			fprintf(stderr, "Deadlock detected\n");
			return;
		}
		global_time = next_time;
	}
}

ca::IntTime casr::State::run_process(int process_id) {
	ThreadInfo &ti = thread_info[process_id];

	if (ti.release_time > global_time) {
		// This process is still working
		return ti.release_time;
	}

	ControlledTimeTraceLog *tracelog =
		(ControlledTimeTraceLog*) get_tracelog(process_id);

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
	if (quit && quit_time == ca::MAX_INT_TIME) {
		quit_time = ti.release_time;
	}
	return ti.release_time;
}

void casr::State::packet_preprocess(
		int origin_id, int target_id, Packet &packet, size_t fake_size) {
	ControlledTimeTraceLog *tracelog =
		(ControlledTimeTraceLog*) get_tracelog(origin_id);
	StateThread thread(this, target_id);
	Context ctx(&thread);
	packet.start_time = tracelog->get_time();
	packet.release_time = packet.start_time +
		run_configuration.packet_time(ctx, origin_id, target_id, fake_size);
}

casr::State::PacketQueue casr::State::get_current_packets(int process_id1, int process_id2)
{
	PacketQueue &pq = get_packets(process_id1, process_id2);
	PacketQueue result;
	for (size_t t = 0; t < pq.size(); t++) {
		if (global_time >= pq[t].start_time) {
			result.push_back(pq[t]);
		}
	}
	return result;
}

int casr::State::get_packets_count(int process_id1, int process_id2)
{
	PacketQueue &pq = get_packets(process_id1, process_id2);
	return pq.size();
}

size_t casr::State::get_data_size(int process_id1, int process_id2)
{
	PacketQueue &pq = get_packets(process_id1, process_id2);
	size_t size = 0;
	for (size_t t = 0; t < pq.size(); t++) {
		size += pq[t].size;
	}
	return size;
}

