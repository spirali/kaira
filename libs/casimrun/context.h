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

			int get_packets_count(int process_id1, int process_id2) {
				return state->get_packets_count(process_id1, process_id2);
			}

			int get_data_size(int process_id1, int process_id2) {
				return state->get_data_size(process_id1, process_id2);
			}

			casr::State::PacketQueue get_packet_queue(int process_id1, int process_id2) {
				return state->get_current_packets(process_id1, process_id2);
			}
	};

}

#endif // CASIMRUN_TRANSTION_H
