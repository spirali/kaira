
#ifndef CAILIE_STATE_H
#define CAILIE_STATE_H

#include <vector>
#include <deque>
#include <stdlib.h>

#include "net.h"
#include "thread.h"
#include "packet.h"
#include "packing.h"

namespace ca {


	struct TransitionActivation {
			ca::TransitionDef *transition_def;
			int process_id;
			int thread_id;
			void *binding;
	};

	typedef std::deque<Packet> PacketQueue;
	typedef std::vector<TransitionActivation> TransitionActivations;


	class State {
		public:
			State(NetDef *net_def, Net **net);
			~State();

			const TransitionActivations& get_activations() {
				return activations;
			};
			PacketQueue* get_packet_queue(int i) { return &packets[i]; }
			void write_reports(FILE *file);
			void fire_transition_phase1(int process_id, TransitionDef *transition_def);
			void fire_transition_full(int process_id, TransitionDef *transition_def);
			void finish_transition(int transition_id, int process_id, int thread_id);
			NetDef* get_net_def() { return net_def; }
			int get_idle_thread(int process_id);

			void add_packet(int source_id, int target_id, const Packet &packet);
			void receive(int process_id, int origin_id);
			void set_quit_flag() { quit = true; }
		protected:
			Net **nets;
			NetDef *net_def;
			PacketQueue *packets;
			TransitionActivations activations;
			bool quit;
	};


	class StateThread : public ThreadBase {
		public:
			StateThread(State *state, int process_id, int thread_id)
				: state(state),
				  process_id(process_id),
                  thread_id(thread_id)
				  {}
			void quit_all() { state->set_quit_flag(); }
			int get_process_count() const;
			int get_threads_count() const { return 1; }
			int get_process_id() const { return process_id; }

			void set(int process_id, int thread_id) {
				this->process_id = process_id;
				this->thread_id = thread_id;
			}

			void send(int target,
						   ca::NetBase *net,
						   int edge_id,
						   int tokens_count,
                           const ca::Packer &packer) {
				std::vector<int> a(1);
				a[0] = target;
				send_multicast(a, net, edge_id, tokens_count, packer);
			}

			void send_multicast(const std::vector<int> &targets, ca::NetBase *net,
				int edge_id, int tokens_count, const ca::Packer &packer);

		protected:
			State *state;
			int process_id;
			int thread_id;
	};
};

#endif // CAILIE_STATE_H
