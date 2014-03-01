
#ifndef CAILIE_STATE_H
#define CAILIE_STATE_H

#include <vector>
#include <deque>
#include <stdlib.h>

#include "net.h"
#include "thread.h"
#include "packet.h"
#include "packing.h"
#include "output.h"

namespace ca {

	extern int process_count;
	extern int threads_count;

	struct Activation {
		Activation(
			TransitionDef *transition_def,
			int process_id,
			int thread_id,
			void *binding)
			: transition_def(transition_def),
			  process_id(process_id),
			  thread_id(thread_id),
			  binding(binding) {}

		TransitionDef *transition_def;
		int process_id;
		int thread_id;
		void *binding;
	};

	template<typename NetT, typename ActivationT, typename PacketT>
	class StateBase {
		public:
			class StateThread : public ThreadBase {
				public:
					StateThread(StateBase *state,
								int process_id,
								int thread_id)
						: ThreadBase(thread_id,
									 state->get_tracelog(process_id, thread_id)),
						  state(state),
						  process_id(process_id),
						  thread_id(0) {}

					void quit_all() {
							state->set_quit_flag();
					}

					int get_process_count() const {
							return process_count;
					}

					int get_threads_count() const {
							return 1;
					}

					int get_process_id() const {
							return process_id;
					}

					void set(int process_id, int thread_id) {
						this->process_id = process_id;
						this->thread_id = thread_id;
					}

					void send(int target,
								   ca::NetBase *net,
								   int edge_id,
								   int tokens_count,
								   const ca::Packer &packer) {
						send(target, net, edge_id, tokens_count, packer, packer.get_size());
					}

					void send(int target,
								   ca::NetBase *net,
								   int edge_id,
								   int tokens_count,
								   const ca::Packer &packer,
								   size_t fake_size) {
						std::vector<int> a(1);
						a[0] = target;
						send_multicast(a, net, edge_id, tokens_count, packer, fake_size);
					}

					void send_multicast(const std::vector<int> &targets, ca::NetBase *net,
						int edge_id, int tokens_count, const ca::Packer &packer) {
							send_multicast(
								targets, net, edge_id, tokens_count, packer, packer.get_size());
					}

					void send_multicast(const std::vector<int> &targets, ca::NetBase *net,
						int edge_id, int tokens_count,
						const ca::Packer &packer, size_t fake_size) {
							std::vector<int>::const_iterator i;
							ca::Tokens *data = (ca::Tokens*) packer.get_buffer();
							data->edge_id = edge_id;
							data->tokens_count = tokens_count;
							PacketT packet;
							packet.from_process = process_id;
							packet.size = packer.get_size();
							for (i = targets.begin(); i != targets.end(); i++) {
								int target = *i;
								if(target < 0 || target >= ca::process_count) {
									fprintf(stderr,
											"Net sends %i token(s) to invalid "
											"process id %i (valid ids: [0 .. %i])\n",
											tokens_count, target, ca::process_count - 1);
									exit(1);
								}
								if (i + 1 == targets.end()) {
									packet.data = data;
								} else {
									packet.data = malloc(packer.get_size());
									memcpy(packet.data, data, packer.get_size());
								}
								state->packet_preprocess(process_id, target, packet, fake_size);
								state->add_packet(process_id, target, packet);
							}
					}

					int collective_bindings(TransitionDef *transition_def, std::vector<void*> &bindings) {
						bindings.resize(ca::process_count, NULL);
						int count = 0;
						for (int i = 0; i < state->activations.size(); i++) {
							ActivationT &activation = state->activations[i];
							TransitionDef *td = activation.transition_def;
							if (td->is_blocked(activation.binding)) {
								if (transition_def != td)
								{
									fprintf(stderr, "Two different collective transitions started\n");
									exit(-1);
								} else {
									bindings[activation.process_id] = activation.binding;
									count++;
								}
							}
						}
						return count;
					}

				protected:
					StateBase<NetT, ActivationT, PacketT> *state;
					int process_id;
					int thread_id;
			};

			typedef std::deque<PacketT> PacketQueue;
			typedef std::vector<ActivationT> Activations;

			StateBase() :
				nets(process_count), net_def(NULL), quit(false)
			{
				packets = new std::deque<PacketT>[ca::process_count * ca::process_count];
			}

			void spawn(NetDef *net_def) {
				this->net_def = net_def;
				for (int i = 0; i < ca::process_count; i++) {
					StateThread thread(this, i, 0);
					if (thread.get_tracelog()) {
						thread.get_tracelog()->event_net_spawn(net_def->get_id());
					}
					nets[i] = static_cast<NetT*>(net_def->spawn(&thread));
				}
			}

			StateBase(NetDef *net_def, const std::vector<NetT*> &nets)
				: nets(nets), net_def(net_def), quit(false)
			{
				packets = new std::deque<PacketT>[process_count * process_count];
			}

			StateBase(const StateBase<NetT, ActivationT, PacketT> &state)
				: nets(process_count),
				  net_def(state.net_def),
				  activations(state.activations),
				  quit(state.quit)
			{
				for (int i = 0; i < ca::process_count; i++) {
					nets[i] = static_cast<NetT*> (state.nets[i]->copy());
				}
				packets = new std::deque<PacketT>[ca::process_count * ca::process_count];
				for (int i = 0; i < ca::process_count * ca::process_count; i++) {
					packets[i] = state.packets[i];
				}
			}

			virtual ~StateBase()
			{
				delete [] packets;
			}

			Activations& get_activations() {
				return activations;
			};

			PacketQueue* get_packet_queue(int i) {
					return &packets[i];
			}

			void write_reports(FILE *out) {
				Output output(out);
				output.child("report");
				output.set("net-id", net_def->get_id());
				output.set("processes", ca::process_count);
				output.set("quit", quit);

				for (int i = 0; i < process_count; i++) {
					output.child("process");
					StateThread thread(this, i, 0);
					nets[i]->write_reports(&thread, output);

					if (get_idle_thread(i) != -1) {
						const std::vector<TransitionDef*>& transitions = \
							net_def->get_transition_defs();
						bool enabled = false;
						for (int t = 0; t < transitions.size(); t++) {
							if (enabled && transitions[t - 1]->get_priority() !=
									transitions[t]->get_priority()) {
								break;
							}
							if (transitions[t]->is_enable(&thread, nets[i])) {
								enabled = true;
								output.child("enabled");
								output.set("id", transitions[t]->get_id());
								output.back();
							}
						}
					}
					output.back();
				}

				for (int i = 0; i < activations.size(); i++) {
					output.child("activation");
					output.set("process-id", activations[i].process_id);
					output.set("thread-id", activations[i].thread_id);
					output.set("binding", activations[i].binding);
					output.set("transition-id", activations[i].transition_def->get_id());
					void *binding = activations[i].binding;
					output.set("blocked", activations[i].transition_def->is_blocked(binding));
					output.back();
				}

				for (int i = 0; i < ca::process_count; i++) {
					for (int j = 0; j < ca::process_count; j++) {
						PacketQueue& pq = packets[i * ca::process_count + j];
						typename PacketQueue::iterator it;
						for (it = pq.begin(); it != pq.end(); it++) {
							output.child("packet");
							output.set("origin-id", j);
							output.set("target-id", i);
							output.set("size", it->size);
							Tokens *tokens = (Tokens*) it->data;
							output.set("edge-id", tokens->edge_id);
							output.back();
						}
					}
				}
				output.back();

			}

			bool fire_transition_phase1(int process_id, TransitionDef *transition_def)
			{
				if (transition_def->is_immediate()) {
					StateThread thread(this, process_id, 0);
					return transition_def->full_fire(&thread, nets[process_id]);
				}
				int thread_id = 0;
				StateThread thread(this, process_id, thread_id);
				void *binding = transition_def->fire_phase1(&thread, nets[process_id]);
				if (binding == NULL) {
					return false;
				}
				ActivationT ta(transition_def, process_id, thread_id, binding);
				activations.push_back(ta);
				return true;
			}

			bool fire_transition_full(int process_id, TransitionDef *transition_def, bool ro_finish=false)
			{
				int thread_id = 0;
				if (!transition_def->is_collective()) {
					StateThread thread(this, process_id, thread_id);
					return transition_def->full_fire(&thread, nets[process_id]);
				} else {
					if (fire_transition_phase1(process_id, transition_def)) {
						for (int i = activations.size() - 1; i >= 0; i--) {
							ActivationT &a = activations[i];
							if (a.transition_def == transition_def &&
								!a.transition_def->is_blocked(a.binding)) {
								if (ro_finish) {
									finish_transition_ro_binding(i);
								} else {
									finish_transition(i);
								}
							}

						}
						return true;
					} else {
						return false;
					}
				}
			}

			bool fire_transition_full_with_binding(int process_id, TransitionDef *transition_def,
					ca::Packer &packer)
			{
				int thread_id = 0;
				StateThread thread(this, process_id, thread_id);
				return transition_def->full_fire_with_binding(&thread, nets[process_id], packer);
			}

			bool is_transition_enabled(int process_id, TransitionDef *transition_def)
			{
				int thread_id = 0;
				StateThread thread(this, process_id, thread_id);
				return transition_def->is_enable(&thread, nets[process_id]);
			}

			void finish_transition(typename std::vector<ActivationT>::iterator i)
			{
				StateThread thread(this, i->process_id, i->thread_id);
				i->transition_def->fire_phase2(
					&thread, nets[i->process_id], i->binding);
				activations.erase(i);
			}

			void finish_transition_ro_binding(typename std::vector<ActivationT>::iterator i)
			{
				StateThread thread(this, i->process_id, i->thread_id);
				i->transition_def->fire_phase2_ro_binding(
					&thread, nets[i->process_id], i->binding);
				activations.erase(i);
			}

			void finish_transition(int i)
			{
				finish_transition(activations.begin() + i);
			}

			void finish_transition_ro_binding(int i)
			{
				finish_transition_ro_binding(activations.begin() + i);
			}

			typename std::vector<ActivationT>::iterator find_activation(int process_id, int thread_id) {
				typename Activations::iterator i;
				for (i = activations.begin(); i != activations.end(); i++) {
					if (i->process_id == process_id &&
						i->thread_id == i->thread_id) {
							return i;
						}
				}
				return activations.end();

			}

			bool is_process_busy(int process_id) {
				typename Activations::iterator i;
				for (i = activations.begin(); i != activations.end(); i++) {
					if (i->process_id == process_id) {
						return true;
					}
				}
				return false;
			}

			bool finish_transition(int process_id, int thread_id)
			{
				typename Activations::iterator i = find_activation(process_id, thread_id);
				if (i != activations.end()) {
					finish_transition(i);
					return true;
				}
				return false;
			}

			NetDef* get_net_def() { return net_def; }
			NetT* get_net(int id) { return nets[id]; }

			int get_idle_thread(int process_id) {
				for (int i = 0; i < threads_count; i++) {
					int j;
					for (j = 0; j < activations.size(); j++) {
						if (activations[j].process_id == process_id &&
							activations[j].thread_id == i) {
							break;
						}
					}
					if (j == activations.size()) {
						return i;
					}
				}
				return -1;
			}

			void add_packet(int origin_id, int target_id, const PacketT &packet) {
				packets[target_id * ca::process_count + origin_id].push_back(packet);
			}

			// Pre-process packet before send
			virtual void packet_preprocess(
				int origin_id, int target_id, PacketT &packet, size_t fake_size) {}

			bool receive(int process_id, int origin_id, bool free_data=true) {
				PacketQueue &pq = packets[process_id * ca::process_count + origin_id];
				if (pq.empty()) {
					return false;
				}
				Packet packet = pq.front();
				pq.pop_front();
				ca::Tokens *tokens = (ca::Tokens *) packet.data;
				ca::Unpacker unpacker(tokens + 1);
				NetBase *net = nets[process_id];
				int edge_id = tokens->edge_id;
				int tokens_count = tokens->tokens_count;
				StateThread thread(this, process_id, 0);
				TraceLog *tracelog = thread.get_tracelog();
				if (tracelog) {
					tracelog->event_receive(origin_id);
				}
				for (int t = 0; t < tokens_count; t++) {
					net->receive(&thread, packet.from_process, edge_id, unpacker);
				}
				if (free_data) {
					free(packet.data);
				}
				return true;
			}

			int get_receiving_edge(int process_id, int origin_id, int position) {
				PacketQueue &pq = packets[process_id * ca::process_count + origin_id];
				if (pq.empty() || position >= pq.size()) {
					return -1;
				} else {
					Packet packet = pq[position];
					ca::Tokens *tokens = (ca::Tokens *) packet.data;
					return tokens->edge_id;
				}
			}

			int get_token_count_in_edge(int process_id, int origin_id, int edge_id) {
				PacketQueue &pq = packets[process_id * ca::process_count + origin_id];
				int count = 0;
				for (int i = 0; i < pq.size(); i++) {
					ca::Tokens *tokens = (ca::Tokens *) pq[i].data;
					if (tokens->edge_id == edge_id) {
						count++;
					}
				}
				return count;
			}

			PacketQueue & get_packets(int process_id, int origin_id) {
				return packets[process_id * process_count + origin_id];
			}

			void set_quit_flag() {
					quit = true;
			}

			bool get_quit_flag() {
					return quit;
			}

		protected:
			virtual TraceLog* get_tracelog(int process_id, int thread_id) {
				return NULL;
			}
			std::vector<NetT*> nets;
			NetDef *net_def;
			PacketQueue *packets;
			Activations activations;
			bool quit;
	};

	typedef StateBase<Net, Activation, Packet> State;
};

#endif // CAILIE_STATE_H
