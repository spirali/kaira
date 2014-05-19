
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
		Activation(TransitionDef *transition_def, Binding *binding)
			: transition_def(transition_def), binding(binding) {}

		Activation(const Activation &a) {
			transition_def = a.transition_def;
			if (a.binding != NULL) {
				binding = a.binding->copy();
			} else {
				binding = NULL;
			}
		}

		Activation& operator=(const Activation &a) {
			if (this != &a) {
				if (binding != NULL) {
					delete binding;
				}

				transition_def = a.transition_def;
				if (a.binding != NULL) {
					binding = a.binding->copy();
				} else {
					binding = NULL;
				}
			}
			return *this;
		}

		virtual ~Activation() {
			if (binding != NULL) {
				delete binding;
			}
		}

		TransitionDef *transition_def;
		ca::Binding *binding;
	};

	template<typename NetT, typename ActivationT, typename PacketT>
	class StateBase {
		public:
			class StateThread : public ThreadBase {
				public:
					StateThread(StateBase *state,
								int process_id)
						: ThreadBase(state->get_tracelog(process_id)),
						  state(state),
						  process_id(process_id) {}

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

					void set_process(int process_id) {
						this->process_id = process_id;
					}

					void send(int target,
								   ca::NetBase *net,
								   int edge_id,
								   int tokens_count,
								   ca::Packer &packer) {
						send(target, net, edge_id, tokens_count, packer, packer.get_size());
					}

					void send(int target,
								   ca::NetBase *net,
								   int edge_id,
								   int tokens_count,
								   ca::Packer &packer,
								   size_t fake_size) {
						std::vector<int> a(1);
						a[0] = target;
						send_multicast(a, net, edge_id, tokens_count, packer, fake_size);
					}

					void send_multicast(const std::vector<int> &targets, ca::NetBase *net,
						int edge_id, int tokens_count, ca::Packer &packer) {
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

					int collective_bindings(TransitionDef *transition_def, std::vector<Binding*> &bindings) {
						bindings.resize(ca::process_count, NULL);
						int count = 0;
						for (int i = 0; i < process_count; i++) {
							ActivationT *a = state->activations[i];
							if (a == NULL) {
								continue;
							}
							TransitionDef *td = a->transition_def;
							if (td->is_blocked(a->binding)) {
								if (transition_def != td)
								{
									fprintf(stderr, "Two different collective transitions started\n");
									exit(-1);
								} else {
									bindings[i] = a->binding;
									count++;
								}
							}
						}
						return count;
					}

				protected:
					StateBase<NetT, ActivationT, PacketT> *state;
					int process_id;
			};

			typedef std::deque<PacketT> PacketQueue;
			typedef std::vector<ActivationT*> Activations;

			StateBase() :
				nets(process_count),
				net_def(NULL),
				activations(process_count,
				(ActivationT*) NULL),
				quit(false)
			{
				packets = new std::deque<PacketT>[ca::process_count * ca::process_count];
			}

			void spawn(NetDef *net_def) {
				this->net_def = net_def;
				for (int i = 0; i < ca::process_count; i++) {
					StateThread thread(this, i);
					if (thread.get_tracelog()) {
						thread.get_tracelog()->event_net_spawn(net_def->get_id());
					}
					nets[i] = static_cast<NetT*>(net_def->spawn(&thread));
				}
			}

			StateBase(NetDef *net_def, const std::vector<NetT*> &nets) :
				nets(nets),
				net_def(net_def),
				activations(process_count, (ActivationT*) NULL),
				quit(false)
			{
				packets = new std::deque<PacketT>[process_count * process_count];
			}

			StateBase(const StateBase<NetT, ActivationT, PacketT> &state)
				: nets(process_count),
				  net_def(state.net_def),
				  activations(process_count, (ActivationT*) NULL),
				  quit(state.quit)
			{
				for (int i = 0; i < ca::process_count; i++) {
					nets[i] = static_cast<NetT*> (state.nets[i]->copy());
				}

				for (int i = 0; i < process_count; i++) {
					if (state.activations[i] != NULL) {
						activations[i] = new ActivationT(*state.activations[i]);
					}
				}

				packets = new std::deque<PacketT>[ca::process_count * ca::process_count];
				for (int i = 0; i < ca::process_count * ca::process_count; i++) {
					packets[i] = state.packets[i];
					for (size_t t = 0; t < packets[i].size(); t++) {
						packets[i][t].data = malloc(packets[i][t].size);
						memcpy(packets[i][t].data, state.packets[i][t].data, packets[i][t].size);
					}
				}
			}

			virtual ~StateBase()
			{
				for (int i = 0; i < ca::process_count * ca::process_count; i++) {
					for (size_t t = 0; t < packets[i].size(); t++) {
						free(packets[i][t].data);
					}
				}
				delete [] packets;
				for (size_t i = 0; i < nets.size(); i++) {
					delete nets[i];
				}

				for (int i = 0; i < process_count; i++) {
					if (activations[i] != NULL) {
						delete activations[i];
					}
				}

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
					StateThread thread(this, i);
					nets[i]->write_reports(&thread, output);

					if (!is_process_busy(i)) {
						const std::vector<TransitionDef*>& transitions = \
							net_def->get_transition_defs();
						bool enabled = false;
						for (size_t t = 0; t < transitions.size(); t++) {
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

				for (int i = 0; i < process_count; i++) {
					if (activations[i] == NULL) {
						continue;
					}
					output.child("activation");
					output.set("process-id", i);
					output.set("binding", activations[i]->binding);
					output.set("transition-id", activations[i]->transition_def->get_id());
					Binding *binding = activations[i]->binding;
					output.set("blocked", activations[i]->transition_def->is_blocked(binding));
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
					StateThread thread(this, process_id);
					return transition_def->full_fire(&thread, nets[process_id]);
				}
				StateThread thread(this, process_id);
				Binding *binding = transition_def->fire_phase1(&thread, nets[process_id]);
				if (binding == NULL) {
					return false;
				}

				activations[process_id] = new ActivationT(transition_def, binding);
				return true;
			}

			bool fire_transition_full(int process_id, TransitionDef *transition_def)
			{
				if (!transition_def->is_collective()) {
					StateThread thread(this, process_id);
					return transition_def->full_fire(&thread, nets[process_id]);
				} else {
					if (fire_transition_phase1(process_id, transition_def)) {
						for (int i = process_count - 1; i >= 0; i--) {
							if (activations[i] == NULL) {
								continue;
							}
							ActivationT *a = activations[i];
							if (a->transition_def == transition_def &&
								!a->transition_def->is_blocked(a->binding)) {

									finish_transition(i);
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
				StateThread thread(this, process_id);
				return transition_def->full_fire_with_binding(&thread, nets[process_id], packer);
			}

			bool is_transition_enabled(int process_id, TransitionDef *transition_def)
			{
				StateThread thread(this, process_id);
				return transition_def->is_enable(&thread, nets[process_id]);
			}

			void finish_transition(int process_id)
			{
				ActivationT *a = activations[process_id];
				StateThread thread(this, process_id);
				a->transition_def->fire_phase2(&thread, nets[process_id], a->binding);
				a->binding = NULL;
				delete activations[process_id];
				activations[process_id] = NULL;
			}

			bool is_process_busy(int process_id) {
				return (activations[process_id] != NULL);
			}

			size_t get_activate_process_count() {
				size_t size = 0;
				for (int i = 0; i < process_count; i++) {
					if (activations[i] != NULL) {
						size++;
					}
				}
				return size;
			}

			NetDef* get_net_def() { return net_def; }
			NetT* get_net(int id) { return nets[id]; }

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
				StateThread thread(this, process_id);
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
				if (pq.empty() || position >= static_cast<int>(pq.size())) {
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
			virtual TraceLog* get_tracelog(int process_id) {
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
