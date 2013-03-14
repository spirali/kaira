
#include "state.h"
#include "cailie.h"

using namespace ca;

State::State(ca::NetDef *net_def, ca::Net **nets)
{
	this->quit = false;
	this->net_def = net_def;
	this->nets = new Net*[ca::process_count];
	packets = new std::deque<Packet>[ca::process_count * ca::process_count];
	for (int i = 0; i < ca::process_count; i++) {
		this->nets[i] = nets[i];
	}
}

State::~State()
{
	delete [] nets;
	delete [] packets;
}

void State::write_reports(FILE *out)
{
	Output output(out);
	output.child("report");
	output.set("net-id", nets[0]->get_def()->get_id());
	output.set("processes", ca::process_count);
	output.set("quit", quit);

	for (int i = 0; i < process_count; i++) {
		output.child("process");
		StateThread thread(this, i, 0);
		nets[i]->write_reports(&thread, output);

		if (get_idle_thread(i) != -1) {
			const std::vector<TransitionDef*>& transitions = net_def->get_transition_defs();
			for (int t = 0; t < transitions.size(); t++) {
				if (transitions[t]->is_enable(&thread, nets[i])) {
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
		output.back();
	}

	for (int i = 0; i < ca::process_count; i++) {
		for (int j = 0; j < ca::process_count; j++) {
			PacketQueue& pq = packets[i * ca::process_count + j];
			PacketQueue::iterator it;
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

void State::add_packet(int origin_id, int target_id, const Packet &packet)
{
	packets[target_id * ca::process_count + origin_id].push_back(packet);
}

void State::receive(int process_id, int origin_id)
{
	PacketQueue &pq = packets[process_id * ca::process_count + origin_id];
	if (pq.empty()) {
		return;
	}
	Packet packet = pq.front();
	pq.pop_front();
	ca::Tokens *tokens = (ca::Tokens *) packet.data;
	ca::Unpacker unpacker(tokens + 1);
	Net *net = nets[process_id];
	int edge_id = tokens->edge_id;
	int tokens_count = tokens->tokens_count;
	StateThread thread(this, process_id, 0);
	for (int t = 0; t < tokens_count; t++) {
		net->receive(&thread, packet.from_process, edge_id, unpacker);
	}
}

int State::get_idle_thread(int process_id) {
	for (int i = 0; i < ca::threads_count; i++) {
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

void State::fire_transition_phase1(int process_id, TransitionDef *transition_def)
{
	if (transition_def->is_immediate()) {
		fire_transition_full(process_id, transition_def);
		return;
	}
	int thread_id = 0;
	NetDef *net_def = nets[0]->get_def();
	StateThread thread(this, process_id, thread_id);
	void *binding = transition_def->fire_phase1(&thread, nets[process_id]);
	if (binding == NULL) {
		return;
	}

	TransitionActivation ta;
	ta.process_id = process_id;
	ta.thread_id = thread_id;
	ta.binding = binding;
	ta.transition_def = transition_def;
	activations.push_back(ta);
}

void State::fire_transition_full(int process_id, TransitionDef *transition_def)
{
	int thread_id = 0;
	StateThread thread(this, process_id, thread_id);
	transition_def->full_fire(&thread, nets[process_id]);
}

void State::finish_transition(int transition_id, int process_id, int thread_id)
{
	int t;
	TransitionActivations::iterator i;
	for (i = activations.begin(); i != activations.end(); i++) {
		if (i->process_id == process_id &&
			i->thread_id == i->thread_id &&
			i->transition_def->get_id() == transition_id) {
				StateThread thread(this, process_id, thread_id);
				i->transition_def->fire_phase2(&thread, nets[process_id], i->binding);
				activations.erase(i);
				return;
			}
	}
}

void StateThread::send_multicast(const std::vector<int> &targets,
							  ca::NetBase *net,
                              int edge_id,
                              int tokens_count,
                              const ca::Packer &packer)
{
	std::vector<int>::const_iterator i;
	ca::Tokens *data = (ca::Tokens*) packer.get_buffer();
	data->edge_id = edge_id;
	data->tokens_count = tokens_count;
	Packet packet;
	packet.data = data;
	packet.from_process = process_id;
	packet.size = packer.get_size();
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i;
		if(target < 0 || target >= ca::process_count) {
			fprintf(stderr,
					"Net sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
					tokens_count, target, ca::process_count - 1);
			exit(1);
		}
		state->add_packet(process_id, target, packet);
	}
}

int StateThread::get_process_count() const {
	return ca::process_count;
}
