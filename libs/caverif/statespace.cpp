
#include "statespace.h"
#include <string.h>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <alloca.h>
#include <omp.h>

namespace ca {
	extern int process_count;
}

using namespace cass;

void State::pack_state(ca::Packer &packer)
{
	pack_activations(packer);
	pack_packets(packer);
	for (int t = 0; t < ca::process_count; t++) {
		nets[t]->pack(packer);
	}
}

void State::pack_packets(ca::Packer &packer)
{
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++)
	{
		size_t size = packets[pq].size();
		pack(packer, size);
		for (size_t p = 0; p < packets[pq].size(); p++) {
			pack(packer, packets[pq][p].size);
			pack(packer, packets[pq][p].data, packets[pq][p].size);
		}
	}
}

void State::pack_activations(ca::Packer &packer)
{
	pack(packer, get_activate_process_count());
	Activation *a;
	for (int i = 0; i < ca::process_count; i++) {
		if (activations[i] == NULL) {
			continue;
		}
		a = activations[i];
		int id = a->transition_def->get_id();
		pack(packer, id);
		pack(packer, i);
		pack(packer, a->packed_binding, a->packed_binding_size);
	}
}

void State::hash_activations(MHASH hash_thread)
{
	size_t size = get_activate_process_count();
	mhash(hash_thread, &size, sizeof(size_t));
	Activation *a;
	for (int i = 0; i < ca::process_count; i++) {
		if (activations[i] == NULL) {
			continue;
		}
		a = activations[i];
		int id = a->transition_def->get_id();
		mhash(hash_thread, &id, sizeof(int));
		mhash(hash_thread, &i, sizeof(int));
		mhash(hash_thread, a->packed_binding, a->packed_binding_size);
	}
}

void State::hash_packets(MHASH hash_thread)
{
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++)
	{
		size_t size = packets[pq].size();
		mhash(hash_thread, &size, sizeof(size_t));
		for (size_t p = 0; p < packets[pq].size(); p++) {
			mhash(hash_thread, &packets[pq][p].size, sizeof(packets[pq][p].size));
			mhash(hash_thread, packets[pq][p].data, packets[pq][p].size);
		}
	}
}

HashDigest State::compute_hash(hashid hash_id)
{
	MHASH hash_thread = mhash_init(hash_id);
	if (hash_thread == MHASH_FAILED) {
		fprintf(stderr, "Hash failed\n");
		exit(1);
	}
	mhash(hash_thread, &quit, sizeof(quit));
	hash_activations(hash_thread);
	hash_packets(hash_thread);
	ca::Packer packer;
	for (int t = 0; t < ca::process_count; t++) {
		packer.reset();
		nets[t]->pack(packer);
		mhash(hash_thread, packer.get_buffer(), packer.get_size());
	}
	packer.free();
	return mhash_end(hash_thread);
}

HashDigest Core::pack_marking(Node *node) {
	ca::Packer packer;
	for (int t = 0; t < ca::process_count; t++) {
		verif_configuration.pack_final_marking(
				node->get_state()->get_net(t), packer);
	}
	HashDigest hash = hash_packer(packer);
	packer.free();
	return hash;
}

HashDigest Core::hash_packer(ca::Packer &packer)
{
	MHASH hash_thread = mhash_init(MHASH_MD5);
	if (hash_thread == MHASH_FAILED) {
		fprintf(stderr, "Hash failed\n");
		exit(1);
	}
	mhash(hash_thread, packer.get_buffer(), packer.get_size());
	return mhash_end(hash_thread);
}

const char hex_chars[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

void Core::hashdigest_to_string(HashDigest hash, char *out)
{
	int size = mhash_get_block_size(CASS_HASH_ID);
	for (int i = 0; i < size; i++) {
		char byte = ((char*) hash)[i];
		out[i*2] = hex_chars[(byte & 0xF0) >> 4];
		out[i*2+1] = hex_chars[byte & 0x0F];
	}
	out[size*2] = 0;
}

std::string Core::hashdigest_to_string(HashDigest hash)
{
	char *hashstr = (char*) alloca(mhash_get_block_size(CASS_HASH_ID) * 2 + 1);
	hashdigest_to_string(hash, hashstr);
	return hashstr;
}

void State::serialize(std::vector<char> &data)
{
	data.clear();

	// quit state
	data.insert(data.end(), reinterpret_cast<const char*>(&quit), reinterpret_cast<const char*>(&quit) + sizeof(quit));

	// nets
	ca::Packer packer;
	for (int t = 0; t < ca::process_count; t++) {
		packer.reset();
		nets[t]->pack(packer);
		size_t net_size = packer.get_size();
		data.insert(data.end(), reinterpret_cast<const char*>(&net_size), reinterpret_cast<const char*>(&net_size) + sizeof(net_size));
		data.insert(data.end(), packer.get_buffer(), packer.get_buffer() + packer.get_size());
	}
	packer.free();

	// activations
	size_t size = get_activate_process_count();
	data.insert(data.end(), reinterpret_cast<const char*>(&size), reinterpret_cast<const char*>(&size) + sizeof(size));
	Activation *a;
	for (int i = 0; i < ca::process_count; i++) {
		if (activations[i] == NULL) {
			continue;
		}

		a = activations[i];
		int id = a->transition_def->get_id();

		data.insert(data.end(), reinterpret_cast<const char*>(&i), reinterpret_cast<const char*>(&i) + sizeof(i));
		data.insert(data.end(), reinterpret_cast<const char*>(&id), reinterpret_cast<const char*>(&id) + sizeof(id));
		std::string bbb((char*)a->packed_binding, a->packed_binding_size);
		data.insert(data.end(), reinterpret_cast<const char*>(&a->packed_binding_size), reinterpret_cast<const char*>(&a->packed_binding_size) + sizeof(a->packed_binding_size));
		data.insert(data.end(), reinterpret_cast<const char*>(a->packed_binding), reinterpret_cast<const char*>(a->packed_binding) + a->packed_binding_size);
	}

	// packets
	std::vector<size_t> sizes;
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++) {
		size_t size = packets[pq].size();
		sizes.push_back(size);
		data.insert(data.end(), reinterpret_cast<const char*>(&size), reinterpret_cast<const char*>(&size) + sizeof(size));
		for (size_t p = 0; p < packets[pq].size(); p++) {
			data.insert(data.end(), reinterpret_cast<const char*>(&packets[pq][p].from_process), reinterpret_cast<const char*>(&packets[pq][p].from_process) + sizeof(packets[pq][p].from_process));
			data.insert(data.end(), reinterpret_cast<const char*>(&packets[pq][p].size), reinterpret_cast<const char*>(&packets[pq][p].size) + sizeof(packets[pq][p].size));
			data.insert(data.end(), reinterpret_cast<const char*>(packets[pq][p].data), reinterpret_cast<const char*>(packets[pq][p].data) + packets[pq][p].size);
		}
	}
}

template <typename TType>
static void readData(TType &data, char* &p)
{
	memcpy(&data, p, sizeof(TType));
	p += sizeof(TType);
}

void State::deserialize(std::vector<char> &data)
{
	char* p = data.data();

	// QUIT FLAG
	readData(quit, p);

	// NET
	for (int t = 0; t < ca::process_count; t++) {
		size_t net_size;
		readData(net_size, p);

		ca::Unpacker unpacker(p, net_size);
		nets[t]->unpack(unpacker);
		p += net_size;
	}

	// ACTIVATIONS
	for (int i = 0; i < ca::process_count; i++) {
		if (activations[i] != NULL) {
			delete activations[i];
			activations[i] = NULL;
		}
	}

	int index, id;
	size_t size, binding_size;

	readData(size, p);
	for (size_t i = 0; i < size; i++) {
		readData(index, p);
		readData(id, p);
		readData(binding_size, p);

		ca::TransitionDef *t = net_def->get_transition_def(id);
		ca::Unpacker unpacker(p, binding_size);
		ca::Binding *binding = t->unpack_binding(unpacker);
		p += binding_size;

		activations[index] = new Activation(t, binding);
	}

	// PACKETS
	std::vector<size_t> sizes;
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++) {
		packets[pq].clear();
		readData(size, p);
		sizes.push_back(size);
		for (size_t i = 0; i < size; i++) {
			packets[pq].push_back(ca::Packet());
			readData(packets[pq].back().from_process, p);
			readData(packets[pq].back().size, p);
			packets[pq].back().data = malloc(packets[pq].back().size);
			mempcpy(packets[pq].back().data, p, packets[pq].back().size);
			p += packets[pq].back().size;
		}
	}
}

Node::Node(HashDigest hash, State *state, Node *prev)
	: hash(hash), state(state), prev(prev), quit(false), quit_flag(false), final_marking(NULL), tag(0), data(NULL)
{
	if (prev != NULL) {
		distance = prev->get_distance() + 1;
	} else {
		distance = 0;
	}
}

Node::~Node()
{
	free(hash);
	if (final_marking != NULL) {
		free(final_marking);
	}
	for(size_t i = 0; i < nexts.size(); i++) {
		if (nexts[i].action == ActionFire && nexts[i].data.fire.binding != NULL) {
			free(nexts[i].data.fire.binding);
		}
	}
}

void Node::set_prev(Node *node)
{
	if (prev != NULL && node->get_distance() + 1 < get_distance()) {
		prev = node;
		distance = node->get_distance() + 1;
	}
}

void Node::set_quit(bool quit_flag)
{
	this->quit_flag = quit_flag;
	const std::vector<Activation*> &activations = state->get_activations();
	for (size_t i = 0; i < activations.size(); i++) {
		if (activations[i] != NULL && activations[i]->transition_def->is_collective()) {
			quit = false;
			return;
		}
	}
	quit = quit_flag;
}

ActionSet Node::compute_enable_set(Core *core)
{
	ActionSet enable;

	ca::NetDef *def = core->get_net_def();
	const std::vector<ca::TransitionDef*> &transitions = def->get_transition_defs();

	int enabled_priorities = 0;
	bool added;
	for (int p = 0; p < ca::process_count; p++) {
		if (state->is_process_busy(p)) {
			continue;
		}
		added = false;
		for (int i = 0; i < def->get_transitions_count(); i++) {
			if (added && transitions[i]->get_priority() < enabled_priorities) {
				break;
			}
			if (state->is_transition_enabled(p, transitions[i])) {
				added = true;
				enabled_priorities = transitions[i]->get_priority();
				Action action;
				action.type = ActionFire;
				action.data.fire.transition_def = transitions[i];
				action.process = p;
				enable.insert(action);
			}
		}
	}

	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++) {
		int target = pq / ca::process_count;
		int source = pq % ca::process_count;
		int edge_id = state->get_receiving_edge(target, source, 0);

		if (edge_id != -1) {
			Action action;
			action.type = ActionReceive;
			action.data.receive.source = source;
			action.data.receive.edge_id = edge_id;
			action.process = target;
			enable.insert(action);
		}
	}

	return enable;
}

void Node::compute_ample(Core *core)
{
	if (quit_flag) {
		return;
	}

	ActionSet enabled = compute_enable_set(core);
	if (cfg::partial_order_reduction) {
		ample =  core->compute_ample_set(state, enabled);
	} else {
		ample = enabled;
	}
}

void Node::fire_ample(Core *core)
{
	State *s;

	ActionSet::iterator it;
	for (it = ample.begin(); it != ample.end(); it++) {
		if (++it != ample.end()) {
			s = new State(*state);
		} else {
			s = state;
		}
		it--;
		switch (it->type) {
			case ActionFire:
			{
				ca::Packer packer;
				if (core->generate_binding_in_nni(it->data.fire.transition_def->get_id())) {
					s->fire_transition_full_with_binding(it->process, it->data.fire.transition_def, packer);
				} else {
					s->fire_transition_full(it->process, it->data.fire.transition_def);
				}
				Node *n = core->add_state(s, this);
				NextNodeInfo nninfo;
				nninfo.node = n;
				nninfo.action = ActionFire;
				nninfo.data.fire.process_id = it->process;
				nninfo.data.fire.transition_id = it->data.fire.transition_def->get_id();
				if (core->generate_binding_in_nni(it->data.fire.transition_def->get_id())) {
					nninfo.data.fire.binding = core->hash_packer(packer);
				} else {
					nninfo.data.fire.binding = NULL;
				}
				nexts.push_back(nninfo);
				packer.free();
				break;
			}

			case ActionReceive:
			{
				s->receive(it->process, it->data.receive.source, true);
				Node *n = core->add_state(s, this);
				NextNodeInfo nninfo;
				nninfo.node = n;
				nninfo.action = ActionReceive;
				nninfo.data.receive.process_id = it->process;
				nninfo.data.receive.source_id = it->data.receive.source;
				nexts.push_back(nninfo);
				break;
			}
		}
	}

}

struct ValidSubset {
	ValidSubset(const ActionSet &ample, const ActionSet &denied, const std::vector<int> &marking)
	: ample(ample), denied(denied), marking(marking) { next = this->ample.begin(); }

	ValidSubset(const ValidSubset &other)
	: ample(other.ample), denied(other.denied), marking(other.marking) { next = this->ample.begin(); }

	ValidSubset& operator=(const ValidSubset &other) {
		if (&other == this) {
			ample = other.ample;
			denied = other.denied;
			marking = other.marking;
			next = ample.begin();
		}
		return *this;
	}

	ActionSet ample;
	ActionSet denied;
	ActionSet::iterator next;
	std::vector<int> marking;
};

ActionSet Core::compute_ample_set(State *s, const ActionSet &enable)
{
	if (cfg::debug) {
		debug_output << "\n" << Core::hashdigest_to_string(s->compute_hash(hash_id));
		debug_output << " states: " << nodes.size() << ", on stact: " << not_processed.size();
		debug_output.setf(std::ios::fixed, std::ios::floatfield);
		debug_output.precision(2);
		debug_output << " " << not_processed.size() / (double)nodes.size() << "%\n";
		debug_output << "ENABLED: " << enable << "\n";
	}

	if (enable.size() == 1) {
		fullyEplored++;
		return enable;
	}

	std::vector<int> marking;

	if (cfg::only_singletons) {
		marking = verif_configuration.get_marking(s);
		for (auto it = enable.begin(); it != enable.end(); it++) {
			std::vector<int> mcopy = marking;
			ActionSet ample;
			ample.insert(*it);
			if (cfg::debug) {
				debug_output << "\nTry: " << ample << "\n";
			}
			check_C1(enable, ample, s, mcopy);
			if (ample.size() && check_C2(ample) && check_C3(s)) {
				singleExplored++;
				return ample;
			}
		}

		if (cfg::debug) {
			debug_output << "AMPLE SET: " << enable << "\n\n";
		}
		fullyEplored++;
		return enable;
	}

	ActionSet best = enable;
	std::list<ValidSubset> subset = { ValidSubset(enable, ActionSet(), verif_configuration.get_marking(s)) };

	while (subset.size()) {

		ActionSet denied = subset.back().denied;
		while (subset.back().next != subset.back().ample.end()) {
			if (subset.back().denied.find(*subset.back().next) != subset.back().denied.end()) {
				++subset.back().next;
				continue;
			}

			ActionSet ample = subset.back().ample;
			ample.erase(*subset.back().next);
			if (cfg::debug) {
				debug_output << "\nTry: " << ample << "\n";
			}

			marking = subset.back().marking;
			check_C1(subset.back().ample, ample, s, marking);
			if (ample.size() && check_C2(ample) && check_C3(s)) {
				if (ample.size() == 1) {
					if (cfg::debug) {
						debug_output << "AMPLE SET: " << ample << "\n\n";
					}
					singleExplored++;
					return ample;
				}
				if (best.size() > ample.size()) {
					best = ample;
				}
				subset.back().next++;
				subset.push_back(ValidSubset(ample, denied, marking));
				if (cfg::debug) {
					debug_output << "VALID SUBSET: " << subset.back().ample << "\n";
				}
				break;
			} else {
				denied.insert(*subset.back().next++);
			}
		}
		if (subset.back().next == subset.back().ample.end()) {
			subset.pop_back();
			if (subset.back().ample.size() > cfg::all_subset_max_size) {
				break;
			}
		}
	}

	if (cfg::debug) {
		debug_output << "AMPLE SET: " << best << "\n\n";
	}

	if (enable.size() == best.size()) {
		fullyEplored++;
	} else {
		partlyExplored++;
	}
	return best;
}

void Core::check_C1(const ActionSet &enabled, ActionSet &ample, State *s, std::vector<int> &marking)
{
	ActionSet::iterator it1;
	ActionSet::iterator it2;

	std::deque<Action> queue;
	ActionSet processed = ample;
	std::vector<bool> receive_blocked(ca::process_count * ca::process_count, false);
	std::vector<int> enabled_priorities(ca::process_count, 0);

	for (ActionSet::iterator i = enabled.begin(); i != enabled.end(); i++) {
		if (ample.find(*i) != ample.end()) {
			if (i->type == ActionReceive) {
				receive_blocked[i->process * ca::process_count + i->data.receive.source] = true;
			}
			if (i->type == ActionFire) {
				enabled_priorities[i->process] = i->data.fire.transition_def->get_priority();
			}
		} else {
			if (i->type == ActionFire) {
				processed.insert(*i);
				queue.push_back(*i);
				const std::vector<ca::TransitionDef*> &transitions = net_def->get_transition_defs();
				for (int t = 0; t < net_def->get_transitions_count(); t++) {
					if (transitions[t]->get_priority() >= enabled_priorities[i->process]) continue;
					if (s->is_transition_enabled(i->process, transitions[t])) {
						Action a;
						a.type = ActionFire;
						a.data.fire.transition_def = transitions[t];
						a.process = i->process;
						processed.insert(a);
						queue.push_back(a);
					}
				}
				continue;
			}
			if (i->type == ActionReceive) {
				const State::PacketQueue& pq = s->get_packets(i->process, i->data.receive.source);
				for (size_t p = 0; p < pq.size(); p++) {
					Action a;
					a.type = ActionReceive;
					a.process = i->process;
					a.data.receive.source = i->data.receive.source;
					ca::Tokens *tokens = (ca::Tokens *) pq[p].data;
					a.data.receive.edge_id = tokens->edge_id;
					if (processed.find(a) == processed.end()) {
						processed.insert(a);
						queue.push_back(a);
					}
				}
				continue;
			}
			processed.insert(*i);
			queue.push_back(*i);
		}
	}

	while(queue.size()) {
		for (ActionSet::iterator a = ample.begin(); a != ample.end(); ) {
			if (verif_configuration.is_dependent(*a, queue.front(), marking)) {
				if (cfg::debug) {
					debug_output << "C1 remove " << *a << " (" << queue.front() << " is dependent successor)\n";
				}
				if (processed.find(*a) == processed.end()) {
					processed.insert(*a);
					queue.push_back(*a);
				}
				a = ample.erase(a);
				if (!ample.size()) {
					return;
				}
			} else {
				++a;
			}
		}
		verif_configuration.compute_successors(queue.front(), queue, processed, receive_blocked, enabled_priorities, marking, ample);
		queue.pop_front();
	}
}

bool Core::check_C2(const ActionSet &ample)
{
	for (ActionSet::iterator it = ample.begin(); it != ample.end(); it++) {
		if (it->type == ActionFire) {
			if (verif_configuration.is_visible(*it)) {
				if (cfg::debug) {
					debug_output << "C2 is violated: " << *it << " is visible transition\n";
				}
				return false;
			}
		}
	}
	return true;
}

bool Core::check_C3(State *s)
{
	// TODO: Checking for cycles
	return true;
}

void Core::set_tags(int tag)
{
	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		node->set_tag(tag);
	}
}

Node * Core::get_node(HashDigest digest) const
{
	NodeMap::const_iterator it = nodes.find(digest);
	if (it == nodes.end())
		return NULL;
	else
		return it->second;
}

const NextNodeInfo& Node::get_next_node_info(Node *node) const
{
	for (int i = 0; nexts.size(); i++) {
		if (nexts[i].node == node) {
			return nexts[i];
		}
	}
	printf("get_next_node_info: Node not found\n");
	abort();
}

bool Core::is_known_node(Node *node) const
{
	return nodes.find(node) != nodes.end();
}

bool Core::generate_binding_in_nni(int transition_id)
{
	if (generate_binging_in_nni && verif_configuration.is_transition_analyzed(transition_id)) {
		return true;
	} else {
		return false;
	}

}
