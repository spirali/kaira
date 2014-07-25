
#include "statespace.h"
#include <string.h>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <alloca.h>

namespace ca {
	extern ca::NetDef **defs;
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

Node::Node(HashDigest hash, State *state, Node *prev)
	: hash(hash), state(state), prev(prev), quit(false), final_marking(NULL), tag(0), data(NULL)
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

void Node::generate(Core *core)
{
	if (state->get_quit_flag()) {
		return;
	}

	ActionSet enabled = compute_enable_set(core);
	ActionSet ws;
	if (cfg::partial_order_reduction) {
		ws = core->compute_ample_set(state, enabled);
	} else {
		ws = enabled;
	}

	State *s;

	ActionSet::iterator it;
	for (it = ws.begin(); it != ws.end(); it++) {
		if (++it != ws.end()) {
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

Core::Core(VerifConfiguration &verif_configuration) :
	nodes(10000, HashDigestHash(MHASH_MD5), HashDigestEq(MHASH_MD5)),
	initial_node(NULL),
	net_def(NULL),
	verif_configuration(verif_configuration)
{
	if (cfg::analyse_transition_occurence) {
		generate_binging_in_nni = true;
	} else {
		generate_binging_in_nni = false;
	}
}

Core::~Core()
{
	NodeMap::iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++) {
		delete it->second;
	}
}

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(MHASH_MD5);

	Node *node = get_node(hash);
	if (node == NULL) {
		node = new Node(hash, state, prev);
		node->set_quit_flag(state->get_quit_flag());
		nodes[hash] = node;
		not_processed.push(node);
		return node;
	} else {
		free(hash);
		delete state;
		if (prev) {
			node->set_prev(prev);
		}
		return node;
	}
}

void Core::generate()
{
	ca::check_parameters();

	net_def = ca::defs[0]; // Take first definition
	State *initial_state = new State(net_def);
	initial_node = add_state(initial_state, NULL);
	int count = 0;
	do {
		count++;
		if (count % 1000 == 0 && !cfg::silent) {
			fprintf(stderr, "==KAIRA== Nodes %i\n", count);
		}
		Node *node = not_processed.top();
		not_processed.pop();
		if (cfg::debug && cfg::wait_for_key == false) {
			char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
			hashdigest_to_string(MHASH_MD5, node->get_state()->compute_hash(MHASH_MD5) , hashstr);
			if (!strncmp(hashstr, cfg::interesting_state_prefix.c_str(), cfg::interesting_state_prefix.size())) {
				cfg::wait_for_key = true;
			}
		}
		node->generate(this);
		if (cfg::wait_for_key) {
			getchar();
		}
		if (node->get_nexts().size() == 0 && cfg::analyse_final_marking) {
			node->set_final_marking(pack_marking(node));
		}
		if (node->get_quit_flag()) {
			delete node->get_state();
		}
	} while (!not_processed.empty());
}

ActionSet Core::compute_ample_set(State *s, const ActionSet &enable)
{
	ActionSet::iterator it;

	if (cfg::debug) {
		// print state name
		char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
		hashdigest_to_string(MHASH_MD5, s->compute_hash(MHASH_MD5) , hashstr);
		printf(">> state: %s: \nenable: { ", hashstr);
		for (it = enable.begin(); it != enable.end(); it++) {
			printf("%s ", it->to_string().c_str());
		}
		printf("}\n");
	}

	for (it = enable.begin(); it != enable.end(); it++) {
		ActionSet ample;
		ample.insert(*it);
		if (cfg::debug) {
			printf("  > ample: { ");
			for (ActionSet::iterator i = ample.begin(); i != ample.end(); i++) {
				printf("%s ", i->to_string().c_str());
			}
			printf("}\n");
		}
		if (check_C1(enable, ample, s) && check_C2(ample) && check_C3(s)) {
			if (cfg::debug) {
				printf("  This ample set is independent.\n");
			}
			return ample;
		}
		if (cfg::wait_for_key) {
			getchar();
		}
	}
	return enable;
}

bool Core::check_C1(const ActionSet &enabled, const ActionSet &ample, State *s)
{
	ActionSet::iterator it1;
	ActionSet::iterator it2;

	std::deque<Action> queue;
	// This assume that there is no optimization for number of tokens in places
	ActionSet processed = ample;
	std::vector<bool> receive_blocked(ca::process_count * ca::process_count, false);
	std::vector<int> enabled_priorities(ca::process_count, 0);
	std::vector<int> marking = verif_configuration.get_marking(s);

	if (cfg::debug) {
		printf("    C1: Marking: {");
		int place_count = marking.size() / ca::process_count;
		for (size_t i = 0; i < marking.size(); i++) {
			printf(" %d", marking[i]);
			if ((i + 1) % place_count == 0 && i + 1 != marking.size()) {
				printf(" |");
			}
		}
		printf(" }\n");
		printf("    C1: starting configuration {");
	}
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
				if (cfg::debug) {
					printf(" %s", i->to_string().c_str());
				}
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
						if (cfg::debug) {
							printf(" %s", a.to_string().c_str());
						}
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
						if (cfg::debug) {
							printf(" %s", a.to_string().c_str());
						}
					}
				}
				continue;
			}
			processed.insert(*i);
			queue.push_back(*i);
			if (cfg::debug) {
				printf(" %s", i->to_string().c_str());
			}
		}
	}

	if (cfg::debug) {
		printf(" }\n");
	}

	while(queue.size() > 0) {
		for (ActionSet::iterator a = ample.begin(); a != ample.end(); a++) {
			if (verif_configuration.is_dependent(*a, queue.front(), marking)) {
				if (cfg::debug) {
					printf("    C1: %s and %s are dependent.\n", a->to_string().c_str(), queue.front().to_string().c_str());
				}
				return false;
			}
		}
		if (cfg::debug) {
			size_t i = queue.size();
			verif_configuration.compute_successors(queue.front(), queue, processed, receive_blocked, enabled_priorities, marking);
			printf("    C1: successors of %s: {", queue.front().to_string().c_str());
			for (; i < queue.size(); i++) {
				printf(" %s", queue[i].to_string().c_str());
			}
			printf(" }\n");
		} else {
			verif_configuration.compute_successors(queue.front(), queue, processed, receive_blocked, enabled_priorities, marking, ample);
		}
		queue.pop_front();
	}
	return true;
}

bool Core::check_C2(const ActionSet &ample)
{
	for (ActionSet::iterator it = ample.begin(); it != ample.end(); it++) {
		if (it->type == ActionFire) {
			if (verif_configuration.is_visible(*it)) {
				if (cfg::debug) {
					printf("    C2: %s is visible transition.\n", it->to_string().c_str());
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
