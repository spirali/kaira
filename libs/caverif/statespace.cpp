
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
	extern char *project_description_string;
}

static int const MAX_STATES_IN_REPORT = 5;

using namespace cass;

static bool write_dot = false;
static bool analyse_deadlock = false;
static bool analyse_transition_occurrence = false;
static bool analyse_final_marking = false;
static bool analyse_cycle = false;
static bool partial_order = true;
static bool testing_mode = false;
static bool debug = false;
static std::string project_name;


struct CmpByDistance
{
    bool operator()(Node *a, Node *b) const
    {
        return a->get_distance() < b->get_distance();
    }
};

static void args_callback(char c, char *optarg, void *data)
{
	if (c == 'V') {
		if (!strcmp(optarg, "dot")) {
			write_dot = true;
			return;
		}
		if (!strcmp(optarg, "debug")) {
			debug = true;
			return;
		}
		if (!strcmp(optarg, "deadlock")) {
			analyse_deadlock = true;
			return;
		}
		if (!strcmp(optarg, "tchv")) {
			analyse_transition_occurrence = true;
			return;
		}
		if (!strcmp(optarg, "fmarking")) {
			analyse_final_marking = true;
			return;
		}
		if (!strcmp(optarg, "cycle")) {
			analyse_cycle = true;
			return;
		}
		if (!strcmp(optarg, "disable_partial_order")) {
			partial_order = false;
			return;
		}
		if (!strcmp(optarg, "test")) {
			testing_mode = true;
			return;
		}
		fprintf(stderr, "Invalid argument in -V\n");
		exit(1);
	}
}

const char hex_chars[16] = {
	'0', '1', '2', '3', '4', '5', '6', '7',
	'8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

static void hashdigest_to_string(hashid hash_id, HashDigest hash, char *out)
{
	int size = mhash_get_block_size(hash_id);
	for (int i = 0; i < size; i++) {
		char byte = ((char*) hash)[i];
		out[i*2] = hex_chars[(byte & 0xF0) >> 4];
		out[i*2+1] = hex_chars[byte & 0x0F];
	}
	out[size*2] = 0;
}

static std::string hashdigest_to_string(hashid hash_id, HashDigest hash)
{
	char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
	hashdigest_to_string(hash_id, hash, hashstr);
	return hashstr;
}

void cass::init(int argc, char **argv, std::vector<ca::Parameter*> &parameters)
{
	char s[1024];
	strncpy(s, argv[0], 1024);
	project_name = basename(s); // basename can modify its argument
	init(argc, argv, parameters, "V:", args_callback);
}

void State::pack_state(ca::Packer &packer)
{
	pack_activations(packer);
	pack_packets(packer);
	for (int t = 0; t < ca::process_count; t++) {
		nets[t]->pack(packer);
	}
}

void State::hash_activations(MHASH hash_thread)
{
	size_t size = activations.size();
	mhash(hash_thread, &size, sizeof(size_t));
	std::sort(activations.begin(), activations.end(), ActivationCompare());
	std::vector<Activation>::iterator i;
	for (i = activations.begin(); i != activations.end(); i++) {
		int id = i->transition_def->get_id();
		mhash(hash_thread, &id, sizeof(int));
		mhash(hash_thread, &i->process_id, sizeof(i->process_id));
		mhash(hash_thread, &i->thread_id, sizeof(i->thread_id));
		mhash(hash_thread, i->packed_binding, i->packed_binding_size);
	}
}

void State::hash_packets(MHASH hash_thread)
{
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++)
	{
		size_t size = packets[pq].size();
		mhash(hash_thread, &size, sizeof(size_t));
		for (int p = 0; p < packets[pq].size(); p++) {
			mhash(hash_thread, &packets[pq][p].size, sizeof(packets[pq][p].size));
			mhash(hash_thread, packets[pq][p].data, packets[pq][p].size);
		}
	}
}

void State::pack_packets(ca::Packer &packer)
{
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++)
	{
		size_t size = packets[pq].size();
		pack(packer, size);
		for (int p = 0; p < packets[pq].size(); p++) {
			pack(packer, packets[pq][p].size);
			pack(packer, packets[pq][p].data, packets[pq][p].size);
		}
	}
}

void State::pack_activations(ca::Packer &packer)
{
	size_t size = activations.size();
	pack(packer, size);
	std::sort(activations.begin(), activations.end(), ActivationCompare());
	std::vector<Activation>::iterator i;
	for (i = activations.begin(); i != activations.end(); i++) {
		int id = i->transition_def->get_id();
		pack(packer, id);
		pack(packer, i->process_id);
		pack(packer, i->thread_id);
		pack(packer, i->packed_binding, i->packed_binding_size);
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
	return mhash_end(hash_thread);
}

Node::Node(HashDigest hash, State *state, Node *prev)
	: hash(hash), state(state), prev(prev), data(NULL), tag(0)
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
}

void Node::set_prev(Node *node)
{
	if (prev != NULL && node->get_distance() + 1 < get_distance()) {
		prev = node;
		distance = node->get_distance() + 1;
	}
}

WorkSet* Node::compute_work_set(Core *core)
{
	WorkSet *ws = new WorkSet;

	ca::NetDef *def = core->get_net_def();
	const std::vector<ca::TransitionDef*> &transitions = def->get_transition_defs();

	for (int p = 0; p < ca::process_count; p++) {
		for (int i = 0; i < def->get_transitions_count(); i++) {
			if (state->is_transition_enabled(p, transitions[i])) {
				Action action;
				action.type = ActionFire;
				action.data.fire.transition_def = transitions[i];
				action.process = p;
				ws->insert(action);
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
			ws->insert(action);
		}
	}

	return ws;
}

void Node::generate(Core *core)
{
	if (state->get_quit_flag()) {
		return;
	}

	ca::NetDef *def = core->get_net_def();
	const std::vector<ca::TransitionDef*> &transitions = def->get_transition_defs();
	WorkSet *ws = compute_work_set(core);
	if (partial_order) {
		ws = core->compute_ample_set(state, ws);
	}

	State *s;

	WorkSet::iterator it;
	for (it = ws->begin(); it != ws->end(); it++) {
		s = new State(*state);
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
				nninfo.data.fire.thread_id = 0;
				nninfo.data.fire.transition_id = it->data.fire.transition_def->get_id();
				if (core->generate_binding_in_nni(it->data.fire.transition_def->get_id())) {
					nninfo.data.fire.binding = core->hash_packer(packer);
				}
				nexts.push_back(nninfo);
			} break;

			case ActionReceive:
			{
				s->receive(it->process, it->data.receive.source, false);
				Node *n = core->add_state(s, this);
				NextNodeInfo nninfo;
				nninfo.node = n;
				nninfo.action = ActionReceive;
				nninfo.data.receive.process_id = it->process;
				nninfo.data.receive.source_id = it->data.receive.source;
				nexts.push_back(nninfo);
			} break;
		}
	}

	delete ws;
}

Core::Core(VerifConfiguration &verif_configuration) : initial_node(NULL), nodes(10000, HashDigestHash(MHASH_MD5),
		HashDigestEq(MHASH_MD5)), net_def(NULL), verif_configuration(verif_configuration)
{
	if (analyse_transition_occurrence) {
		generate_binging_in_nni = true;
	} else {
		generate_binging_in_nni = false;
	}
}

Core::~Core()
{
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
		if (count % 1000 == 0 && !testing_mode) {
			fprintf(stderr, "Nodes %i\n", count);
		}
		Node *node = not_processed.top();
		not_processed.pop();
		node->generate(this);
		if (debug) {
			getchar();
		}
	} while (!not_processed.empty());
}

WorkSet* Core::compute_ample_set(State *s, WorkSet *ws)
{
	WorkSet::iterator it;

	if (debug) {
		// print state name
		char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
		hashdigest_to_string(MHASH_MD5, s->compute_hash(MHASH_MD5) , hashstr);
		hashstr[5] = 0;
		printf(">>>>> %s <<<<<\n", hashstr);
		for (it = ws->begin(); it != ws->end(); it++) {
			it->print("", " | ");
		}
		printf("\n");
	}

	for (it = ws->begin(); it != ws->end(); it++) {
		WorkSet *subset = new WorkSet();
		subset->insert(*it);
		if (debug) {
			it->print("CHECK ", "");
		}
		if (check_C1(ws, subset, s) && check_C2(subset) && check_C3(s)) {
			if (debug) {
				printf(" ::>> OK\n");
			}
			delete ws;
			return subset;
		}
		delete subset;
	}
	return ws;
}

bool Core::check_C1(WorkSet *ws, WorkSet *subset, State *s)
{
	WorkSet::iterator it1;
	WorkSet::iterator it2;
	for (it1 = subset->begin(); it1 != subset->end(); it1++) {
		for (it2 = ws->begin(); it2 != ws->end(); it2++) {
			if (subset->count(*it2)) continue;
			if (verif_configuration.is_dependent(*it1, *it2, *s)) {
				if (debug) {
					it2->print(" IS DEP ON ", "\n");
				}
				return false;
			}
			if (verif_configuration.is_predecesor(*it2, *it1, *s, debug)) {
				if (debug) {
					it2->print("", "\n");
				}
				return false;
			}
		}
	}
	return true;
}

bool Core::check_C2(WorkSet *ws)
{
	WorkSet::iterator it;
	for (it = ws->begin(); it != ws->end(); it++) {
		if (it->type == ActionFire) {
			if (verif_configuration.is_visible(*it)) {
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

void Core::write_dot_file(const std::string &filename)
{
	FILE *f = fopen(filename.c_str(), "w");
	char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
	fprintf(f, "digraph X {\n");
	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		const std::vector<NextNodeInfo> &nexts = node->get_nexts();
		if (node == initial_node) {
			if (testing_mode) {
				hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);
				fprintf(f, "S%p [label=\"%s\", style=filled]\n", node, hashstr);
			} else {
				fprintf(f, "S%p [style=filled, label=init]\n", node);
			}
		} else {
			hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);
			if (!testing_mode) {
				hashstr[5] = 0;
			}
			int packets_count = 0;
			for (int i = 0; i < ca::process_count * ca::process_count; i++) {
				packets_count += node->get_state()->get_packets(
					i / ca::process_count, i % ca::process_count).size();
			}
			const char *quit_flag;
			if (node->get_state()->get_quit_flag()) {
				quit_flag = "!";
			} else {
				quit_flag = "";
			}
			fprintf(f, "S%p [label=\"%s%s|%i|%i|%i\"]\n",
				node,
				quit_flag,
				hashstr,
				(int) node->get_state()->get_activations().size(),
				packets_count,
				node->get_distance());
		}
		for (size_t i = 0; i < nexts.size(); i++) {
			fprintf(f, "S%p -> S%p [label=\"", node, nexts[i].node);
			char c;
			switch(nexts[i].action) {
				case ActionFire:
					fprintf(f, "s %i %i\"]\n",
						nexts[i].data.fire.process_id,
						nexts[i].data.fire.transition_id);
					break;
				case ActionFinish:
					fprintf(f, "f %i\"]\n",
						nexts[i].data.finish.process_id);
					break;
				case ActionReceive:
					fprintf(f, "r %i %i\"]\n",
						nexts[i].data.receive.process_id,
						nexts[i].data.receive.source_id);
					break;
			}
		}
	}
	fprintf(f, "}\n");

	fclose(f);
}

void Core::postprocess()
{
	if (write_dot) {
		write_dot_file("statespace.dot");
	}

	FILE *f = fopen((project_name + ".kreport").c_str(), "w");
	ca::Output report(f);
	report.child("report");
	report.set("version", 1);

	report.child("analysis");
	report.set("name", "Overall statistics");
	report.child("result");
	report.set("name", "Number of states");
	report.set("value", nodes.size());
	report.back();
	report.back();

	if (analyse_deadlock || analyse_final_marking) {
		run_analysis_final_nodes(report);
	}
	if (analyse_transition_occurrence) {
		run_analysis_transition_occurrence(report);
	}
	if (analyse_cycle) {
		run_analysis_cycle(report);
	}

	report.child("description");
	report.text(ca::project_description_string);
	report.back();

	report.back();
	fclose(f);
}

bool Core::generate_binding_in_nni(int transition_id)
{
	if (generate_binging_in_nni && verif_configuration.is_transition_analyzed(transition_id)) {
		return true;
	} else {
		return false;
	}

}

static void write_control_line(ca::NetDef *def, std::stringstream &s, const NextNodeInfo &nninfo)
{
	switch (nninfo.action) {
		case ActionFire: {
			s << nninfo.data.fire.process_id;
			s << " " << nninfo.data.fire.thread_id;
			s << " T ";
			ca::TransitionDef *t = def->get_transition_def(nninfo.data.fire.transition_id);
			if (t->get_name().size() > 0) {
				s << t->get_name();
			} else {
				s << "#" << nninfo.data.fire.transition_id;
			}
			break;
		}
		case ActionReceive:
			s << nninfo.data.receive.process_id;
			s << " 0 R " << nninfo.data.receive.source_id;
			break;
	}
	s << std::endl;
}

void Core::write_control_sequence(std::vector<Node*> &nodes, ca::Output &report)
{
	std::vector<Node*> path;
	Node *node;

	if (nodes.size() == 0) {
		fprintf(stderr, "Control sequence of zero length cannot be written\n");
		exit(1);
	}

	path.reserve(nodes[0]->get_distance() + nodes.size());
	for (int i = nodes.size() - 1; i > 0; i--) {
		path.push_back(nodes[i]);
	}

	node = nodes[0];
	do {
		path.push_back(node);
	} while ((node = node->get_prev()) != NULL);

	std::stringstream s;
	Node *prev = path[path.size() - 1];
	for (std::vector<Node*>::reverse_iterator i = path.rbegin() + 1;
		 i != path.rend();
		 ++i) {
		write_control_line(net_def, s, prev->get_next_node_info(*i));
		prev = *i;
	}

	report.child("control-sequence");
	report.text(s.str());
	report.back();
}

void Core::write_state(const std::string &name, Node *node, ca::Output &report)
{
	report.child("state");
	report.set("name", name);
	report.set("hash", hashdigest_to_string(MHASH_MD5, node->get_hash()));
	report.set("distance", node->get_distance());
	std::vector<Node*> nodes;
	nodes.push_back(node);
	write_control_sequence(nodes, report);
	report.back();
}

void Core::write_suffix(const std::string &name, std::vector<Node*> &nodes, ca::Output &report)
{
	report.child("state");
	report.set("name", name);
	report.set("hash", hashdigest_to_string(MHASH_MD5, nodes.back()->get_hash()));
	report.set("distance", nodes.back()->get_distance());
	write_control_sequence(nodes, report);
	report.back();
}

void Core::run_analysis_final_nodes(ca::Output &report)
{
	size_t deadlocks = 0;
	Node* deadlock_node = NULL;
	NodeMap final_markings(100, HashDigestHash(MHASH_MD5), HashDigestEq(MHASH_MD5));

	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		if (node->get_nexts().size() == 0) {
			if (analyse_final_marking) {
				ca::Packer packer;
				for (int t = 0; t < ca::process_count; t++) {
					verif_configuration.pack_final_marking(
							node->get_state()->get_net(t), packer);
				}
				HashDigest hash = hash_packer(packer);
				NodeMap::const_iterator n = final_markings.find(hash);
				if (n != final_markings.end()) {
					Node *node2 = n->second;
					if (node2->get_distance() > node->get_distance()) {
						final_markings[hash] = node;
					}
					free(hash);
				} else {
					final_markings[hash] = node;
				}
				packer.free();
			}
			if (analyse_deadlock) {
				if (!node->get_state()->get_quit_flag()) {
					deadlocks++;
					if (deadlock_node == NULL ||
						deadlock_node->get_distance() > node->get_distance()) {
						deadlock_node = node;
					}
				}
			}
		}
	}

	if (analyse_deadlock) {
		report.child("analysis");
		report.set("name", "Quit analysis");

		report.child("result");
		report.set("name", "Number of deadlock states");
		report.set("value", deadlocks);
		if (deadlocks != 0) {
			report.set("status", "fail");
			report.set("text", "Deadlocks found");
			report.child("states");
			write_state("Deadlock with minimal distance", deadlock_node, report);
			report.back();
		} else {
			report.set("status", "ok");
		}
		report.back();
		report.back();
	}

	if (analyse_final_marking) {
		report.child("analysis");
		report.set("name", "Final marking");
		report.child("result");
		report.set("name", "Number of final markings");
		report.set("value", final_markings.size());
		if (final_markings.size() < 2) {
			report.set("status", "ok");
		} else {
			report.set("status", "fail");
			report.set("text", "There are more final markings.");

			std::vector<Node*> ns;
			ns.reserve(final_markings.size());

			for (NodeMap::const_iterator it = final_markings.begin();
				it != final_markings.end(); ++it) {
				  ns.push_back(it->second);
			}
			std::sort(ns.begin(), ns.end(), CmpByDistance());

			report.child("states");
			for (int i = 0; i < ns.size() && i < MAX_STATES_IN_REPORT; i++) {
				std::stringstream sstr;
				sstr << i + 1 << ". final state";
				write_state(sstr.str(), ns[i], report);
			}
			report.back();
		}

		report.back();
		report.back();
	}
}

bool operator==(const ParikhVector &p1, const ParikhVector &p2) {
	if (p1.size() != p2.size()) {
		return false;
	}
	for (ParikhVector::const_iterator it = p1.begin(); it != p1.end(); it++) {
		ParikhVector::const_iterator it2 = p2.find(it->first);
		if (it2 == p2.end() || it->second != it2->second) {
			return false;
		}
	}
	return true;
}

bool operator!=(const ParikhVector &p1, const ParikhVector &p2) {
	return !(p1 == p2);
}

void Core::run_analysis_transition_occurrence(ca::Output &report)
{
	ArcCompare arcCmp(verif_configuration);
	ParikhVector *current, *next;
	std::queue<Node*> node_queue;
	std::vector<Node*> error_node2;
	Node *error_node = NULL;
	Node *node;

	current = new ParikhVector(arcCmp);
	const std::vector<ca::TransitionDef*> transitions = net_def->get_transition_defs();
	initial_node->set_data(current);
	node_queue.push(initial_node);

	while (!node_queue.empty() && error_node == NULL) {
		node = node_queue.front();
		node_queue.pop();
		current = (ParikhVector*)(node->get_data());
		const std::vector<NextNodeInfo> &nexts_nodes = node->get_nexts();
		for (int i = 0; i < nexts_nodes.size(); i++) {
			next = new ParikhVector(*current);
			if (nexts_nodes[i].action == ActionFire) {
				Arc arc(node, &nexts_nodes[i]);
				if (verif_configuration.is_transition_analyzed(nexts_nodes[i].data.fire.transition_id)) {
					ParikhVector::iterator i = next->find(arc);
					if (i == next->end()) {
						next->insert(std::pair<Arc, int>(arc, 1));
					} else {
						i->second += 1;
					}
				}
			}

			Node *n = nexts_nodes[i].node;
			ParikhVector *v = (ParikhVector*) n->get_data();
			if (v != NULL) {
				ParikhVector::const_iterator it;
				if (*v != *next) {
					error_node = n;
					error_node2.push_back(node);
					error_node2.push_back(n);
					delete next;
					break;
				}
				delete next;
			} else {
				nexts_nodes[i].node->set_data(next);
				node_queue.push(nexts_nodes[i].node);
			}
		}
	}

	NodeMap::const_iterator it;
	current = NULL;
	Node *current_node = NULL;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		ParikhVector *v = (ParikhVector*) node->get_data();
		if (v != NULL) {
			if (node->get_state()->get_quit_flag() && error_node == NULL) {
				if (current == NULL) {
					current_node = node;
					current = v;
					continue;
				}
				if (*current != *v) {
					error_node = current_node;
					error_node2.push_back(node);
				}
			}
			delete v;
			node->set_data(NULL);
		}
	}

	if (current) {
		delete current;
	}

	report.child("analysis");
	report.set("name", "Analysis of characteristic vectors");

	report.child("result");
	report.set("name", "Uniqueness of characteristic vector");
	if (error_node) {
		report.set("status", "fail");
		report.set("text", "Different characteristic vectors found");
		report.child("states");
		write_state("First witness path", error_node, report);
		write_suffix("Second witness path", error_node2, report);
		report.back();
	} else {
		report.set("status", "ok");
	}
	report.back();
	report.back();
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

void Core::run_analysis_cycle(ca::Output &report)
{
	set_tags(-1);
	std::stack<Node*> stack;
	stack.push(initial_node);
	initial_node->set_tag(0);

	std::vector<Node*> error_suffix;

	while (!stack.empty()) {
		Node *n = stack.top();
		int tag = n->get_tag();
		const std::vector<NextNodeInfo> &nexts = n->get_nexts();
		if (tag == nexts.size()) {
			n->set_tag(-2);
			stack.pop();
			continue;
		}
		n->set_tag(tag + 1);
		Node *next = nexts[tag].node;

		if (next->get_tag() == -1) {
			next->set_tag(0);
			stack.push(next);
			continue;
		}

		if (next->get_tag() >= 0) {
			error_suffix.push_back(n);
			error_suffix.push_back(next);
			break;
		}
	}

	report.child("analysis");
	report.set("name", "Cycle detection");

	report.child("result");
	report.set("name", "Cycle detection");
	if (!error_suffix.empty()) {
		report.set("status", "fail");
		report.set("text", "A cyclic computation detected");
		report.child("states");
		write_state("Start of the cycle", error_suffix[1], report);
		write_suffix("The full cycle", error_suffix, report);
		report.back();
	} else {
		report.set("status", "ok");
	}
	report.back();
	report.back();
}

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(MHASH_MD5);

	Node *node = get_node(hash);
	if (node == NULL) {
		node = new Node(hash, state, prev);
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

Node * Core::get_node(HashDigest digest) const
{
	NodeMap::const_iterator it = nodes.find(digest);
	if (it == nodes.end())
		return NULL;
	else
		return it->second;
}
