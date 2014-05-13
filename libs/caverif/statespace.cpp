
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

static size_t const MAX_STATES_IN_REPORT = 5;

using namespace cass;

static bool cfg_write_dot = false;
static bool cfg_write_statespace = false;
static bool cfg_analyse_deadlock = false;
static bool cfg_analyse_transition_occurence = false;
static bool cfg_analyse_final_marking = false;
static bool cfg_analyse_cycle = false;
static bool cfg_partial_order_reduction = true;
static bool cfg_silent = false;
static bool cfg_debug = false;
static bool cfg_wait_for_key = false;
static std::string cfg_interesting_state_prefix;
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
			cfg_write_dot = true;
			return;
		}

		if (!strncmp(optarg, "debug", 5)) {
			cfg_debug = true;
			std::string str(optarg);
			if (str.length() == 5) {
				cfg_wait_for_key = true;
				return;
			}
			if (str.length() > 6 && str[5] == '=') {
				cfg_interesting_state_prefix = str.substr(6);
				cfg_wait_for_key = false;
				return;
			}
		}
		if (!strcmp(optarg, "deadlock")) {
			cfg_analyse_deadlock = true;
			return;
		}
		if (!strcmp(optarg, "tchv")) {
			cfg_analyse_transition_occurence = true;
			return;
		}
		if (!strcmp(optarg, "fmarking")) {
			cfg_analyse_final_marking = true;
			return;
		}
		if (!strcmp(optarg, "cycle")) {
			cfg_analyse_cycle = true;
			return;
		}
		if (!strcmp(optarg, "disable-por")) {
			cfg_partial_order_reduction = false;
			return;
		}
		if (!strcmp(optarg, "silent")) {
			cfg_silent = true;
			return;
		}
		if (!strcmp(optarg, "write-statespace")) {
			cfg_write_statespace = true;
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

void cass::init(int argc, char **argv, std::vector<ca::Parameter*> &parameters, bool tracing)
{
	char s[1024];
	strncpy(s, argv[0], 1024);
	project_name = basename(s); // basename can modify its argument
	init(argc, argv, parameters, tracing, "V:", args_callback);
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
	if (cfg_partial_order_reduction) {
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
	if (cfg_analyse_transition_occurence) {
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

void Core::generate()
{
	ca::check_parameters();

	net_def = ca::defs[0]; // Take first definition
	State *initial_state = new State(net_def);
	initial_node = add_state(initial_state, NULL);
	int count = 0;
	do {
		count++;
		if (count % 1000 == 0 && !cfg_silent) {
			fprintf(stderr, "==KAIRA== Nodes %i\n", count);
		}
		Node *node = not_processed.top();
		not_processed.pop();
		if (cfg_debug && cfg_wait_for_key == false) {
			char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
			hashdigest_to_string(MHASH_MD5, node->get_state()->compute_hash(MHASH_MD5) , hashstr);
			if (!strncmp(hashstr, cfg_interesting_state_prefix.c_str(), cfg_interesting_state_prefix.size())) {
				cfg_wait_for_key = true;
			}
		}
		node->generate(this);
		if (cfg_wait_for_key) {
			getchar();
		}
		if (node->get_nexts().size() == 0 && cfg_analyse_final_marking) {
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

	if (cfg_debug) {
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
		if (cfg_debug) {
			printf("  > ample: { ");
			for (ActionSet::iterator i = ample.begin(); i != ample.end(); i++) {
				printf("%s ", i->to_string().c_str());
			}
			printf("}\n");
		}
		if (check_C1(enable, ample, s) && check_C2(ample) && check_C3(s)) {
			if (cfg_debug) {
				printf("  This ample set is independent.\n");
			}
			return ample;
		}
		if (cfg_wait_for_key) {
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

	if (cfg_debug) {
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
				if (cfg_debug) {
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
						if (cfg_debug) {
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
						if (cfg_debug) {
							printf(" %s", a.to_string().c_str());
						}
					}
				}
				continue;
			}
			processed.insert(*i);
			queue.push_back(*i);
			if (cfg_debug) {
				printf(" %s", i->to_string().c_str());
			}
		}
	}

	if (cfg_debug) {
		printf(" }\n");
	}

	while(queue.size() > 0) {
		for (ActionSet::iterator a = ample.begin(); a != ample.end(); a++) {
			if (verif_configuration.is_dependent(*a, queue.front(), marking)) {
				if (cfg_debug) {
					printf("    C1: %s and %s are dependent.\n", a->to_string().c_str(), queue.front().to_string().c_str());
				}
				return false;
			}
		}
		if (cfg_debug) {
			size_t i = queue.size();
			verif_configuration.compute_successors(queue.front(), queue, processed, receive_blocked, enabled_priorities, marking);
			printf("    C1: successors of %s: {", queue.front().to_string().c_str());
			for (; i < queue.size(); i++) {
				printf(" %s", queue[i].to_string().c_str());
			}
			printf(" }\n");
		} else {
			verif_configuration.compute_successors(queue.front(), queue, processed, receive_blocked, enabled_priorities, marking);
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
				if (cfg_debug) {
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
		hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);
		hashstr[5] = 0; // Take just prefix
		const char *quit_flag;
		if (node->get_quit_flag()) {
			quit_flag = "!";
		} else {
			quit_flag = "";
		}
		fprintf(f, "S%p [label=\"%s%s|%i\"]\n",
			node,
			quit_flag,
			hashstr,
			node->get_distance());
		for (size_t i = 0; i < nexts.size(); i++) {
			fprintf(f, "S%p -> S%p [label=\"", node, nexts[i].node);
			switch(nexts[i].action) {
				case ActionFire:
					fprintf(f, "s %i %i\"]\n",
						nexts[i].data.fire.process_id,
						nexts[i].data.fire.transition_id);
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
	if (cfg_write_dot) {
		write_dot_file("statespace.dot");
	}

	write_report();
}

void Core::write_report()
{
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

	if (cfg_analyse_deadlock || cfg_analyse_final_marking) {
		run_analysis_final_nodes(report);
	}
	if (cfg_analyse_transition_occurence) {
		run_analysis_transition_occurrence(report);
	}
	if (cfg_analyse_cycle) {
		run_analysis_cycle(report);
	}

	report.child("description");
	report.text(ca::project_description_string);
	report.back();

	if (cfg_write_statespace) {
		write_xml_statespace(report);
	}

	report.back();
	fclose(f);
}

void Core::write_xml_statespace(ca::Output &report)
{
	char *hashstr = (char*) alloca(mhash_get_block_size(MHASH_MD5) * 2 + 1);
	report.child("statespace");
	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		report.child("state");
		Node *node = it->second;
		hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);
		report.set("hash", hashstr);
		if (initial_node == node) {
			report.set("initial", true);
		}
		if (node->get_quit_flag()) {
			report.set("quit", true);
		}

		const std::vector<NextNodeInfo> &nexts = node->get_nexts();
		for (size_t i = 0; i < nexts.size(); i++) {
			report.child("child");
			hashdigest_to_string(MHASH_MD5, nexts[i].node->get_hash(), hashstr);
			report.set("hash", hashstr);
			switch(nexts[i].action) {
				case ActionFire:
					report.set("action", "fire");
					report.set("process", nexts[i].data.fire.process_id);
					report.set("transition", nexts[i].data.fire.transition_id);
					break;
				case ActionReceive:
					report.set("action", "receive");
					report.set("process", nexts[i].data.receive.process_id);
					report.set("source", nexts[i].data.receive.source_id);
					break;
			}
			report.back();
		}
		report.back();
	}
	report.back();
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
			s << " R " << nninfo.data.receive.source_id;
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
			if (cfg_analyse_final_marking) {
				NodeMap::const_iterator n = final_markings.find(node->get_final_marking());
				if (n != final_markings.end()) {
					if (n->second->get_distance() > node->get_distance()) {
						final_markings[node->get_final_marking()] = node;
					}
				} else {
					final_markings[node->get_final_marking()] = node;
				}
			}
			if (cfg_analyse_deadlock) {
				if (!node->get_quit_flag()) {
					deadlocks++;
					if (deadlock_node == NULL ||
						deadlock_node->get_distance() > node->get_distance()) {
						deadlock_node = node;
					}
				}
			}
		}
	}

	if (cfg_analyse_deadlock) {
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

	if (cfg_analyse_final_marking) {
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
			for (size_t i = 0; i < ns.size() && i < MAX_STATES_IN_REPORT; i++) {
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
		for (size_t i = 0; i < nexts_nodes.size(); i++) {
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
			if (node->get_quit_flag() && error_node == NULL) {
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
		if (tag == static_cast<int>(nexts.size())) {
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
