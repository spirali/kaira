
#include "statespace.h"
#include <string.h>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <alloca.h>

namespace ca {
	extern ca::NetDef **defs;
	extern int defs_count;
	extern int process_count;
	extern char *project_description_string;
}

using namespace cass;

static bool write_dot = false;
static bool analyse_deadlock = false;
static bool analyse_transition_occurrence = false;
static std::string project_name;

static void args_callback(char c, char *optarg, void *data)
{
	if (c == 'V') {
		if (!strcmp(optarg, "dot")) {
			write_dot = true;
			return;
		}
		if (!strcmp(optarg, "deadlock")) {
			analyse_deadlock = true;
			return;
		}
		if (!strcmp(optarg, "transition_occurrence")) {
			analyse_transition_occurrence = true;
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

Node::Node(HashDigest hash, State *state)
	: hash(hash), state(state), prev(NULL), distance(0), data(NULL)
{

}

Node::~Node()
{
	free(hash);
}

void Node::set_prev(Node *node)
{
	if (prev == NULL || node->get_distance() + 1 < get_distance()) {
		prev = node;
		distance = node->get_distance() + 1;
	}
}

void Node::generate(Core *core)
{
	if (state->get_quit_flag()) {
		return;
	}

	ca::NetDef *def = core->get_net_def();
	const std::vector<ca::TransitionDef*> &transitions =
		def->get_transition_defs();
	int transitions_count = def->get_transitions_count();

	State *s = NULL;

	/* Full fire transition */
	for (int p = 0; p < ca::process_count; p++) {
			for (int i = 0; i < transitions_count; i++) {
				if (s == NULL) {
					s = new State(*state);
				}
				ca::Packer packer;
				bool fired;
				if (core->generate_binding_in_nni(transitions[i]->get_id())) {
					fired = s->fire_transition_full_with_binding(p, transitions[i], packer);
				} else {
					fired = s->fire_transition_full(p, transitions[i]);
				}
				if (fired) {
					Node *n = core->add_state(s);
					n->set_prev(this);
					NextNodeInfo nninfo;
					nninfo.node = n;
					nninfo.action = ActionFire;
					nninfo.data.fire.process_id = p;
					nninfo.data.fire.thread_id = 0;
					nninfo.data.fire.transition_id = transitions[i]->get_id();
					if (core->generate_binding_in_nni(transitions[i]->get_id())) {
						nninfo.data.fire.binding = core->hash_packer(packer);
					}
					nexts.push_back(nninfo);
					s = NULL;
				}
			}
		}

	/* we do not support threads
	// Fire transitions
	for (int p = 0; p < ca::process_count; p++) {
		for (int i = 0; i < transitions_count; i++) {
			if (s == NULL) {
				s = new State(*state);
			}
			bool fired = s->fire_transition_phase1(p, transitions[i]);
			if (fired) {
				Node *n = core->add_state(s);
				n->set_prev(this);
				NextNodeInfo nninfo;
				nninfo.node = n;
				nninfo.action = ActionFire;
				nninfo.data.fire.process_id = p;
				nninfo.data.fire.thread_id = 0;
				nninfo.data.fire.transition_id = transitions[i]->get_id();
				nexts.push_back(nninfo);
				s = NULL;
			}
		}
	}

	// Finish transitions
	std::vector<Activation> &activations = state->get_activations();
	for (int i = 0; i < activations.size(); i++)
	{
		if (s == NULL) {
			s = new State(*state);
		}
		s->finish_transition_ro_binding(i);
		Node *n = core->add_state(s);
		n->set_prev(this);
		NextNodeInfo nninfo;
		nninfo.node = n;
		nninfo.action = ActionFinish;
		nninfo.data.finish.process_id = activations[i].process_id;
		nninfo.data.finish.thread_id = activations[i].thread_id;
		nexts.push_back(nninfo);
		s = NULL;
	}
	*/

	/* Receive packets */
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++) {
		int target = pq / ca::process_count;
		int source = pq % ca::process_count;
		if (s == NULL) {
			s = new State(*state);
		}
		if (!s->receive(target, source, false)) {
			continue;
		}
		Node *n = core->add_state(s);
		n->set_prev(this);
		NextNodeInfo nninfo;
		nninfo.node = n;
		nninfo.action = ActionReceive;
		nninfo.data.receive.process_id = target;
		nninfo.data.receive.source_id = source;
		nexts.push_back(nninfo);
		s = NULL;
	}
	if (s != NULL) {
		delete s;
	}
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
	initial_node = add_state(initial_state);
	int count = 0;
	do {
		count++;
		if (count % 1000 == 0) {
			fprintf(stderr, "Nodes %i\n", count);
		}
		Node *node = not_processed.top();
		not_processed.pop();
		node->generate(this);
	} while (!not_processed.empty());
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
			fprintf(f, "S%p [style=filled, label=init]\n",
				node);
		} else {
			hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);
			hashstr[5] = 0;
			int packets_count = 0;
			for (int i = 0; i < ca::process_count * ca::process_count; i++) {
				packets_count += node->get_state()->get_packets(
					i / ca::process_count, i % ca::process_count).size();
			}
			fprintf(f, "S%p [label=\"%s|%i|%i\"]\n",
				node,
				hashstr,
				(int) node->get_state()->get_activations().size(),
				packets_count);
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

	if (analyse_deadlock) {
		run_analysis_deadlock(report);
	}
	if (analyse_transition_occurrence) {
		run_analysis_transition_occurrence(report);
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
			s << " S ";
			ca::TransitionDef *t = def->get_transition_def(nninfo.data.fire.transition_id);
			if (t->get_name().size() > 0) {
				s << t->get_name();
			} else {
				s << "#" << nninfo.data.fire.transition_id;
			}
			break;
		}
		case ActionFinish:
			s << nninfo.data.finish.process_id;
			s << " " << nninfo.data.finish.thread_id << " F";
			break;
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

	if(nodes.size() == 0) {
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

void Core::run_analysis_deadlock(ca::Output &report)
{
	size_t deadlocks = 0;
	Node* deadlock_node = NULL;

	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		if (node->get_nexts().size() == 0) {
			deadlocks++;
			if (deadlock_node == NULL ||
				deadlock_node->get_distance() > node->get_distance()) {
				deadlock_node = node;
			}
		}
	}
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

void Core::run_analysis_transition_occurrence(ca::Output &report)
{
	ArcCompare arcCmp(verif_configuration);
	ParikhVector *actual, *next;
	std::queue<Node*> node_queue;
	std::vector<Node*> second_sequence;
	Node *first_sequence;
	Node *node;
	int errors = 0;

	actual = new ParikhVector(arcCmp);
	const std::vector<ca::TransitionDef*> transitions = net_def->get_transition_defs();
	initial_node->set_data(actual);
	node_queue.push(initial_node);

	while (!node_queue.empty() && !errors) {
		node = node_queue.front();
		node_queue.pop();
		actual = (ParikhVector*)(node->get_data());
		const std::vector<NextNodeInfo> &nexts_nodes = node->get_nexts();
		for (int i = 0; i < nexts_nodes.size(); i++) {
			if (errors) {
				break;
			}
			next = new ParikhVector(*actual);
			if (nexts_nodes[i].action == ActionFire) {
				Arc arc(node, &nexts_nodes[i]);
				if (verif_configuration.is_transition_analyzed(nexts_nodes[i].data.fire.transition_id)) {
					if (next->count(arc)) {
						next->at(arc) += 1;
					} else {
						next->insert(std::pair<Arc, int>(arc, 1));
					}
				}
			}
			if (nexts_nodes[i].node->get_data() != NULL) {
				ParikhVector::const_iterator it;
				ParikhVector *old = (ParikhVector*)nexts_nodes[i].node->get_data();
				for (it = next->begin(); it != next->end(); it++) {
					if (old->count(it->first) == 0 || it->second != old->at(it->first)) {
						first_sequence = nexts_nodes[i].node;
						second_sequence.push_back(node);
						second_sequence.push_back(nexts_nodes[i].node);
						errors++;
						break;
					}
				}
				delete next;
			} else {
				nexts_nodes[i].node->set_data(next);
				node_queue.push(nexts_nodes[i].node);
			}
		}
	}

	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		if (node->get_data() != NULL) {
			delete (ParikhVector*)node->get_data();
			node->set_data(NULL);
		}
	}

	report.child("analysis");
	report.set("name", "Transition occurence analysis");

	report.child("result");
	report.set("name", "Number of errors");
	report.set("value", errors);
	if (errors > 0) {
		report.set("status", "fail");
		report.set("text", "Sequences with different transition occurence found");
		report.child("states");
		write_state("First witness path", first_sequence, report);
		write_suffix("Second witness path", second_sequence, report);
		report.back();
	} else {
		report.set("status", "ok");
	}
	report.back();
	report.back();
}

Node * Core::add_state(State *state)
{
	HashDigest hash = state->compute_hash(MHASH_MD5);

	Node *node = get_node(hash);
	if (node == NULL) {
		node = new Node(hash, state);
		nodes[hash] = node;
		not_processed.push(node);
		return node;
	} else {
		free(hash);
		delete state;
		return node;
	}
}

HashDigest Core::hash_packer(ca::Packer packer)
{
	MHASH hash_thread = mhash_init(MHASH_MD5);
	if (hash_thread == MHASH_FAILED) {
		fprintf(stderr, "Hash failed\n");
		exit(1);
	}
	mhash(hash_thread, packer.get_buffer(), packer.get_size());
	return mhash_end(hash_thread);
}

const NextNodeInfo& Node::get_next_node_info(Node *node)
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
