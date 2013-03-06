
#include "statespace.h"
#include <string.h>
#include <vector>
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
static bool analyse_quit = false;
static std::string project_name;

static void args_callback(char c, char *optarg, void *data)
{
	if (c == 'V') {
		if (!strcmp(optarg, "dot")) {
			write_dot = true;
			return;
		}
		if (!strcmp(optarg, "quit")) {
			analyse_quit = true;
			return;
		}
		fprintf(stderr, "Invalid argument in -V\n");
		exit(1);
	}
}

const char hex_chars[16] = { '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

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

void cass::init(int argc, char **argv, std::vector<ca::Parameter*> &parameters)
{
	char s[1024];
	strncpy(s, argv[0], 1024);
	project_name = basename(s); // basename can modify its argument
	init(argc, argv, parameters, "V:", args_callback);
}

State::State(ca::NetDef *net_def) : flag(NULL)
{
	nets = new Net*[ca::process_count];
	packets = new std::deque<Packet>[ca::process_count * ca::process_count];
	for (int i = 0; i < ca::process_count; i++) {
		Thread thread(packets, i, 0);
		nets[i] = (Net*) net_def->spawn(&thread);
	}
}

State::State() : flag(NULL)
{
	nets = new Net*[ca::process_count];
	packets = new std::deque<Packet>[ca::process_count * ca::process_count];
}

State::State(const std::vector<TransitionActivation> & activations, std::deque<Packet> *packets)
	: activations(activations), flag(NULL)
{
	nets = new Net*[ca::process_count];
	this->packets = new std::deque<Packet>[ca::process_count * ca::process_count];
	for (int t = 0; t < ca::process_count * ca::process_count; t++) {
		this->packets[t] = packets[t];
	}
}

State::~State()
{
	delete [] nets;
	delete [] packets;
	NodeFlag *f = flag;
	while (f) {
		NodeFlag *next = f->next;
		delete f;
		f = next;
	}
}

State * State::copy()
{
	State *state = new State(activations, packets);
	for (int i = 0; i < ca::process_count; i++) {
		state->nets[i] = nets[i]->copy();
	}
	return state;
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
	std::vector<TransitionActivation>::iterator i;
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
	std::vector<TransitionActivation>::iterator i;
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

void State::add_flag(const std::string &name, const std::string &value)
{
	NodeFlag *f = new NodeFlag;
	f->name = name;
	f->value = value;
	f->next = flag;
	flag = f;
}

NodeFlag *State::get_flag(const std::string &name)
{
	NodeFlag *f = flag;
	while(f) {
		if (f->name == name) {
			return f;
		}
		f = f->next;
	}
	return NULL;
}

Node::Node(HashDigest hash, State *state) : hash(hash), state(state)
{

}

Node::~Node()
{
	free(hash);
}

void Node::generate(Core *core)
{
	if (state->get_flag("quit")) {
		return;
	}

	ca::NetDef *def = core->get_net_def();
	const std::vector<ca::TransitionDef*> &transitions =
		def->get_transition_defs();
	int transitions_count = def->get_transitions_count();

	State *s = NULL;
	/* Fire transitions */
	for (int p = 0; p < ca::process_count; p++) {
		for (int i = 0; i < transitions_count; i++) {
			if (s == NULL) {
				s = state->copy();
			}
			Net *net = s->nets[p];
			Thread thread(s->packets, p, 1);
			void *binding = transitions[i]->fire_phase1(&thread, net);
			if (binding) {
				TransitionActivation activation;
				activation.binding = binding;

				activation.transition_def = transitions[i];
				activation.process_id = p;
				activation.thread_id = 0;

				ca::Packer packer;
				transitions[i]->pack_binding(packer, binding);
				activation.packed_binding = packer.get_buffer();
				activation.packed_binding_size = packer.get_size();
				s->activations.push_back(activation);
				Node *n = core->add_state(s);
				nexts.push_back(n);
				s = NULL;
			}
		}
	}

	/* Finish transitions */
	std::vector<TransitionActivation>::iterator i;
	int p = 0;
	for (i = state->activations.begin(); i != state->activations.end(); i++, p++)
	{
		if (s == NULL) {
			s = state->copy();
		}
		s->activations.erase(s->activations.begin() + p);
		Net *net = s->nets[i->process_id];
		Thread thread(s->packets, i->process_id, i->thread_id);
		i->transition_def->fire_phase2(&thread, net, i->binding);
		if (thread.get_quit_flag()) {
			s->add_flag("quit", "");
		}
		Node *n = core->add_state(s);
		nexts.push_back(n);
		s = NULL;
	}

	/* Receive packets */
	for (int pq = 0; pq < ca::process_count * ca::process_count; pq++) {
		int target = pq / ca::process_count;
		if (state->packets[pq].empty()) {
			continue;
		}
		if (s == NULL) {
			s = state->copy();
		}
		Packet packet = s->packets[pq].front();
		s->packets[pq].pop_front();
		ca::Tokens *tokens = (ca::Tokens *) packet.data;
		ca::Unpacker unpacker(tokens + 1);
		Net *net = s->nets[target];
		int place_index = tokens->place_index;
		int tokens_count = tokens->tokens_count;
		Thread thread(s->packets, -1, -1);
		for (int t = 0; t < tokens_count; t++) {
			net->receive(&thread, packet.from_process, place_index, unpacker);
		}
		Node *n = core->add_state(s);
		nexts.push_back(n);
		s = NULL;
	}
	if (s != NULL) {
		delete s;
	}
}

Core::Core() : initial_node(NULL), nodes(10000, HashDigestHash(MHASH_MD5), HashDigestEq(MHASH_MD5))
{
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
		const std::vector<Node*> &nexts = node->get_nexts();
		if (node == initial_node) {
			fprintf(f, "S%p [style=filled, label=init]\n",
				node);
		} else {
			hashdigest_to_string(MHASH_MD5, node->get_hash(), hashstr);

			/*
			ca::Packer packer;
			node->get_state()->pack_state(packer);
			char xtmp[1000];
			snprintf(xtmp, 1000, "hashes/%s", hashstr);
			packer.write_to_file(xtmp);
			*/

			hashstr[5] = 0;
			int packets_count = 0;
			for (int i = 0; i < ca::process_count * ca::process_count; i++) {
				packets_count += node->get_state()->get_packets()[i].size();
			}
			fprintf(f, "S%p [label=\"%s|%li|%i\"]\n",
				node,
				hashstr,
				node->get_state()->get_activations().size(),
				packets_count);
		}
		for (size_t i = 0; i < nexts.size(); i++) {
			fprintf(f, "S%p -> S%p\n", node, nexts[i]);
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

	if (analyse_quit) {
		run_analysis_quit(report);
	}

	report.child("description");
	report.text(ca::project_description_string);
	report.back();

	report.back();
	fclose(f);
}

void Core::run_analysis_quit(ca::Output &report)
{
	size_t quit_states = 0;
	size_t deadlocks = 0;

	NodeMap::const_iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++)
	{
		Node *node = it->second;
		if (node->get_state()->get_flag("quit")) {
			quit_states++;
		} else if (node->get_nexts().size() == 0) {
			deadlocks++;
		}
	}
	report.child("analysis");
	report.set("name", "Quit analysis");

	report.child("result");
	report.set("name", "Number of quit states");
	report.set("value", quit_states);
	if (quit_states == 0) {
		report.set("status", "fail");
		report.set("text", "There is no quit-state");
	} else {
		report.set("status", "ok");
	}
	report.back();

	report.child("result");
	report.set("name", "Number of deadlock states");
	report.set("value", deadlocks);
	if (deadlocks != 0) {
		report.set("status", "fail");
		report.set("text", "There are deadlocks");
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

int Thread::get_process_count() const
{
	return ca::process_count;
}

void Thread::multisend_multicast(const std::vector<int> &targets,
								 ca::NetBase *net,
								 int place_index,
								 int tokens_count,
								 const ca::Packer &packer)
{
	std::vector<int>::const_iterator i;
	ca::Tokens *data = (ca::Tokens*) packer.get_buffer();
	data->place_index = place_index;
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
		packets[target * ca::process_count + process_id].push_back(packet);
	}
}
