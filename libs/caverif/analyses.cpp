#include "statespace.h"

static size_t const MAX_STATES_IN_REPORT = 5;

namespace ca {
	extern char *project_description_string;
}

using namespace cass;

bool cfg::write_dot = false;
bool cfg::write_statespace = false;
bool cfg::analyse_deadlock = false;
bool cfg::analyse_transition_occurence = false;
bool cfg::analyse_final_marking = false;
bool cfg::analyse_cycle = false;
bool cfg::partial_order_reduction = true;
bool cfg::silent = false;
bool cfg::debug = false;
std::string cfg::project_name;

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
			cfg::write_dot = true;
			return;
		}

		if (!strncmp(optarg, "debug", 5)) {
			cfg::debug = true;
			return;
		}
		if (!strcmp(optarg, "deadlock")) {
			cfg::analyse_deadlock = true;
			return;
		}
		if (!strcmp(optarg, "tchv")) {
			cfg::analyse_transition_occurence = true;
			return;
		}
		if (!strcmp(optarg, "fmarking")) {
			cfg::analyse_final_marking = true;
			return;
		}
		if (!strcmp(optarg, "cycle")) {
			cfg::analyse_cycle = true;
			return;
		}
		if (!strcmp(optarg, "disable-por")) {
			cfg::partial_order_reduction = false;
			return;
		}
		if (!strcmp(optarg, "silent")) {
			cfg::silent = true;
			return;
		}
		if (!strcmp(optarg, "write-statespace")) {
			cfg::write_statespace = true;
			return;
		}

		fprintf(stderr, "Invalid argument in -V\n");
		exit(1);
	}
}

void cass::init(int argc, char **argv, std::vector<ca::Parameter*> &parameters, bool tracing)
{
	char s[1024];
	strncpy(s, argv[0], 1024);
	cfg::project_name = basename(s); // basename can modify its argument
	init(argc, argv, parameters, tracing, "V:", args_callback);
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
			if (cfg::analyse_final_marking) {
				NodeMap::const_iterator n = final_markings.find(node->get_final_marking());
				if (n != final_markings.end()) {
					if (n->second->get_distance() > node->get_distance()) {
						final_markings[node->get_final_marking()] = node;
					}
				} else {
					final_markings[node->get_final_marking()] = node;
				}
			}
			if (cfg::analyse_deadlock) {
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

	if (cfg::analyse_deadlock) {
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

	if (cfg::analyse_final_marking) {
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


void Core::postprocess()
{
	if (cfg::write_dot) {
		write_dot_file("statespace.dot");
	}

	write_report();
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



void Core::write_report()
{
	FILE *f = fopen((cfg::project_name + ".kreport").c_str(), "w");
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

	if (cfg::analyse_deadlock || cfg::analyse_final_marking) {
		run_analysis_final_nodes(report);
	}
	if (cfg::analyse_transition_occurence) {
		run_analysis_transition_occurrence(report);
	}
	if (cfg::analyse_cycle) {
		run_analysis_cycle(report);
	}

	report.child("description");
	report.text(ca::project_description_string);
	report.back();

	if (cfg::write_statespace) {
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

static void write_control_line(ca::NetDef *def, std::stringstream &s, const NextNodeInfo &nninfo)
{
	switch (nninfo.action) {
		case ActionFire: {
			s << nninfo.data.fire.process_id;
			s << " T ";
			ca::TransitionDef *t = def->get_transition_def(nninfo.data.fire.transition_id);
			if (t->get_name().size() > 0) {
				std::string name = t->get_name();
				std::replace(name.begin(), name.end(), '\n', '_');
				s << name;
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
