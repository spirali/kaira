
#include "statespace.h"
#include <string.h>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <alloca.h>
#include <omp.h>

namespace ca {
	extern ca::NetDef **defs;
}

using namespace cass;

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(MHASH_MD5);

	Node *node = get_node(hash);
	if (node == NULL) {
		node = new Node(hash, state, prev);
		node->set_quit(state->get_quit_flag());
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

Core::Core(int argc, char **argv, VerifConfiguration &verif_configuration, std::vector<ca::Parameter*> &parameters):
	nodes(10000, HashDigestHash(MHASH_MD5), HashDigestEq(MHASH_MD5)),
	initial_node(NULL),
	net_def(NULL),
	verif_configuration(verif_configuration),
	fullyEplored(0),
	partlyExplored(0),
	singleExplored(0)
{
	rank = 0;
	size = 0;
	data_buffer = NULL;
	if (cfg::analyse_transition_occurence) {
		generate_binging_in_nni = true;
	} else {
		generate_binging_in_nni = false;
	}
	if (cfg::debug) {
		debug_output.open((cfg::project_name + "_debug_out.txt").c_str(), std::ios::out | std::ios::trunc);
		debug_output << "Project: " << cfg::project_name << "\n";
		debug_output << "processes: " << ca::process_count << "\n";
		debug_output << "Parameters: ";
		for (size_t i = 0; i < parameters.size(); i++) {
			debug_output << parameters[i]->get_name() << ": " << parameters[i]->to_string();
			if (i != parameters.size() - 1) {
				debug_output << ", ";
			}
		}
		debug_output << "\n";
	}
}

void Core::generate()
{
	ca::check_parameters();

	double start = omp_get_wtime();

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
		node->generate(this);
		if (node->get_nexts().size() == 0 && cfg::analyse_final_marking) {
			node->set_final_marking(pack_marking(node));
		}
		if (node->get_quit_flag()) {
			delete node->get_state();
		}
	} while (!not_processed.empty());

	double end = omp_get_wtime();

	printf("total number of explored states: %ld\n", nodes.size());
	printf("    size(ample) = size(enable) : %ld\n", fullyEplored);
	printf("1 < size(ample) < size(enable) : %ld\n", partlyExplored);
	printf("1 = size(ample) < size(enable) : %ld\n", singleExplored);
	printf("\n");
	printf("total verification time        : %5.2f\n", end - start);
}

Core::~Core()
{
	NodeMap::iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++) {
		delete it->second;
	}
	if (cfg::debug) {
		debug_output.close();
	}
}


