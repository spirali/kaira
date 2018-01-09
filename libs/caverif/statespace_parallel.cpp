#include "statespace.h"
#include "vertices.h"

#include <cmath>
#include "mpi.h"
#include <omp.h>

#define ASSUMED_HASHES_SIZE 10
#define HASH_THRESHOLD 1024

namespace ca {
	extern ca::NetDef **defs;
}

using namespace cass;

Core::Core(int argc, char **argv, VerifConfiguration &verif_configuration, std::vector<ca::Parameter*> &parameters):
	hash_id(CASS_HASH_ID),
	nodes(10000, HashDigestHash(hash_id), HashDigestEq(hash_id)),
	initial_node(NULL),
	net_def(NULL),
	verif_configuration(verif_configuration),
	fullyEplored(0),
	partlyExplored(0),
	singleExplored(0),
	hashes(hash_id),
	receives(hash_id)
{
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

	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);

	HashVertex::rank = rank;

	hashes.resize(ASSUMED_HASHES_SIZE);
	receives.resize(ASSUMED_HASHES_SIZE * size);

	displacement.push_back(std::vector<int>(size));
	receive_count.push_back(std::vector<int>(size));
	threshold.resize(1);
	threshold[0] = HASH_THRESHOLD;
	next_end.resize(size, 0);
	next_receive.push_back(std::vector<int>(size, 0));
	next_threshold.push_back(0);

	processes.resize(size);
	for (int i = 0; i < size; i++) {
		processes[i].rank = i;
	}

	output = 0;
}

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(hash_id);

	Node *node = get_node(hash);
	if (node == NULL) {
		auto it = hash_processes.find(hash);
		if (it != hash_processes.end()) {
			node = it->second.node;
		}
	}
	if (node == NULL) {
		node = new Node(hash, state, prev);
		node->set_quit(state->get_quit_flag());
		//nodes[hash] = node;
		//not_processed.push(node);
		node->compute_ample(this);
		hash_processes.insert(
			std::pair<HashDigest, HashVertex>(hash, HashVertex(hash, node, node->get_ample_size()))
		);

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

	MPI_Barrier(MPI_COMM_WORLD);
	double start = omp_get_wtime();

	net_def = ca::defs[0]; // Take first definition
	State *initial_state = new State(net_def);
	initial_node = add_state(initial_state, NULL);

	// INIT TRANSFER DATA
	size_t count = 0;
	size_t last_level = 0;
	size_t part = 0;
	init_partition();

	std::stringstream ss;
	ss << rank << ".txt";

	if (cfg::debug) {
		debug_output.open(ss.str().c_str(), std::ios::out | std::ios::trunc);
	}

	do {
		count++;
		if (count % 1000 == 0 && !cfg::silent) {
//			fprintf(stderr, "==KAIRA== Nodes %ld\n", count);
		}
		Node *node = not_processed.front();
		not_processed.pop_front();
		node->fire_ample(this);
		if (node->get_nexts().size() == 0 && cfg::analyse_final_marking) {
			node->set_final_marking(pack_marking(node));
		}
		if (node->get_quit_flag()) {
			delete node->get_state();
		}

		if (not_processed.empty() || count - last_level == threshold[part]) {
			do {
				size_t size = receive_count.size();
				send_hashes(part);
				send_result(part);
				partitiate(part);
				if (++part == size) {
					part = 0;
				}
				last_level = count;
			} while(part > 0 && threshold[part] == 0);
		}

	} while (!not_processed.empty());

	// FINISH COOPERATION WITH OTHER PROCESSES
	while (displacement[part][size - 1] + receive_count[part][size - 1] > 0) {
		size_t size = receive_count.size();
		send_hashes(part);
		send_result(part);
		partitiate(part);
		if (++part == size) {
			part = 0;
			last_level = count;
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);
	double end = omp_get_wtime();

	debug_output.close();
	MPI_Finalize();

	if (rank == 0) {
		int sum = 0;
		std::cout << "nodes: ";
		for (int i = 0; i < size; i++) {
			std::cout << processes[i].realNodes << " ";
			sum += processes[i].realNodes;
		}
		std::cout << " = " << sum + 1 << "\n";

		printf("\n");
		printf("total verification time        : %5.2f\n", end - start);
	}
}


void Core::init_partition()
{
	HashVertex &nVertex = hash_processes.begin()->second;
	nodes[hash_processes.begin()->second.hash] = nVertex.node;
	not_processed.push_back(nVertex.node);

	for (int i = 0; i < size; i++) {
		displacement[0][i] = i * nVertex.ample_size;
		receive_count[0][i] = nVertex.ample_size;
	}
	hash_processes.clear();
}

void Core::send_hashes(size_t part)
{
	// Because displacement and receive_count are counted in hashes, while MPI counts in char
	int receive_size = 0;
	for (int i = 0; i < size; i++) {
		receive_size += receive_count[part][i];
		displacement[part][i] *= hashes.element_size();
		receive_count[part][i] *= hashes.element_size();
	}

	hashes.clear();
	std::map<HashDigest, HashVertex, HashDigestOrder>::const_iterator it;
	for (it = hash_processes.begin(); it != hash_processes.end(); ++it) {
		hashes.push(it->first, it->second.ample_size);
	}

	receives.resize(receive_size);
	MPI_Allgatherv(
		hashes[0], receive_count[part][rank], MPI_CHAR,
		receives[0], &receive_count[part][0], &displacement[part][0], MPI_CHAR,
		MPI_COMM_WORLD);

	// Return original sizes
	for (int i = 0; i < size; i++) {
		displacement[part][i] /= hashes.element_size();
		receive_count[part][i] /= hashes.element_size();
	}
}

unsigned char bits[8] = { 128, 64, 32, 16, 8, 4, 2, 1 };

void Core::send_result(size_t part)
{
	int mask_size = ceil((displacement[part][size - 1] + receive_count[part][size - 1]) / 8.);
	char* mask = new char[mask_size];
	char* results = new char[mask_size * size];

	// FILL MASK - 0 means a new hash
	memset(mask, 0, mask_size);
	memset(results, 0, mask_size * size);
	int byte, bit;
	byte = bit = 0;
	for (int i = 0; i < size; i++) {
		for (int j = 0; j < receive_count[part][i]; j++) {
			if (rank == i && hashes.size() <= (size_t)j) {
				mask[byte] |= bits[bit];
			} else if (get_node(receives[displacement[part][i] + j]) != NULL) {
				mask[byte] |= bits[bit];
			}
			(bit == 7) ? byte++, bit = 0 : bit++;
		}
	}

	// COMMUNICATE
	MPI_Allgather(
		mask, mask_size, MPI_CHAR,
		results, mask_size, MPI_CHAR,
		MPI_COMM_WORLD);

	// Put results from all processes together
	for (int i = 1; i < size; i++) {
		for (int j = 0; j < mask_size; j++) {
			results[j] |= results[i * mask_size + j];
		}
	}

	// COLECT RESULT
	for (size_t i = 0; i < processes.size(); i++) {
		processes[i].clear();
	}

	std::map<HashDigest, HashVertex, HashDigestOrder>::iterator it;
	byte = bit = 0;
	for (int i = 0; i < size; i++) {
		for (int j = 0; j < receive_count[part][i]; j++) {
			if (!(results[byte] & bits[bit])) {
				it = hash_processes.find(receives[displacement[part][i] + j]);
				if (it == hash_processes.end()) {
					it = hash_processes.insert(
						std::pair<HashDigest, HashVertex>(
							receives[displacement[part][i] + j],
							HashVertex(receives[displacement[part][i] + j], receives.ample_size(displacement[part][i] + j))
						)
					).first;
				}
				it->second.processes.push_back(&processes[i]);
				processes[i].add_vertex(&(it->second));
			}
			(bit == 7) ? byte++, bit = 0 : bit++;
		}
	}

	delete[] mask;
	delete[] results;
}

void Core::add_part(size_t part, size_t process, size_t node_size, size_t ample_size)
{
	if (next_receive.size() == part) {
		next_receive.push_back(std::vector<int>(size, 0));
		next_threshold.push_back(0);

	}
	if ((int)process == rank) {
		next_threshold[part] = node_size;
	}
	next_receive[part][process] = ample_size;
	next_end[process] = part;
}

void Core::partitiate(size_t part)
{
	// ASSIGN ALL HASHES TO PROCESSES WITH THE SMALLEST NUMBER OF STATES
	std::map<HashDigest, HashVertex, HashDigestOrder>::iterator it;
	for (it = hash_processes.begin(); it != hash_processes.end(); ++it) {
		it->second.assign();
	}

	// PROCESS WITHOUT HASH STEALS SOME FROM NEIGHBOUR
	for (int i = 0; i < size; i++) {
		processes[i].steal();
	}

	// PREPARE NEXT LEVEL
	int max_size = next_receive[next_end[0]][0] + processes[0].next_size;
	for (int i = 1; i < size; i++) {
		if (max_size < next_receive[next_end[i]][i] + processes[i].next_size) {
			max_size = next_receive[next_end[i]][i] + processes[i].next_size;
		}
	}

	if (max_size > HASH_THRESHOLD) {
		std::set<HashVertex*, HashVertexEq>::const_iterator it;
		for (int i = 0; i < size; i++) {
			size_t node_c = next_threshold[next_end[rank]], ample_c = next_receive[next_end[i]][i], next_part = next_end[i];
			for (it = processes[i].assigned.begin(); it != processes[i].assigned.end(); ++it) {
				if (ample_c + (*it)->ample_size > HASH_THRESHOLD) {
					add_part(next_part, i, node_c, ample_c);
					ample_c = (*it)->ample_size;
					node_c = 1;
					next_part++;
				} else {
					node_c++;
					ample_c += (*it)->ample_size;
				}
			}
			if (processes[i].assigned.size()) {
				add_part(next_part, i, node_c, ample_c);
			}
		}
	} else {
		next_threshold[next_end[rank]] += processes[rank].assigned.size();
		for (int i = 0; i < size; i++) {
			next_receive[next_end[i]][i] += processes[i].next_size;
		}
	}

	std::set<HashVertex*, HashVertexEq>::const_iterator vit;
	for (vit = processes[rank].assigned.begin(); vit != processes[rank].assigned.end(); ++vit) {
		nodes[(*vit)->hash] = (*vit)->node;
		not_processed.push_back((*vit)->node);
	}
	for (int r = 0; r < rank; r++) {
		for (vit = processes[r].assigned.begin(); vit != processes[r].assigned.end(); ++vit) {
			if ((*vit)->node != NULL && (*vit)->clear) {
				delete (*vit)->node->get_state();
				delete (*vit)->node;
			}
		}
	}
	for (int r = rank + 1; r < size; r++) {
		for (vit = processes[r].assigned.begin(); vit != processes[r].assigned.end(); ++vit) {
			if ((*vit)->node != NULL && (*vit)->clear) {
				delete (*vit)->node->get_state();
				delete (*vit)->node;
			}
		}
	}

	hash_processes.clear();
	if (receive_count.size() == part + 1) {
		receive_count.swap(next_receive);
		threshold.swap(next_threshold);

		next_receive.clear();
		next_receive.push_back(std::vector<int>(size, 0));
		next_threshold.clear();
		next_threshold.push_back(0);
		next_end.clear();
		next_end.resize(size, 0);
		displacement.resize(receive_count.size());
		for (size_t p = 0; p < receive_count.size(); p++) {
			displacement[p].resize(size);
			displacement[p][0] = 0;
			for (int i = 1; i < size; i++) {
				displacement[p][i] = displacement[p][i - 1] + receive_count[p][i - 1];
			}
			if (hashes.capacity() < (size_t)receive_count[p][rank]) {
				hashes.resize(receive_count[p][rank]);
			}
		}
	}
}

