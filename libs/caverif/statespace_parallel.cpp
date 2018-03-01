#include "statespace.h"
#include "vertices.h"

#include <cmath>
#include "mpi.h"
#include <omp.h>
#include <iomanip>

#define ASSUMED_HASHES_SIZE 1024

namespace ca {
	extern ca::NetDef **defs;
}

using namespace cass;

Core::Core(int argc, char **argv, VerifConfiguration &verif_configuration, std::vector<ca::Parameter*> &parameters):
	hash_id(CASS_HASH_ID),
	nodes(10000, HashDigestHash(), HashDigestEq()),
	initial_node(NULL),
	net_def(NULL),
	verif_configuration(verif_configuration),
	cycle_size(0),
	fullyEplored(0),
	partlyExplored(0),
	singleExplored(0),
	hashes(hash_id),
	receives(hash_id),
	realNodes(0),
	exchanged(0)
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

	displacement = std::vector<int>(size);
	receive_count = std::vector<int>(size);

	pvertices.resize(size);
	for (int i = 0; i < size; i++) {
		pvertices[i].rank = i;
	}

	output = 0;
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

	MPI_Finalize();
}

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(hash_id);

	Node *node = get_node(hash);
	if (node == NULL) {
		if (currentNodes.find(hash) != currentNodes.end()) {
			node = currentNodes[hash];
		}
	}
	if (node == NULL) {
		node = new Node(hash, state, prev);
		node->set_quit(state->get_quit_flag());
		//nodes[hash] = node;
		//not_processed.push(node);
		node->compute_ample(this);
		currentNodes[hash] = node;
		return node;
	} else {
		cycle_starts.push_back(node);
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

		if (not_processed.empty()) {
			send_hashes();
			send_result();
			partitiate();
		}

	} while (!not_processed.empty());

	// FINISH COOPERATION WITH OTHER PROCESSES
	while (displacement[size - 1] + receive_count[size - 1] > 0) {
		send_hashes();
		send_result();
		partitiate();
	}

	MPI_Barrier(MPI_COMM_WORLD);
	double end = omp_get_wtime();

	if (rank == 0) {
		std::cout << "nodes: ";
		for (int i = 0; i < size; i++) {
			std::cout << pvertices[i].realNodes << " ";
		}
		std::cout << "= " << realNodes + 1 << " (" << exchanged << " exchanged)\n";

		printf("\n");
		printf("total verification time        : %5.2f\n", end - start);
	}
}


void Core::init_partition()
{
	nodes[currentNodes.begin()->first] = currentNodes.begin()->second;
	not_processed.push_back(currentNodes.begin()->second);

	for (int i = 0; i < size; i++) {
		displacement[i] = i * currentNodes.begin()->second->get_ample_size();
		receive_count[i] = currentNodes.begin()->second->get_ample_size();
	}
	currentNodes.clear();
	realNodes = 1;
}

void Core::send_hashes()
{
	// Because displacement and receive_count are counted in hashes, while MPI counts in char
	int receive_size = 0;
	for (int i = 0; i < size; i++) {
		receive_size += receive_count[i];
		displacement[i] *= hashes.element_size();
		receive_count[i] *= hashes.element_size();
	}

	hashes.clear();
	for (auto it = currentNodes.begin(); it != currentNodes.end(); ++it) {
		hashes.push(it->second->get_hash(), it->second->get_ample_size());
	}

	receives.resize(receive_size);
	MPI_Allgatherv(
		hashes[0], receive_count[rank], MPI_CHAR,
		receives[0], &receive_count[0], &displacement[0], MPI_CHAR,
		MPI_COMM_WORLD);

	// Return original sizes
	for (int i = 0; i < size; i++) {
		displacement[i] /= hashes.element_size();
		receive_count[i] /= hashes.element_size();
	}
}

unsigned char bits[8] = { 128, 64, 32, 16, 8, 4, 2, 1 };

void Core::send_result()
{
	int mask_size = ceil((displacement[size - 1] + receive_count[size - 1]) / 8.);
	char* mask = new char[mask_size];
	char* results = new char[mask_size * size];

	// FILL MASK - 0 means a new hash
	memset(mask, 0, mask_size);
	memset(results, 0, mask_size * size);
	int byte, bit;
	byte = bit = 0;
	for (int i = 0; i < size; i++) {
		for (int j = 0; j < receive_count[i]; j++) {
			if (rank == i && hashes.size() <= (size_t)j) {
				mask[byte] |= bits[bit];
			} else if (get_node(receives[displacement[i] + j]) != NULL) {
				mask[byte] |= bits[bit];
			}
			(bit == 7) ? byte++, bit = 0 : bit++;
		}
	}

	// COMMUNICATE
	MPI_Allgather(mask, mask_size, MPI_CHAR, results, mask_size, MPI_CHAR, MPI_COMM_WORLD);

	// Put results from all processes together
	for (int i = 1; i < size; i++) {
		for (int j = 0; j < mask_size; j++) {
			results[j] |= results[i * mask_size + j];
		}
	}
	// COLECT RESULT
	for (size_t i = 0; i < pvertices.size(); i++) {
		pvertices[i].clear();
	}

	byte = bit = 0;
	for (int i = 0; i < size; i++) {
		for (int j = 0; j < receive_count[i]; j++) {
			if (!(results[byte] & bits[bit])) {
				if (i == rank) {
					hvertices.push_back(HashVertex(currentNodes[receives[displacement[i] + j]]->get_hash(), currentNodes[receives[displacement[i] + j]], receives.ample_size(displacement[i] + j)));
				} else {
					hvertices.push_back(HashVertex(receives[displacement[i] + j], receives.ample_size(displacement[i] + j)));
				}
				hvertices.back().processes.push_back(&pvertices[i]);
			}
			(bit == 7) ? byte++, bit = 0 : bit++;
		}
	}
	currentNodes.clear();

	if (hvertices.size()) {
		std::sort(hvertices.begin(), hvertices.end(), [&] (HashVertex &h1, HashVertex &h2) {
			if (h1.ample_size == h2.ample_size) {
				return hashOrder(h1.hash, h2.hash);
			}
			return h1.ample_size < h2.ample_size;
		});
		size_t unique = 0;
		for (size_t d = 1; d < hvertices.size(); d++) {
			if (!hashEq(hvertices[unique].hash, hvertices[d].hash)) {
				hvertices[++unique] = hvertices[d];
			} else {
				hvertices[unique].processes.push_back(hvertices[d].processes.back());
				if (hvertices[unique].node == NULL) {
					hvertices[unique].node = hvertices[d].node;
					hvertices[unique].hash = hvertices[d].hash;
				}
			}
		}
		hvertices.resize(unique + 1);
	}

	for (size_t i = 0; i < hvertices.size(); i++) {
		std::sort(hvertices[i].processes.begin(), hvertices[i].processes.end(), [] (ProcessVertex *p1, ProcessVertex *p2) { return p1->rank < p2->rank; });
		for (auto it = hvertices[i].processes.begin(); it != hvertices[i].processes.end(); ++it) {
			(*it)->hashes.push_back(&hvertices[i]);
		}
	}

	delete[] mask;
	delete[] results;
}

void Core::partitiate()
{
	long limit = 0;
	for (auto it = hvertices.begin(); it != hvertices.end(); ++it) {
		limit += it->ample_size;
	}
	if (hvertices.size()) {
		limit = limit / size + limit / hvertices.size();
	} else {
		limit = limit / size;
	}
	std::vector<int> potential(size);

	// ASSIGN ALL HASHES TO PROCESSES WITH THE SMALLEST NUMBER OF STATES
//	for (auto it = hvertices.begin(); it != hvertices.end(); ++it) {
//		it->assign();
//	}

//	std::vector<int> exchanged(size * size), esize(size * size);
	std::vector<HashVertex*> postponed;
	for (auto it = hvertices.begin(); it != hvertices.end(); ++it) {
		auto p = *std::min_element(it->processes.begin(), it->processes.end(), [] (ProcessVertex *p1, ProcessVertex *p2) { return p1->realNodes < p2->realNodes; });
		if (cfg::balance && potential[p->rank] + it->ample_size > limit) {
			postponed.push_back(&*it);
		} else {
			potential[p->rank] += (int)it->ample_size;
			it->owner = p;
			p->assign(&(*it));
			p->realNodes++;
			if (it->node != NULL) {
				for (size_t i = 0; i < it->node->get_prev()->get_nexts().size(); i++) {
					if (it->node->get_prev()->get_nexts()[i].node == it->node) {
						it->node->get_prev()->get_nexts()[i].rank = p->rank;
						break;
					}
				}
			}
		}
	}

	for (size_t i = 0; i < postponed.size(); i++) {
		auto p = *std::min_element(postponed[i]->processes.begin(), postponed[i]->processes.end(), [] (ProcessVertex *p1, ProcessVertex *p2) { return p1->realNodes < p2->realNodes; });
		if (cfg::balance && potential[p->rank] + postponed[i]->ample_size > limit) {
			auto target = std::min_element(pvertices.begin(), pvertices.end(), [&] (ProcessVertex &p1, ProcessVertex &p2) { return potential[p1.rank] < potential[p2.rank]; });
			if (p->rank != target->rank && potential[target->rank] + postponed[i]->ample_size <= limit) {
				++exchanged;
				Node *n;
				int distance, nni;
				if (p->rank == rank) {
					std::vector<char> data;
					distance = postponed[i]->node->get_distance();
					n = postponed[i]->node->get_prev();
					for (size_t j = 0; j < n->get_nexts().size(); j++) {
						if (n->get_nexts()[j].node == postponed[i]->node) {
							n->get_nexts()[j].rank = target->rank;
							nni = j;
							break;
						}
					}
					data.insert(data.end(), reinterpret_cast<const char*>(&n), reinterpret_cast<const char*>(&n) + sizeof(n));
					data.insert(data.end(), reinterpret_cast<const char*>(&distance), reinterpret_cast<const char*>(&distance) + sizeof(distance));
					data.insert(data.end(), reinterpret_cast<const char*>(&nni), reinterpret_cast<const char*>(&nni) + sizeof(nni));
					postponed[i]->node->get_state()->serialize(data);
					MPI_Send(data.data(), data.size(), MPI_BYTE, target->rank, 0, MPI_COMM_WORLD);
				}
				if (target->rank == rank) {
					MPI_Status status;
					std::vector<char> data;
					int size;
					MPI_Probe(p->rank, 0, MPI_COMM_WORLD, &status);
					MPI_Get_count(&status, MPI_BYTE, &size);
					data.resize(size);
					MPI_Recv(data.data(), size, MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					memcpy(&n, data.data(), sizeof(n));
					memcpy(&distance, data.data() + sizeof(n), sizeof(distance));
					memcpy(&nni, data.data() + sizeof(n) + sizeof(distance), sizeof(nni));
					State *state = new State(ca::defs[0]);
					state->deserialize(data.data() + sizeof(Node*) + sizeof(distance) + sizeof(nni));

					postponed[i]->hash = state->compute_hash(hash_id);
					postponed[i]->node = new Node(postponed[i]->hash, state, n, distance, nni);
					postponed[i]->node->set_quit(state->get_quit_flag());
					postponed[i]->node->compute_ample(this);
					postponed[i]->node->set_prev_rank(status.MPI_SOURCE);
				}
				p = &(*target);
			}
		}
		potential[p->rank] += (int)postponed[i]->ample_size;
		p->assign(&(*postponed[i]));
		p->realNodes++;
	}

	// PROCESS WITHOUT HASH STEALS SOME FROM NEIGHBOUR
//	for (int i = 0; i < size; i++) {
//		pvertices[i].steal();
//	}
	for (int i = 0; i < size; i++) {
		if (pvertices[i].assigned.size() == 0 && pvertices[i].hashes.size()) {
			pvertices[i].assign(pvertices[i].hashes.back());
		}
	}

	for (int i = 0; i < size; i++) {
		receive_count[i] = pvertices[i].next_size;
	}

	for (size_t i = 0; i < pvertices[rank].assigned.size(); i++) {
		nodes[pvertices[rank].assigned[i]->hash] = pvertices[rank].assigned[i]->node;
		not_processed.push_back(pvertices[rank].assigned[i]->node);
		pvertices[rank].assigned[i]->clear = 0;
	}

	for (auto it = hvertices.begin(); it != hvertices.end(); ++it) {
		if (it->clear && it->node != NULL) {
			for (size_t i = cycle_size; i < cycle_starts.size(); i++) {
				if (cycle_starts[i] == it->node) {
					cycle_starts[i] = NULL;
				}
			}
			cycle_size = cycle_starts.size();
			delete it->node->get_state();
			delete it->node;
		}
	}

	realNodes = 0;
	for (int r = 0; r < size; r++) {
		realNodes += pvertices[r].realNodes;
	}

	hvertices.clear();
	for (int i = 1; i < size; i++) {
		displacement[i] = displacement[i - 1] + receive_count[i - 1];
	}
	if (hashes.capacity() < (size_t)receive_count[rank]) {
		hashes.resize(receive_count[rank]);
	}
}

