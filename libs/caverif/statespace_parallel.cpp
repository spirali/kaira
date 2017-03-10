
#include "mpi.h"

#include "statespace.h"
#include <string.h>
#include <vector>
#include <map>
#include <queue>
#include <algorithm>
#include <alloca.h>
#include <ctime>
#include <omp.h>

namespace ca {
	extern ca::NetDef **defs;
}

#define WAITING_TIME 0.1
#define TAG_STATE 1
#define TAG_STATUS 2
#define TAG_FINISH 3

struct DataBuffer {
	void clear_sent()
	{
		int finished = 1;
		while (buffer.size() && finished) {
			MPI_Test(&buffer.front().first, &finished, MPI_STATUS_IGNORE);
			if (finished) {
				buffer.pop_front();
			}
		}
	}

	std::pair<MPI_Request, std::vector<char> >& create()
	{
		buffer.push_back(std::pair<MPI_Request, std::vector<char> >());
		return buffer.back();
	}

	std::list<std::pair<MPI_Request, std::vector<char> > > buffer;
};

using namespace cass;

static int modulo(HashDigest hash, int n)
{
	int size = mhash_get_block_size(MHASH_MD5);
	uint reminder = 0;
	for (int i = 0; i < size; i++) {
		reminder = ((reminder << 8) | (((char*)hash)[i] & (0xFF))) % n;
	}
	return reminder;
}

Node * Core::add_state(State *state, Node *prev)
{
	HashDigest hash = state->compute_hash(MHASH_MD5);

	int destination_rank = modulo(hash, size);
	if (destination_rank == rank) {
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
	} else {
		free(hash);
		((DataBuffer*)data_buffer)->clear_sent();
		auto &data = ((DataBuffer*)data_buffer)->create();
		state->serialize(data.second);
		delete state;
		MPI_Isend(data.second.data(), data.second.size(), MPI_BYTE, destination_rank, TAG_STATE, MPI_COMM_WORLD, &data.first);
		communicated[destination_rank]++;
		return NULL;
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

	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	data_buffer = new DataBuffer();
	if (rank) {
		communicated.resize(2 * size);
	} else {
		communicated.resize(2 * size * size);
	}

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

	MPI_Barrier(MPI_COMM_WORLD);
	double start = omp_get_wtime();

	net_def = ca::defs[0]; // Take first definition
	State *initial_state = new State(net_def);
	for (int i = 0; i < ca::process_count; i++) {
		nets.push_back(initial_state->get_net(i)->copy_without_tokens());
	}

	HashDigest hash = initial_state->compute_hash(MHASH_MD5);
	if (modulo(hash, size) == rank) {
		initial_node = add_state(initial_state, NULL);
	} else {
		delete initial_state;
	}
	free(hash);

	bool run = true;
	std::vector<char> data;

	MPI_Request *request = NULL;

	while (run) {
		if (not_processed.empty()) {
			std::clock_t start = std::clock();
			// remove finished request
			if (request != NULL) {
				int received;
				MPI_Status status;
				MPI_Test(request, &received, &status);
				if (received) {
					delete request;
					request = NULL;
				}
			}

			while (true) {

				// If wait more than TIME send communication statistic to root
				if (request == NULL && (std::clock() - start) > CLOCKS_PER_SEC * WAITING_TIME) {
					request = new MPI_Request();
					MPI_Isend(communicated.data(), 2 * size * sizeof(size_t), MPI_BYTE, 0, TAG_STATUS, MPI_COMM_WORLD, request);
				}

				// check incomming messages
				int received;
				MPI_Status status;
				MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &received, &status);
				if (received) {
					if (status.MPI_TAG == TAG_STATE) {
						MPI_Get_count(&status, MPI_BYTE, &received);
						data.resize(received);
						MPI_Recv(data.data(), received, MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
						run = true;
						State *s = new State(net_def, nets, data);
						add_state(s, NULL);
						communicated[size + status.MPI_SOURCE]++;
					}
					if (status.MPI_TAG == TAG_FINISH) {
						run = false;
					}
					if (status.MPI_TAG == TAG_STATUS) {
						// reachable only on root
						MPI_Recv(communicated.data() + 2 * size * status.MPI_SOURCE, 2 * size * sizeof(size_t), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

						bool finish = true;
						for (int s = 0; s < size; s++) {
							for (int d = 0; d < size; d++) {
								if (communicated[2 * size * s + d] != communicated[2 * size * d + size + s]) {
									finish = false;
								}
							}
						}
						if (finish) {
							for (int r = 1; r < size; r++) {
								MPI_Send(NULL, 0, MPI_BYTE, r, TAG_FINISH, MPI_COMM_WORLD);
							}
							run = false;
						}
					}
					break;
				}

			}

		} else {
			Node *node = not_processed.top();
			not_processed.pop();

			node->generate(this);
			if (node->get_nexts().size() == 0 && cfg::analyse_final_marking) {
				node->set_final_marking(pack_marking(node));
			}
			if (node->get_quit_flag()) {
				delete node->get_state();
			}
		}
	}

	MPI_Barrier(MPI_COMM_WORLD);
	double end = omp_get_wtime();

	printf("rank %d holds %ld nodes.\n", rank, nodes.size());

	size_t nsize = nodes.size();
	std::vector<size_t> n(size), all(size), part(size), single(size);

	MPI_Gather(&nsize         , sizeof(size_t), MPI_BYTE, n.data()     , sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD);
	MPI_Gather(&fullyEplored  , sizeof(size_t), MPI_BYTE, all.data()   , sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD);
	MPI_Gather(&partlyExplored, sizeof(size_t), MPI_BYTE, part.data()  , sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD);
	MPI_Gather(&singleExplored, sizeof(size_t), MPI_BYTE, single.data(), sizeof(size_t), MPI_BYTE, 0, MPI_COMM_WORLD);

	if (rank == 0) {
		std::for_each(n.begin() + 1     , n.end()      , [&] (size_t size) { n[0]      += size; });
		std::for_each(all.begin() + 1   , all.end()    , [&] (size_t size) { all[0]    += size; });
		std::for_each(part.begin() + 1  , part.end()   , [&] (size_t size) { part[0]   += size; });
		std::for_each(single.begin() + 1, single.end() , [&] (size_t size) { single[0] += size; });

		printf("total number of explored states: %ld\n", n[0]);
		printf("    size(ample) = size(enable) : %ld\n", all[0]);
		printf("1 < size(ample) < size(enable) : %ld\n", part[0]);
		printf("1 = size(ample) < size(enable) : %ld\n", single[0]);
		printf("\n");
		printf("total verification time        : %5.2f\n", end - start);
	}
}

Core::~Core()
{
	MPI_Finalize();
	NodeMap::iterator it;
	for (it = nodes.begin(); it != nodes.end(); it++) {
		delete it->second;
	}
	if (cfg::debug) {
		debug_output.close();
	}

	for (size_t i = 0; i < nets.size(); i++) {
		delete nets[i];
	}
	delete (DataBuffer*)data_buffer;
}



