
#include "statespace.h"
#include <string>
#include <stack>
#include "mpi.h"

namespace ca {
	extern char *project_description_string;
}

using namespace cass;


void Core::run_analysis_final_nodes(ca::Output &report)
{
	int deadlocks = 0;
	Node* deadlock_node = NULL;
	NodeMap final_markings(100, HashDigestHash(), HashDigestEq());

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
				if (!node->is_quitted()) {
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
		int tdeadlocks, ldistance = 0, distance, mrank;
		if (deadlocks) {
			ldistance = deadlock_node->get_distance();
		}

		MPI_Allreduce(&deadlocks, &tdeadlocks, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
		MPI_Allreduce(&ldistance, &distance, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD);
		if (deadlocks) {
			ldistance = deadlock_node->get_distance();
		} else {
			ldistance = distance;
		}
		MPI_Allreduce(&ldistance, &distance, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
		if (rank && ldistance == distance) {
			MPI_Allreduce(&rank, &mrank, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
		} else {
			MPI_Allreduce(&size, &mrank, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);
		}

		if (!rank) {
			report.child("analysis");
			report.set("name", "Quit analysis");

			report.child("result");
			report.set("name", "Number of deadlock states");
			report.set("value", tdeadlocks);
			if (tdeadlocks != 0) {
				report.set("status", "fail");
				report.set("text", "Deadlocks found");
				report.child("states");
			}
		}
		if (tdeadlocks) {
			if (mrank == rank) {
				write_suffix("Deadlock with minimal distance", { deadlock_node }, report);
			} else {
				write_suffix("Deadlock with minimal distance", { }, report);
			}
		}
		if (!rank) {
			if (tdeadlocks != 0) {
				report.back();
			} else {
				report.set("status", "ok");
			}
			report.back();
			report.back();
		}
	}

	if (cfg::analyse_final_marking) {
		int fstates, states = final_markings.size(), offset = 0;

		MPI_Allreduce(&states, &fstates, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
		MPI_Exscan(&states, &offset, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

		auto print_states = [&] () {
			auto it = final_markings.begin();
			for (int i = 0; i < fstates; i++) {
				if (offset <= i && i < offset + states) {
					write_suffix(std::to_string(i + 1) + ". final state", { it->second }, report);
					++it;
				} else {
					write_suffix(std::to_string(i + 1) + ". final state", { }, report);
				}
			}
		};

		if (!rank) {
			report.child("analysis");
			report.set("name", "Final marking");
			report.child("result");
			report.set("name", "Number of final markings");
			report.set("value", fstates);
			if (fstates == 1) {
				report.set("status", "ok");
			} else {
				report.set("status", "fail");
				report.set("text", "There are more final markings.");

				report.child("states");
				print_states();
				report.back();
			}

			report.back();
			report.back();
		} else {
			if (fstates != 1) {
				print_states();
			}
		}
	}
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

	size_t processes = 0, level = 1;
	std::vector<std::vector<char> > sdata(size), rdata(size);
	std::vector<MPI_Request> req(size);
	std::vector<int> smsgcounter(size);
	std::vector<int> rmsgcounter(size);
	int run = 1;
	while (run) {
		if (node_queue.size()) {
			++processes;
			node = node_queue.front();
			node_queue.pop();
			current = (ParikhVector*)(node->get_data());
			const std::vector<NextNodeInfo> &nexts_nodes = node->get_nexts();
			for (size_t i = 0; i < nexts_nodes.size(); i++) {
				next = new ParikhVector(*current);
				if (nexts_nodes[i].action == ActionFire) {
					Arc arc(&nexts_nodes[i]);
					if (verif_configuration.is_transition_analyzed(nexts_nodes[i].data.fire.transition_id)) {
						ParikhVector::iterator i = next->find(arc);
						if (i == next->end()) {
							next->insert(std::pair<Arc, int>(arc, 1));
						} else {
							i->second += 1;
						}
					}
				}

				if (nexts_nodes[i].rank == -1 || nexts_nodes[i].rank == rank) {
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
				} else {
					int size = next->size();
					sdata[nexts_nodes[i].rank].insert(sdata[nexts_nodes[i].rank].end(), reinterpret_cast<const char*>(&size), reinterpret_cast<const char*>(&size) + sizeof(size));
					sdata[nexts_nodes[i].rank].insert(sdata[nexts_nodes[i].rank].end(), reinterpret_cast<const char*>(nexts_nodes[i].hash), reinterpret_cast<const char*>(nexts_nodes[i].hash) + mhash_get_block_size(CASS_HASH_ID));
					for (auto it = next->begin(); it != next->end(); ++it) {
						const NextNodeInfo &nni = *(it->first.nni);
						int counter = it->second;
						sdata[nexts_nodes[i].rank].insert(sdata[nexts_nodes[i].rank].end(), reinterpret_cast<const char*>(&nni), reinterpret_cast<const char*>(&nni) + sizeof(nni));
						sdata[nexts_nodes[i].rank].insert(sdata[nexts_nodes[i].rank].end(), reinterpret_cast<const char*>(&counter), reinterpret_cast<const char*>(&counter) + sizeof(counter));
					}
				}
			}
		}

		if (processes == level) {
			int scouter = 0;
			for (int r = 0; r < size; r++) {
				smsgcounter[r] = sdata[r].size() ? 1 : 0;
			}
			req.clear();

			MPI_Allreduce(smsgcounter.data(), rmsgcounter.data(), size, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

			for (int r = 0; r < size; r++) {
				if (sdata[r].size()) {
					MPI_Isend(sdata[r].data(), sdata[r].size(), MPI_BYTE, r, 0, MPI_COMM_WORLD, req.data() + scouter++);
				}
			}

			int flag;
			int counter = 0;
			MPI_Status status;
			while (counter < rmsgcounter[rank]) {
				MPI_Iprobe(MPI_ANY_SOURCE, 0, MPI_COMM_WORLD, &flag, &status);
				if (flag) {
					int count;
					MPI_Get_count(&status, MPI_BYTE, &count);
					rdata[status.MPI_SOURCE].resize(count);
					MPI_Recv(rdata[status.MPI_SOURCE].data(), count, MPI_BYTE, status.MPI_SOURCE, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					counter++;
				}
			}

			MPI_Waitall(scouter, req.data(), MPI_STATUSES_IGNORE);

			for (int r = 0; r < size; r++) {
				if (rdata[r].size()) {
					char *p = rdata[r].data();
					while (p < rdata[r].data() + rdata[r].size()) {
						HashDigest hash;
						int size;
						memcpy(&size, p, sizeof(size));
						p += sizeof(size);

						hash = p;
						p += mhash_get_block_size(CASS_HASH_ID);
						auto nn = nodes.find(hash);
						ParikhVector *next = new ParikhVector(arcCmp);
						for (int i = 0; i < size; i++) {
							NextNodeInfo nni;
							int counter;
							memcpy(&nni, p, sizeof(nni));
							p += sizeof(nni);
							memcpy(&counter, p, sizeof(counter));
							p += sizeof(counter);
							next->insert(std::pair<Arc, int>(Arc(&nni), counter));
						}
						ParikhVector *v = (ParikhVector*) nn->second->get_data();
						if (v != NULL) {
							ParikhVector::const_iterator it;
							if (*v != *next) {
								error_node = nn->second;
								error_node2.push_back(node);
								delete next;
								break;
							}
							delete next;
						} else {
							nn->second->set_data(next);
							node_queue.push(nn->second);
						}
					}
				}
			}

			level = processes + node_queue.size();
			for (int r = 0; r < size; r++) {
				sdata[r].clear();
				rdata[r].clear();
			}
			int mrun = node_queue.size();
			MPI_Allreduce(&mrun, &run, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
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
			if (node->is_quitted() && error_node == NULL) {
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

	if (!rank) {
		report.child("analysis");
		report.set("name", "Analysis of characteristic vectors");

		report.child("result");
		report.set("name", "Uniqueness of characteristic vector");
		if (error_node) {
			report.set("status", "fail");
			report.set("text", "Different characteristic vectors found");
			report.child("states");
			write_suffix("First witness path", { error_node }, report);
			write_suffix("Second witness path", error_node2, report);
			report.back();
		} else {
			report.set("status", "ok");
		}
		report.back();
		report.back();
	}
}

#define NODE 0
#define RETURN_NODE 1
#define FINISH 2
#define CYCLE 3

void Core::run_analysis_cycle(ca::Output &report)
{
	std::vector<Node*> starts;
	for (size_t n = 0; n < cycle_starts.size(); n++) {
		if (cycle_starts[n] != NULL) {
			starts.push_back(cycle_starts[n]);
		}
	}
	int csize = starts.size(), size, offset = 0;
	MPI_Allreduce(&csize, &size, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
	MPI_Exscan(&csize, &offset, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

	set_tags(size);

	int run = 0;
	int index;
	int current = -1, inited = 0;
	std::vector<std::stack<DFSNode> > stacks(size);
	for (size_t i = 0; i < starts.size(); i++) {
		stacks[offset + i].push({starts[i], 0, -1});
	}
	if (starts.size()) {
		current = offset;
	}
	MPI_Status status;
	std::vector<char> hhash(mhash_get_block_size(CASS_HASH_ID));
	HashDigest hash = hhash.data();

	int cycle_index = -1;
	while (run < size) {
		if (current >= 0) {
			DFSNode n = stacks[current].top();
			if (current < n.node->get_tag()) {
				n.node->set_tag(current);
				if (n.nni < (int)n.node->get_nexts().size()) {
					if (n.node->get_nexts()[n.nni].rank == -1 || n.node->get_nexts()[n.nni].rank == rank) {
						stacks[current].push({ n.node->get_nexts()[n.nni].node, 0, rank });
					} else {
						MPI_Bsend(&current, 1, MPI_INT, n.node->get_nexts()[n.nni].rank, NODE, MPI_COMM_WORLD);
						MPI_Bsend(n.node->get_nexts()[n.nni].hash, mhash_get_block_size(CASS_HASH_ID), MPI_BYTE, n.node->get_nexts()[n.nni].rank, NODE, MPI_COMM_WORLD);
						if (inited + 1 < (int)starts.size()) {
							++inited;
							current = offset + inited;
						} else {
							current = -1;
						}
					}
					++n.nni;
				} else {
					stacks[current].pop();
					if (stacks[current].size() == 0) {
						if (n.origin == -1) {
							for (int r = 0; r < this->size; r++) {
								MPI_Bsend(&current, 1, MPI_INT, r, FINISH, MPI_COMM_WORLD);
							}
						} else {
							MPI_Bsend(&current, 1, MPI_INT, n.origin, RETURN_NODE, MPI_COMM_WORLD);
						}
						if (inited + 1 < (int)starts.size()) {
							++inited;
							current = offset + inited;
						} else {
							current = -1;
						}
					}
				}
			}
			if (current == n.node->get_tag()) {
				for (int r = 0; r < this->size; r++) {
					MPI_Bsend(&current, 1, MPI_INT, r, CYCLE, MPI_COMM_WORLD);
				}
				if (inited + 1 < (int)starts.size()) {
					++inited;
					current = offset + inited;
				} else {
					current = -1;
				}
			}
		} else {
			MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
			if (status.MPI_TAG == FINISH) {
				MPI_Recv(&index, 1, MPI_INT, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				run++;
			}
			if (status.MPI_TAG == RETURN_NODE) {
				MPI_Recv(&current, 1, MPI_INT, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			}
			if (status.MPI_TAG == NODE) {
				MPI_Recv(&current, 1, MPI_INT, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				MPI_Recv(hhash.data(), mhash_get_block_size(CASS_HASH_ID), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				stacks[current].push({ nodes.find(hash)->second, 0, status.MPI_SOURCE });
			}
			if (status.MPI_TAG == CYCLE) {
				run++;
				MPI_Recv(&cycle_index, 1, MPI_INT, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			}
		}
	}

	int mincycle;
	MPI_Allreduce(&cycle_index, &mincycle, 1, MPI_INT, MPI_MIN, MPI_COMM_WORLD);

	if (!rank) {
		report.child("analysis");
		report.set("name", "Cycle detection");

		report.child("result");
		report.set("name", "Cycle detection");
		if (mincycle != -1) {
			report.set("status", "fail");
			report.set("text", "A cyclic computation detected");
			report.child("states");
		}
	}

	if (mincycle != -1) {
		if (offset <= mincycle && mincycle < offset + (int)starts.size()) {
			write_suffix("Path to a cycle.", { starts[mincycle - offset] }, report);

		} else {
			write_suffix("Path to a cycle.", { }, report);
		}
		write_suffix("The full cycle", starts[mincycle - offset], stacks[mincycle], report);
	}

	if (!rank) {
		if (mincycle != -1) {
			report.back();
		} else {
			report.set("status", "ok");
		}
		report.back();
		report.back();
	}
}


void Core::postprocess()
{
	write_report();
}

void Core::write_report()
{
	auto analyses = [&] (ca::Output &report) {
		if (cfg::analyse_deadlock || cfg::analyse_final_marking) {
			run_analysis_final_nodes(report);
		}
		if (cfg::analyse_transition_occurence) {
			run_analysis_transition_occurrence(report);
		}
		if (cfg::analyse_cycle) {
			run_analysis_cycle(report);
		}
	};


	if (rank) {
		ca::Output report(NULL);
		analyses(report);
	} else {
		FILE *f = fopen((cfg::project_name + ".xml").c_str(), "w");
		ca::Output report(f);
		report.child("report");
		report.set("version", 1);

		report.child("analysis");
		report.set("name", "Overall statistics");
		report.child("result");
		report.set("name", "Number of states");
		size_t states = 1;
		for (int i = 0; i < size; i++) {
			states += pvertices[i].realNodes;
		}
		report.set("value", states);
		report.back();
		report.back();

		analyses(report);

		report.child("description");
		report.text(ca::project_description_string);
		report.back();

		report.back();
		fclose(f);
	}
}

#define SNODE 10
#define SFINISH 11
#define SDISTANCE 12

void Core::write_control_sequence(const std::vector<Node*> &nodes, ca::Output &report)
{
	int distance = 0, nsize = nodes.size(), tsize = 0;
	MPI_Allreduce(&nsize, &tsize, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

	if (tsize == 1) {
		if (nodes.size()) {
			distance = nodes.back()->get_distance();
			MPI_Bsend(&distance, 1, MPI_INT, 0, SDISTANCE, MPI_COMM_WORLD);
		}
		if (!rank) {
			MPI_Recv(&distance, 1, MPI_INT, MPI_ANY_SOURCE, SDISTANCE, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}

		std::vector<std::pair<Node*, int> > path;
		Node* current = NULL;
		MPI_Status status;
		if (nodes.size()) {
			current = nodes.back();
		}

		int run = 1, target, nni = -1;
		while (run) {
			if (current != NULL) {
				path.push_back(std::make_pair(current, nni));
				if (current->get_prev_rank() != -1 && current->get_prev_rank() != rank) {
					target = current->get_prev_rank();
					nni = current->get_prev_nni();
					current = current->get_prev();
					MPI_Send(&current, sizeof(current), MPI_BYTE, target, SNODE, MPI_COMM_WORLD);
					MPI_Send(&nni, sizeof(nni), MPI_BYTE, target, SNODE, MPI_COMM_WORLD);
					current = NULL;
				} else {
					if (current->get_prev() != NULL) {
						for (size_t i = 0; i < current->get_prev()->get_nexts().size(); i++) {
							if (current->get_prev()->get_nexts()[i].node == current) {
								nni = i;
								break;
							}
						}
						current = current->get_prev();
					} else {
						for (int r = 0; r < size; r++) {
							if (r != rank) {
								MPI_Send(NULL, 0, MPI_BYTE, r, SFINISH, MPI_COMM_WORLD);
							}
						}
						run = 0;
					}
				}
			} else {
				MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
				if (status.MPI_TAG == SNODE) {
					MPI_Recv(&current, sizeof(current), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					MPI_Recv(&nni, sizeof(nni), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				}
				if (status.MPI_TAG == SFINISH) {
					MPI_Recv(NULL, 0, MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					run = 0;
				}
			}
		}

		MPI_Barrier(MPI_COMM_WORLD);
		std::vector<NextNodeInfo> infos(distance);

		if (rank) {
			for (size_t i = 0; i < path.size(); i++) {
				if (path[i].second != -1) {
					int distance = path[i].first->get_distance();
					MPI_Send(&distance, sizeof(distance), MPI_BYTE, 0, SNODE, MPI_COMM_WORLD);
					MPI_Send(&path[i].first->get_nexts()[path[i].second], sizeof(NextNodeInfo), MPI_BYTE, 0, SNODE, MPI_COMM_WORLD);
				}
			}
		} else {
			for (size_t i = 0; i < (size_t)distance - (path.size() - nodes.size()); i++) {
				MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
				if (status.MPI_TAG == SNODE) {
					int distance;
					MPI_Recv(&distance, sizeof(distance), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
					MPI_Recv(infos.data() + distance, sizeof(NextNodeInfo), MPI_BYTE, status.MPI_SOURCE, status.MPI_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
				}
			}
			for (size_t i = 0; i < path.size(); i++) {
				if (path[i].second != -1) {
					infos[path[i].first->get_distance()] = path[i].first->get_nexts()[path[i].second];
				}
			}

			std::stringstream s;
			for (size_t i = 0; i < infos.size(); i++) {
				add_control_line(s, infos[i]);
			}

			report.child("control-sequence");
			report.text(s.str());
			report.back();
		}
	}
}

#define SUFTAG 20

void Core::write_suffix(const std::string &name, const std::vector<Node*> &nodes, ca::Output &report)
{
	int size = nodes.size(), tsize = 0;
	MPI_Allreduce(&size, &tsize, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);

	int distance = 0;
	std::vector<char> hash;

	if (nodes.size()) {
		std::string ss = hashdigest_to_string(nodes.back()->get_hash());
		hash.insert(hash.end(), ss.begin(), ss.end());
		hash.push_back(0);
		distance = nodes.back()->get_distance();
	}

	if (tsize == 1) {
		if (rank && nodes.size()) {
			MPI_Send(hashdigest_to_string(nodes.back()->get_hash()).c_str(), hash.size(), MPI_BYTE, 0, SUFTAG, MPI_COMM_WORLD);
			MPI_Send(&distance, 1, MPI_INT, 0, 0, MPI_COMM_WORLD);
		}
		if (!rank && !nodes.size()) {
			hash.resize(mhash_get_block_size(CASS_HASH_ID) * 2 + 1);
			MPI_Recv(hash.data(), hash.size(), MPI_BYTE, MPI_ANY_SOURCE, SUFTAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			MPI_Recv(&distance, 1, MPI_INT, MPI_ANY_SOURCE, SUFTAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}

		if (!rank) {
			report.child("state");
			report.set("name", name);
			report.set("hash", hash.data());
			report.set("distance", distance);
			write_control_sequence(nodes, report);
			report.back();
		} else {
			write_control_sequence(nodes, report);
		}
	}
}

void Core::write_suffix(const std::string &name, const Node* start, std::stack<DFSNode> &nodes, ca::Output &report)
{
	std::vector<std::vector<std::pair<DFSNode, NextNodeInfo> > > data(size);
	std::vector<std::pair<DFSNode, NextNodeInfo> > sdata(nodes.size());
	for (size_t i = 0; i < sdata.size(); i++) {
		sdata[i] = std::make_pair(nodes.top(), nodes.top().node->get_nexts()[nodes.top().nni]);
		sdata[i].first.nni = sdata[i].first.node->get_nexts()[sdata[i].first.nni].rank;
		nodes.pop();
	}

	int source = start != NULL ? rank : 0;
	int init = 0;
	MPI_Reduce(&source, &init, 1, MPI_INT, MPI_SUM, 0, MPI_COMM_WORLD);

	MPI_Status status;
	if (rank) {
		MPI_Send(sdata.data(), sdata.size() * sizeof(std::pair<DFSNode, NextNodeInfo>), MPI_BYTE, 0, 111, MPI_COMM_WORLD);
	} else {
		for (int r = 0; r < size - 1; r++) {
			MPI_Probe(MPI_ANY_SOURCE, 111, MPI_COMM_WORLD, &status);
			int count;
			MPI_Get_count(&status, MPI_BYTE, &count);
			data[status.MPI_SOURCE].resize(count / sizeof(std::pair<DFSNode, NextNodeInfo>));
			MPI_Recv(data[status.MPI_SOURCE].data(), count, MPI_BYTE, status.MPI_SOURCE, 111, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		}
		data[rank] = sdata;
	}

	if (!rank) {
		size_t couter = 0;
		for (size_t i = 0; i < data.size(); i++) {
			couter += data[i].size();
		}
		size_t c = 0;
		int process = init;
		std::stringstream s;
		while (c++ < couter) {
			add_control_line(s, data[process].back().second);
			init = process;
			process = data[process].back().first.nni;
			data[init].pop_back();
		}

		report.child("state");
		report.set("name", name);
		report.set("distance", couter);

		report.child("control-sequence");
		report.text(s.str());
		report.back();
		report.back();
	}

}
