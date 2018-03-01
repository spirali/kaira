
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include "cailie.h"
#include "basictypes.h"
#include "state.h"
#include "verifconfiguration.h"
#include "hasharray.h"
#include "vertices.h"

#include <set>
#include <list>
#include <stack>
#include <vector>
#include <iostream>
#include <fstream>

namespace cass {

	namespace cfg {
		extern bool write_dot;
		extern bool write_statespace;
		extern bool analyse_deadlock;
		extern bool analyse_transition_occurence;
		extern bool analyse_final_marking;
		extern bool analyse_cycle;
		extern bool partial_order_reduction;
		extern bool silent;
		extern bool debug;
		extern bool balance;
		extern bool only_singletons;
		extern size_t all_subset_max_size;
		extern std::string project_name;
	};

	class Core;
	class Node;
	class VerifConfiguration;
	struct Arc;
	struct ArcCompare;
	typedef std::map<Arc, int, ArcCompare> ParikhVector;

	class State : public ca::StateBase<Net, Activation, ca::Packet>
	{
		public:
			State(ca::NetDef *net_def) { spawn(net_def); }
			State(State &state) : StateBase(state) {}
			void pack_state(ca::Packer &packer);
			HashDigest compute_hash(hashid hash_id);
			void hash_activations(MHASH hash_thread);
			void hash_packets(MHASH hash_thread);
			void pack_activations(ca::Packer &packer);
			void pack_packets(ca::Packer &packer);

			void serialize(std::vector<char> &data);
			void deserialize(char* p);
	};

	struct NextNodeInfo {
		Node *node;
		HashDigest hash;
		ActionType action;
		int rank;
		union {
				struct {
					int process_id;
					int transition_id;
					HashDigest binding;
				} fire; // ActionFire
				struct {
					int process_id;
				} finish; // ActionFinish
				struct {
					int process_id;
					int source_id;
				} receive; // ActionReceive
		} data;
	};

	struct Arc
	{
		Arc(const NextNodeInfo *nni): nni(nni) {};

		NextNodeInfo const *nni;
	};

	struct ArcCompare
	{
		ArcCompare(VerifConfiguration &verif_configuration):
			verif_configuration(verif_configuration) {};

		bool operator() (const Arc &arc1, const Arc &arc2) const {
			return verif_configuration.compare(arc1, arc2);
		}

		VerifConfiguration &verif_configuration;
	};

	inline bool operator==(const ParikhVector &p1, const ParikhVector &p2) {
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

	inline bool operator!=(const ParikhVector &p1, const ParikhVector &p2) {
		return !(p1 == p2);
	}

	class Node
	{
		public:
			Node(HashDigest hash, State *state, Node *prev);
			Node(HashDigest hash, State *state, Node *prev, int distance, int nni);
			~Node();
			const std::vector<NextNodeInfo> & get_nexts() const { return nexts; }
			std::vector<NextNodeInfo> & get_nexts() { return nexts; }
			//void generate(Core *statespace);

			void compute_ample(Core *core);
			void fire_ample(Core *core);
			const ActionSet& get_ample() const { return ample; }
			HashDigest get_hash() const { return hash; }

			State* get_state() const { return state; }
			Node* get_prev() const { return prev; }
			int get_prev_rank() const { return prev_rank; }
			int get_prev_nni() const { return prev_nni; }
			int get_distance() const { return distance; }
			void* get_data() const { return data; };
			void set_data(void* data) { this->data = data; };
			void set_prev(Node *prev);
			void set_prev_rank(int rank) { prev_rank = rank; }
			const NextNodeInfo& get_next_node_info(Node *node) const;
			size_t get_ample_size() const { return ample.size(); }

			bool is_quitted() { return quit; }
			bool get_quit_flag() { return quit_flag; }
			void set_quit(bool quit_flag);
			HashDigest get_final_marking() { return final_marking; }
			void set_final_marking(HashDigest hash) { final_marking = hash; }
			int get_tag() const { return tag; }
			void set_tag(int tag) { this->tag = tag; }
		protected:
			ActionSet compute_enable_set(Core *core);
			HashDigest hash;
			State* state;
			std::vector<NextNodeInfo> nexts;
			Node* prev;
			int prev_rank, prev_nni;
			int distance;
			bool quit;
			bool quit_flag;
			HashDigest final_marking;
			ActionSet ample;

			// Generic data used during analysis
			int tag;
			void* data;
	};

	typedef google::sparse_hash_map<HashDigest, Node*, HashDigestHash, HashDigestEq> NodeMap;

	struct DFSNode {
		Node* node;
		int nni, origin;
	};

	class Core
	{
		public:
			Core(int argc, char **argv, VerifConfiguration &verif_configuration, std::vector<ca::Parameter*> &parameters);
			~Core();
			void generate();
			void postprocess();
			void write_dot_file(const std::string &filename);
			Node * add_state(State *state, Node *prev);
			HashDigest hash_packer(ca::Packer &packer);
			HashDigest pack_marking(Node *node);
			ca::NetDef * get_net_def() { return net_def; }
			bool generate_binding_in_nni(int transition_id);

			ActionSet compute_ample_set(State *s, const ActionSet &enable);

			static void hashdigest_to_string(HashDigest hash, char *out);
			static std::string hashdigest_to_string(HashDigest hash);
		protected:
			void write_report();
			void add_control_line(std::stringstream &s, const NextNodeInfo &nninfo);
			void write_control_sequence(const std::vector<Node*> &nodes, ca::Output &report);
			void write_suffix(const std::string &name, const std::vector<Node*> &nodes, ca::Output &report);
			void write_suffix(const std::string &name, const Node* start, std::stack<DFSNode> &nodes, ca::Output &report);
			void write_xml_statespace(ca::Output &report);

			void run_analysis_final_nodes(ca::Output &report);
			void run_analysis_transition_occurrence(ca::Output &report);
			void run_analysis_cycle(ca::Output &report);
			void set_tags(int tag);

			bool is_known_node(Node *node) const;
			Node *get_node(HashDigest digest) const;

			void check_C1(const ActionSet &enabled, ActionSet &ample, State *s, std::vector<int> &marking);
			bool check_C2(const ActionSet &ws);
			bool check_C3(State *s);

			hashid hash_id;

			std::list<Node*> not_processed;
			NodeMap nodes;
			Node *initial_node;
			ca::NetDef *net_def;
			VerifConfiguration &verif_configuration;
			bool generate_binging_in_nni;
			std::ofstream debug_output;
			std::vector<Node*> cycle_starts;
			size_t cycle_size;

			size_t fullyEplored;
			size_t partlyExplored;
			size_t singleExplored;
#ifdef CA_MPI
			void init_partition();
			void send_hashes();
			void send_result();
			void partitiate();

			HashArray hashes;
			HashArray receives;

			std::vector<int> receive_count;
			std::vector<int> displacement;

			std::map<HashDigest, Node*, HashDigestOrder> currentNodes;
			std::vector<HashVertex> hvertices;
			std::vector<ProcessVertex> pvertices;

			HashDigestOrder hashOrder;
			HashDigestEq hashEq;

			long realNodes;
			long exchanged;

			int rank;
			int size;

			int output;
#endif
	};

	struct CmpByDistance
	{
		bool operator()(Node *a, Node *b) const
		{
			return a->get_distance() < b->get_distance();
		}
	};

	void init(int argc, char **argv, std::vector<ca::Parameter*> &parameters, bool tracing);
}
#endif // CAVERIF_STATESPACE_H
