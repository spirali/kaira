
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include <stack>
#include <google/sparse_hash_map>
#include <mhash.h>
#include "cailie.h"
#include "basictypes.h"
#include "state.h"
#include "verifconfiguration.h"

#include <set>
#include <list>
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
		extern std::string project_name;
	};

	class Core;
	class Node;
	class VerifConfiguration;
	typedef void* HashDigest;
	struct Arc;
	struct ArcCompare;
	typedef std::map<Arc, int, ArcCompare> ParikhVector;

	class State  : public ca::StateBase<Net, Activation, ca::Packet>
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
	};

	struct NextNodeInfo {
		Node *node;
		ActionType action;
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
		Arc(Node *node, const NextNodeInfo *nni): node(node), nni(nni) {};

		Node *node;
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

	class Node
	{
		public:
			Node(HashDigest hash, State *state, Node *prev);
			~Node();
			const std::vector<NextNodeInfo> & get_nexts() const { return nexts; }
			void generate(Core *statespace);
			HashDigest get_hash() const { return hash; }

			State* get_state() const { return state; }
			Node* get_prev() const { return prev; }
			int get_distance() const { return distance; }
			void* get_data() const { return data; };
			void set_data(void* data) { this->data = data; };
			void set_prev(Node *prev);
			const NextNodeInfo& get_next_node_info(Node *node) const;

			bool get_quit_flag() { return quit; }
			void set_quit_flag(bool quit) { this->quit = quit; }
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
			int distance;
			bool quit;
			HashDigest final_marking;

			// Generic data used during analysis
			int tag;
			void* data;
	};

	struct HashDigestHash
	{
		HashDigestHash(hashid hash_id) : size(mhash_get_block_size(hash_id)) {}

		size_t operator()(HashDigest hash) const {
			size_t *h = (size_t*) hash;
			return *h;
		}
		size_t size;
	};

	struct HashDigestEq
	{
		HashDigestEq(hashid hash_id) : size(mhash_get_block_size(hash_id)) {}

		bool operator()(HashDigest hash1, HashDigest hash2) const {
			return !memcmp(hash1, hash2, size);
		}
		size_t size;
	};

	typedef google::sparse_hash_map<HashDigest, Node*, HashDigestHash, HashDigestEq>
		NodeMap;

	class Core
	{
		public:
			Core(VerifConfiguration &verif_configuration, std::vector<ca::Parameter*> &parameters);
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

			static void hashdigest_to_string(hashid hash_id, HashDigest hash, char *out);
			static std::string hashdigest_to_string(hashid hash_id, HashDigest hash);
		protected:
			void write_report();
			void write_control_sequence(std::vector<Node*> &nodes, ca::Output &report);
			void write_state(const std::string &name, Node *node, ca::Output &report);
			void write_suffix(const std::string &name, std::vector<Node*> &nodes, ca::Output &report);
			void write_xml_statespace(ca::Output &report);

			void run_analysis_final_nodes(ca::Output &report);
			void run_analysis_transition_occurrence(ca::Output &report);
			void run_analysis_cycle(ca::Output &report);
			void set_tags(int tag);

			bool is_known_node(Node *node) const;
			Node *get_node(HashDigest digest) const;

			bool check_C1(const ActionSet &enabled, const ActionSet &ample, State *s);
			bool check_C2(const ActionSet &ws);
			bool check_C3(State *s);
			std::stack<Node*> not_processed;
			NodeMap nodes;
			Node *initial_node;
			ca::NetDef *net_def;
			VerifConfiguration &verif_configuration;
			bool generate_binging_in_nni;
			std::ofstream debug_output;

	};

	void init(int argc, char **argv, std::vector<ca::Parameter*> &parameters, bool tracing);
}
#endif // CAVERIF_STATESPACE_H
