
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include <stack>
#include <google/sparse_hash_set>
#include "cailie.h"
#include "basictypes.h"

namespace cass {

	class Core;

	struct TransitionActivation {
			CaTransitionDef *transition_def;
			int thread_id;
			int process_id;
			void *data;
	};

	struct NodeFlag {
		std::string name;
		std::string value;
		NodeFlag *next;
	};

	class Node {
		public:
			Node();
			Node(CaNetDef *net_def);
			Node(const std::vector<TransitionActivation> &activations,
				 std::deque<Packet> *packets);
			~Node();

			size_t state_hash() const;

			bool state_equals(const Node &rhs) const;

			void generate(Core *statespace);
			Node * copy_state();

			const std::vector<Node*> & get_nexts() const { return nexts; }
			const std::vector<TransitionActivation> & get_activations() {
				return activations;
			};

			void add_flag(const std::string &name, const std::string &value);
			NodeFlag *get_flag(const std::string &name);
		protected:
			/** State of computation */
			Net **nets;
			std::deque<Packet> *packets;
			std::vector<TransitionActivation> activations;

			/** Node state */
			std::vector<Node*> nexts;
			NodeFlag *flag;
	};

	struct NodeStateHash {
		size_t operator()(Node *node) const {
			return node->state_hash();
		}
	};

	struct NodeStateEq {
		bool operator()(Node *node1, Node *node2) const {
			return node1->state_equals(*node2);
		}
	};

	class Core
	{
		public:
			Core();
			~Core();
			void generate();
			void postprocess();
			void write_dot_file(const std::string &filename);
			Node * add_node(Node *node);
			CaNetDef * get_net_def() { return net_def; }
		protected:
			void run_analysis_quit(CaOutput &report);

			bool is_known_node(Node *node) const;
			Node *get_node(Node *node) const;
			std::stack<Node*> not_processed;
			google::sparse_hash_set<Node*,
									NodeStateHash,
									NodeStateEq> nodes;
			Node *initial_node;
			CaNetDef * net_def;
	};

	void init(int argc, char **argv, std::vector<CaParameter*> &parameters);
}
#endif // CAVERIF_STATESPACE_H
