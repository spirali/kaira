
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include <stack>
#include <google/sparse_hash_set>
#include "basictypes.h"

namespace cass {

	class Core;

	struct TransitionActivation {
			CaTransitionDef *transition_def;
			int thread_id;
			int process_id;
			void *data;
	};

	class Node {
		public:
			Node();
			Node(CaNetDef *net_def);
			Node(const std::vector<TransitionActivation> &activations);
			~Node();

			size_t state_hash() const;

			bool state_equals(const Node &rhs) const;

			void generate(Core *statespace);
			Node * copy();

			const std::vector<Node*> & get_nexts() const { return nexts; }
			const std::vector<TransitionActivation> & get_activations() {
				return activations;
			};
		protected:
			Net **nets;
			std::vector<Node*> nexts;
			std::vector<TransitionActivation> activations;
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
			void verify();
			Node * add_node(Node *node);
			CaNetDef * get_net_def() { return net_def; }
		protected:
			bool is_known_node(Node *node) const;
			Node * get_node(Node *node) const;
			std::stack<Node*> not_processed;
			google::sparse_hash_set<Node*,
									NodeStateHash,
									NodeStateEq> nodes;
			Node *initial_node;
			CaNetDef * net_def;
	};

}
#endif // CAVERIF_STATESPACE_H
