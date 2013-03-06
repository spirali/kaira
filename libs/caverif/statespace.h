
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include <stack>
#include <google/sparse_hash_map>
#include <mhash.h>
#include "cailie.h"
#include "basictypes.h"

#include <set>

namespace cass {

	class Core;

	typedef void* HashDigest;

	struct TransitionActivation {
			ca::TransitionDef *transition_def;
			int process_id;
			int thread_id;
			void *binding;
			size_t packed_binding_size;
			void *packed_binding;
	};

	struct ActivationCompare {
	  bool operator() (const TransitionActivation &t1, const TransitionActivation &t2) {
			if (t1.transition_def->get_id() == t2.transition_def->get_id()) {
				if (t1.process_id == t2.process_id) {
					if (t1.thread_id == t2.thread_id) {
						if (t1.packed_binding_size == t2.packed_binding_size) {
							return memcmp(t1.packed_binding, t2.packed_binding, t1.packed_binding_size) < 0;
						} else {
							return t1.packed_binding_size < t2.packed_binding_size;
						}
					} else {
						return t1.thread_id < t2.thread_id;
					}
				} else {
					return t1.process_id < t2.process_id;
				}
			} else {
				return t1.transition_def < t2.transition_def;
			}
	  }
	};

	struct NodeFlag {
		std::string name;
		std::string value;
		NodeFlag *next;
	};

	class State {
		public:
			State();
			State(ca::NetDef *net_def);
			State(const std::vector<TransitionActivation> &activations,
				 std::deque<Packet> *packets);
			~State();

			State * copy();

			const std::vector<TransitionActivation> & get_activations() {
				return activations;
			};
			std::deque<Packet> * get_packets() { return packets; }

			void pack_state(ca::Packer &packer);
			HashDigest compute_hash(hashid hash_id);
			void hash_activations(MHASH hash_thread);
			void hash_packets(MHASH hash_thread);
			void pack_activations(ca::Packer &packer);
			void pack_packets(ca::Packer &packer);

			void add_flag(const std::string &name, const std::string &value);
			NodeFlag *get_flag(const std::string &name);

			Net **nets;
			std::deque<Packet> *packets;
			std::vector<TransitionActivation> activations;
			NodeFlag *flag;
	};

	class Node  {
		public:
			Node(HashDigest hash, State *state);
			~Node();
			const std::vector<Node*> & get_nexts() const { return nexts; }
			void generate(Core *statespace);
			HashDigest get_hash() { return hash; }

			State * get_state() { return state; }
		protected:
			HashDigest hash;
			State *state;
			std::vector<Node*> nexts;
	};

	struct HashDigestHash {
		HashDigestHash(hashid hash_id) : size(mhash_get_block_size(hash_id)) {}

		size_t operator()(HashDigest hash) const {
			size_t *h = (size_t*) hash;
			return *h;
		}
		size_t size;
	};

	struct HashDigestEq {
		HashDigestEq(hashid hash_id) : size(mhash_get_block_size(hash_id)) {}

		bool operator()(HashDigest hash1, HashDigest hash2) const {
			return !memcmp(hash1, hash2, size);
		}
		size_t size;
	};
	typedef google::sparse_hash_map<HashDigest, Node*, HashDigestHash, HashDigestEq> NodeMap;

	class Core
	{
		public:
			Core();
			~Core();
			void generate();
			void postprocess();
			void write_dot_file(const std::string &filename);
			Node * add_state(State *state);
			ca::NetDef * get_net_def() { return net_def; }
		protected:
			void run_analysis_quit(ca::Output &report);

			bool is_known_node(Node *node) const;
			Node *get_node(HashDigest digest) const;
			std::stack<Node*> not_processed;
			//std::set<void*,
			/*google::sparse_hash_set<void*,
									HashHash,
									HashEq> hashes;*/
			NodeMap nodes;
			Node *initial_node;
			ca::NetDef *net_def;
	};

	void init(int argc, char **argv, std::vector<ca::Parameter*> &parameters);
}
#endif // CAVERIF_STATESPACE_H
