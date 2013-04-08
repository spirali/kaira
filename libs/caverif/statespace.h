
#ifndef CAVERIF_STATESPACE_H
#define CAVERIF_STATESPACE_H

#include <stack>
#include <google/sparse_hash_map>
#include <mhash.h>
#include "cailie.h"
#include "basictypes.h"
#include "state.h"

#include <set>

namespace cass {

	class Core;
	typedef void* HashDigest;

	struct Activation : public ca::Activation
	{
		Activation(
			ca::TransitionDef *transition_def,
			int process_id,
			int thread_id,
			void *binding)
			: ca::Activation(transition_def, process_id, thread_id, binding) {
				ca::Packer packer;
				transition_def->pack_binding(packer, binding);
				packed_binding = packer.get_buffer();
				packed_binding_size = packer.get_size();
			}
			size_t packed_binding_size;
			void *packed_binding;
	};

	struct ActivationCompare
	{
	  bool operator() (const Activation &t1, const Activation &t2) {
			if (t1.transition_def->get_id() == t2.transition_def->get_id()) {
				if (t1.process_id == t2.process_id) {
					if (t1.thread_id == t2.thread_id) {
						if (t1.packed_binding_size == t2.packed_binding_size) {
							return memcmp(t1.packed_binding,
										  t2.packed_binding,
                                          t1.packed_binding_size) < 0;
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

	class State  : public ca::StateBase<Net, Activation, Packet>
	{
		public:
			State(ca::NetDef *net_def) : StateBase(net_def) {}
			State(State &state) : StateBase(state) {}
			void pack_state(ca::Packer &packer);
			HashDigest compute_hash(hashid hash_id);
			void hash_activations(MHASH hash_thread);
			void hash_packets(MHASH hash_thread);
			void pack_activations(ca::Packer &packer);
			void pack_packets(ca::Packer &packer);
	};

	class Node
	{
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
			Core();
			~Core();
			void generate();
			void postprocess();
			void write_dot_file(const std::string &filename);
			Node * add_state(State *state);
			ca::NetDef * get_net_def() { return net_def; }
		protected:
			void run_analysis_deadlock(ca::Output &report);

			bool is_known_node(Node *node) const;
			Node *get_node(HashDigest digest) const;
			std::stack<Node*> not_processed;
			NodeMap nodes;
			Node *initial_node;
			ca::NetDef *net_def;
	};

	void init(int argc, char **argv, std::vector<ca::Parameter*> &parameters);
}
#endif // CAVERIF_STATESPACE_H
