#ifndef VERTICES_H_
#define VERTICES_H_

#include "hashing.h"

#include <set>

namespace cass {

	class Node;
	struct ProcessVertex;

	struct HashVertex
	{
		static int rank;

		HashVertex(): node(NULL), ample_size(0), owner(NULL), assigned(0), clear(1), hash(NULL) { }; // never called
		HashVertex(HashDigest hash, byte ample_size): node(NULL), ample_size(ample_size), owner(NULL), assigned(0), clear(1), hash(hash) { };
		HashVertex(HashDigest hash, Node *node, byte ample_size): node(node), ample_size(ample_size), owner(NULL), assigned(0), clear(1), hash(hash) { };
		~HashVertex();

		void assign();
		void release(ProcessVertex *process);

		Node* node;
		byte ample_size;
		std::vector<ProcessVertex*> processes;
		ProcessVertex* owner;
		int assigned;
		int clear;
		HashDigest hash;
	};

	struct HashVertexEq
	{
		bool operator()(const HashVertex* v1, const HashVertex* v2) const {
			return comparator(v1->hash, v2->hash);
		}

		HashDigestOrder comparator;
	};

	struct ProcessVertex
	{
		ProcessVertex(): nodeCount(1), next_size(0), rank(-1), realNodes(0) { };

		void clear();
		void assign(HashVertex *vertex);
		void release(HashVertex *vertex);
		void steal();
		void add_vertex(HashVertex* hash);

		bool redirectable();

		std::vector<HashVertex*> hashes;
		std::set<HashVertex*, HashVertexEq> assigned;
		int nodeCount;
		int next_size;
		int rank;
		long realNodes;
	};

}


#endif /* VERTICES_H_ */
