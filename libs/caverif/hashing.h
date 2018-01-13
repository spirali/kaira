#ifndef HASHING_H_
#define HASHING_H_

#include <google/sparse_hash_map>
#include <mhash.h>

#define CASS_HASH_ID MHASH_MD5

namespace cass {

	typedef unsigned char byte;
	typedef void* HashDigest;

	struct HashDigestHash
	{
		HashDigestHash() : size(mhash_get_block_size(MHASH_MD5)) {}

		size_t operator()(HashDigest hash) const {
			size_t *h = (size_t*) hash;
			return *h;
		}
		size_t size;
	};

	struct HashDigestEq
	{
		HashDigestEq() : size(mhash_get_block_size(MHASH_MD5)) {}

		bool operator()(HashDigest hash1, HashDigest hash2) const {
			return !memcmp(hash1, hash2, size);
		}
		size_t size;
	};

	struct HashDigestOrder
	{
		HashDigestOrder(): size(mhash_get_block_size(MHASH_MD5)) {}

		bool operator()(HashDigest hash1, HashDigest hash2) const {
			return memcmp(hash1, hash2, size) < 0;
		}
		size_t size;
	};

}




#endif /* HASHING_H_ */
