#ifndef HASHARRAY_H_
#define HASHARRAY_H_

#include <iostream>
#include <string>

#include "hashing.h"

namespace cass {

class HashArray {

public:
	HashArray(hashid hash);
	HashArray(size_t capacity, hashid hash);
	~HashArray();

	void resize(size_t size);
	void clear();

	void push(HashDigest hash, byte ample_size);

	size_t size() const { return _size; }
	size_t capacity() const { return _capacity; }
	size_t hash_size() const { return _hash_size; }
	size_t element_size() const { return _element_size; }

	char* operator[](size_t i) { return _data + _element_size * i; }

	byte ample_size(size_t i) const { return _data[i * _element_size + _hash_size]; }

private:
	void realloc(size_t capacity);
	void init_memory(size_t capacity);

	char* _data;
	size_t _size;
	size_t _capacity;
	size_t _hash_size;
	size_t _element_size;
	hashid _hash;
};

}

#endif /* HASHARRAY_H_ */
