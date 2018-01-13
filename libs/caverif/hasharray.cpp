#include "hasharray.h"

using namespace cass;


HashArray::HashArray(hashid hash): _hash(hash)
{
	// Compute number of bytes needed to store one hash with size of its ample set
	_hash_size = mhash_get_block_size(_hash);
	_element_size = _hash_size + sizeof(byte);

	_size = _capacity = 0;
	_data = NULL;
}

HashArray::HashArray(size_t capacity, hashid hash): _size(0), _capacity(capacity), _hash(hash)
{
	// Compute number of bytes needed to store one hash with size of its ample set
	_hash_size = mhash_get_block_size(_hash);
	_element_size = _hash_size + sizeof(byte);

	init_memory(_capacity);
}

void HashArray::init_memory(size_t capacity)
{
	_data = (char*)malloc(capacity * _element_size);
	if (capacity * _element_size == 0) {
		std::cout << "HashArray size is 0.\n";
		exit(1);
	}
	if (_data == NULL) {
		std::cout << "ERROR IN MALLOC\n";
		exit(1);
	}
}

void HashArray::realloc(size_t capacity)
{
	size_t new_capacity = 2 * _capacity;
	while (new_capacity < capacity) new_capacity *= 2;

	char *new_data = (char*)malloc(new_capacity * _element_size);
	if (new_data == NULL) {
		std::cout << "ERROR IN MALLOC\n";
		exit(1);
	}

	memcpy(new_data, _data, _capacity * _element_size);

	free(_data);
	_data = new_data;
	_capacity = new_capacity;
}

void HashArray::resize(size_t size)
{
	if (_capacity == 0) {
		init_memory(size);
		_capacity = size;
		return;
	}
	if (_capacity < size) {
		realloc(size);
	}

	_size = size;
}

void HashArray::push(HashDigest hash, byte ample_size)
{
	if (_size == _capacity - 1) {
		resize(_capacity);
		_size--;
	}

	memcpy(_data + _size * _element_size, hash, _hash_size);
	_data[_size * _element_size + _hash_size] = ample_size;

	_size++;
}

void HashArray::clear()
{
	_size = 0;
	// memset(_data, 0, _element_size * _capacity);
}

HashArray::~HashArray()
{
	free(_data);
}



