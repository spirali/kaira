
#ifndef CAILIE_PACKING_H
#define CAILIE_PACKING_H

#include <string>
#include <string.h>
#include <ostream>
#include <vector>

namespace ca {

class Unpacker {

	public:
		Unpacker() {}
		Unpacker(void *mem) { buffer_pos = (char*) mem; }
		void * unpack_data(size_t size) { void *p = buffer_pos; buffer_pos += size; return p; }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
	protected:
		char *buffer_pos;
};

const size_t PACKER_DEFAULT_SIZE = 4000;

class Packer {

	public:
		Packer(size_t size);
		Packer(size_t size, size_t reserved);

		void pack_data(const void *mem, size_t size) {
			check_size(size);
			memcpy(buffer_pos, mem, size);
			buffer_pos += size;
		}

		void * peek() { return buffer_pos; }
		void move(size_t size) { check_size(size); buffer_pos += size; }
		size_t get_size() const { return buffer_pos - buffer; }
		char * get_buffer() const { return buffer; }
		void check_size(size_t size);
		void free();
	protected:
		char *buffer_pos;
		size_t size;
		char *buffer;
};

template<typename T> void direct_pack(Packer &packer, T value) {
	packer.pack_data(&value, sizeof(T));
}

void pack(Packer &packer, void *data, size_t size) {
	packer.pack_data(data, size);
}

inline void pack(Packer &packer, const int &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const size_t &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const double &value) {
	direct_pack(packer, value);
}

template<typename T> void pack(Packer &packer, const std::vector<T> &value) {
	pack(packer, value.size());
	typename std::vector<T>::const_iterator i;
	for (i = value.begin(); i != value.end(); i++) {
		pack(packer, *i);
	}
}

inline void pack(Packer &packer, const std::string &value) {
	size_t size = value.size();
	pack(packer, size);
	packer.pack_data(value.c_str(), size);
}

template<typename T> T direct_unpack(Unpacker &unpacker) {
	T *value = (T*)unpacker.unpack_data(sizeof(T));
	return *value;
}

void* unpack(Unpacker &unpacker, size_t size) {
	return unpacker.unpack_data(size);
}

template<typename T> T unpack(Unpacker &unpacker) {
	T x;
	unpack_to(unpacker, x);
	return x;
}

template<> int unpack<int>(Unpacker &unpacker);
template<> double unpack<double>(Unpacker &unpacker);
template<> size_t unpack<size_t>(Unpacker &unpacker);
template<> std::string unpack(Unpacker &unpacker);

template<typename T> void unpack_to(Unpacker &unpacker, std::vector<T> &value) {
	size_t size = unpack<size_t>(unpacker);
	value.reserve(size);
	for (size_t i = 0; i < size; i++) {
		value.push_back(unpack<T>(unpacker));
	}
}

}

#endif
