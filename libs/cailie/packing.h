
#ifndef CAILIE_PACKING_H
#define CAILIE_PACKING_H

#include <string>
#include <string.h>
#include <ostream>
#include <vector>
#include <stdint.h>

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
		Packer(size_t size = PACKER_DEFAULT_SIZE);
		Packer(size_t size, size_t reserved);

		void pack_data(const void *mem, size_t size) {
			reserve(size);
			memcpy(buffer_pos, mem, size);
			buffer_pos += size;
		}

		void * peek() { return buffer_pos; }
		void move(size_t size) { reserve(size); buffer_pos += size; }
		size_t get_size() const { return buffer_pos - buffer; }
		char * get_buffer() const { return buffer; }
		void reserve(size_t size);
		void free();
		void write_to_file(const char *filename);
		void reset() { buffer_pos = buffer; }
	protected:
		char *buffer_pos;
		size_t size;
		char *buffer;
};

template<typename T> void direct_pack(Packer &packer, T value) {
	packer.pack_data(&value, sizeof(T));
}

inline void pack(Packer &packer, void *data, size_t size) {
	packer.pack_data(data, size);
}

inline void pack(Packer &packer, const char &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const unsigned char &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const int &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const long &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const long long &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const unsigned int &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const unsigned long &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const unsigned long long &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const double &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const float &value) {
	direct_pack(packer, value);
}

inline void pack(Packer &packer, const bool &value) {
	direct_pack(packer, value);
}

template<typename T> void pack(Packer &packer, const std::vector<T> &value) {
	pack(packer, value.size());
	typename std::vector<T>::const_iterator i;
	for (i = value.begin(); i != value.end(); i++) {
		pack(packer, *i);
	}
}

template<typename T1, typename T2> void pack(Packer &packer, const std::pair<T1, T2> &value) {
	pack(packer, value.first);
	pack(packer, value.second);
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

inline void* unpack(Unpacker &unpacker, size_t size) {
	return unpacker.unpack_data(size);
}

template<typename T> T unpack(Unpacker &unpacker) {
	T x;
	unpack_to(unpacker, x);
	return x;
}

template<> char unpack(Unpacker &unpacker);
template<> int unpack(Unpacker &unpacker);
template<> long unpack(Unpacker &unpacker);
template<> long long unpack(Unpacker &unpacker);
template<> unsigned char unpack(Unpacker &unpacker);
template<> unsigned int unpack(Unpacker &unpacker);
template<> unsigned long unpack(Unpacker &unpacker);
template<> unsigned long long unpack(Unpacker &unpacker);
template<> double unpack(Unpacker &unpacker);
template<> float unpack(Unpacker &unpacker);
template<> bool unpack(Unpacker &unpacker);
template<> std::string unpack(Unpacker &unpacker);

template<typename T> void unpack_to(Unpacker &unpacker, std::vector<T> &value) {
	size_t size = unpack<size_t>(unpacker);
	value.reserve(size);
	for (size_t i = 0; i < size; i++) {
		value.push_back(unpack<T>(unpacker));
	}
}

template<typename T1, typename T2> void unpack_to(Unpacker &unpacker, std::pair<T1, T2> &value) {
	value.first = unpack<T1>(unpacker);
	value.second = unpack<T2>(unpacker);
}

}

#endif
