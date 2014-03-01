
#ifndef CAILIE_PACKING_H
#define CAILIE_PACKING_H

#include <string>
#include <string.h>
#include <ostream>
#include <vector>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

namespace ca {

class Packer;
class Unpacker;

#define CA_PACK(TYPE, PACKER, VALUE) template<> inline void pack<TYPE >(Packer &PACKER, const TYPE &VALUE)
#define CA_UNPACK(TYPE, PACKER, VALUE) template<> inline void unpack<TYPE >(Unpacker &PACKER, TYPE &VALUE)

#define CA_TRIVIALLY_PACKABLE(TYPE) \
	template<> inline bool is_trivially_packable<TYPE >() { return true; } \
	CA_PACK(TYPE, p, value) { p.direct_pack(value); } \
	CA_UNPACK(TYPE, p, value) { p.direct_unpack(value); }

#define CA_FIXED_SIZE(TYPE) template<> inline size_t fixed_size<TYPE >()

template<typename T> bool is_trivially_packable() {
	return false;
}

template<typename T> size_t fixed_size() {
	if (is_trivially_packable<T>()) {
		return sizeof(T);
	} else {
		return 0; // 0 = is not type with fixed size
	}
}

template<typename T> void pack(Packer &packer, const T &value) {
	value.pack(packer);
}

template<typename T> void unpack(Unpacker &unpacker, T &value) {
	value.unpack(unpacker);
}


class Unpacker {

	public:
		Unpacker(): buffer_pos(NULL), buffer(NULL) {}

		Unpacker(void *mem) {
			buffer_pos = static_cast<char*>(mem);
			buffer = static_cast<char*>(mem);
		}

		template<typename T> void direct_unpack(T &value) {
			void *data = unpack_data(sizeof(T));
			memcpy(&value, data, sizeof(T));
		}

		template<typename T> Unpacker& operator>>(T &value) {
			unpack(*this, value);
			return *this;
		}

		void *unpack_data(size_t size) {
			void *p = buffer_pos;
			buffer_pos += size;
			return p;
		}

		void unpack_data(void *data, size_t size) {
			memcpy(data, buffer_pos, size);
			buffer_pos += size;
		}

		void * peek() {
			return buffer_pos;
		}

		void move(size_t size) {
			buffer_pos += size;
		}

		template<typename T> void unpack_aligned(T &value, size_t align) {
			char *pos = buffer_pos + align;
			unpack(*this, value);
			if (buffer_pos > pos) {
				fprintf(stderr, "unpack_aligned failed\n");
				exit(1);
			} else {
				buffer_pos = pos;
			}
		}

		template<typename T> void unpack_at(T &value, int position) {
			buffer_pos = buffer + position;
			unpack(*this, value);
		}

	protected:
		char *buffer_pos;
		char *buffer;
};

const size_t PACKER_DEFAULT_SIZE = 4000;

class Packer {

	public:
		Packer(size_t size = PACKER_DEFAULT_SIZE);
		Packer(size_t size, size_t reserved);

		template<typename T> void direct_pack(const T &value) {
			pack_data(&value, sizeof(T));
		}

		template<typename T> Packer& operator<<(const T &value) {
			pack(*this, value);
			return *this;
		}

		void pack_data(const void *mem, size_t size) {
			reserve(size);
			memcpy(buffer_pos, mem, size);
			buffer_pos += size;
		}

		void * peek() const {
			return buffer_pos;
		}

		void move(size_t size) {
			reserve(size);
			buffer_pos += size;
		}

		size_t get_size() const {
			return buffer_pos - buffer;
		}

		char * get_buffer() const {
			return buffer;
		}

		void reserve(size_t size);
		void free();
		void write_to_file(const char *filename);
		void reset() {
			buffer_pos = buffer;
		}

		template<typename T> void pack_aligned(const T &value, size_t align) {
			char *pos = buffer_pos + align;
			pack(*this, value);
			if (buffer_pos > pos) {
				fprintf(stderr, "pack_aligned failed\n");
				exit(1);
			} else {
				buffer_pos = pos;
			}
		}

	protected:
		char *buffer_pos;
		size_t size;
		char *buffer;
};

inline void pack(Packer &packer, const void *data, size_t size) {
	packer.pack_data(data, size);
}

CA_TRIVIALLY_PACKABLE(char);
CA_TRIVIALLY_PACKABLE(int);
CA_TRIVIALLY_PACKABLE(long);
CA_TRIVIALLY_PACKABLE(long long);
CA_TRIVIALLY_PACKABLE(unsigned char);
CA_TRIVIALLY_PACKABLE(unsigned int);
CA_TRIVIALLY_PACKABLE(unsigned long);
CA_TRIVIALLY_PACKABLE(unsigned long long);
CA_TRIVIALLY_PACKABLE(double);
CA_TRIVIALLY_PACKABLE(float);
CA_TRIVIALLY_PACKABLE(bool);

CA_PACK(std::string, p, value) {
	size_t size = value.size();
	ca::pack(p, size);
	p.pack_data(value.data(), size);
}

CA_UNPACK(std::string, p, value) {
	size_t size;
	unpack(p, size);
	value.assign(static_cast<char*>(p.unpack_data(size)), size);
}

template<typename T> void pack(Packer &packer, const std::vector<T> &value) {
	pack(packer, value.size());
	if (is_trivially_packable<T>()) {
		pack(packer, &value[0], sizeof(T) * value.size());
	} else {
		typename std::vector<T>::const_iterator i;
		for (i = value.begin(); i != value.end(); i++) {
			pack(packer, *i);
		}
	}
}

template<typename T> void pack_with_step(Packer &packer, const std::vector<T> &value, size_t size) {
	if (is_trivially_packable<T>() && size == sizeof(T)) {
		pack(packer, &value[0], sizeof(T) * value.size());
	} else {
		for (size_t i = 0; i < value.size(); i++) {
			packer.pack_aligned(value[i], size);
		}
	}
}

template<typename T> void unpack_with_step(Unpacker &unpacker, std::vector<T> &value, size_t size, int count) {
	if (is_trivially_packable<T>() && size == sizeof(T)) {
		unpacker.unpack_data(&value[0], sizeof(T) * count);
	} else {
		for (size_t i = 0; i < count; i++) {
			T v;
			unpacker.unpack_aligned(v, size);
			value.push_back(v);
		}
	}
}

template<typename T> void unpack_with_displs(Unpacker &unpacker, std::vector<T> &value, int count, int *displs) {
	for (size_t i = 0; i < count; i++) {
			T v;
			unpacker.unpack_at(v, displs[i]);
			value.push_back(v);
	}
}

template<typename T> void unpack(Unpacker &unpacker, std::vector<T> &value) {
	size_t size;
       	unpack(unpacker, size);
	if (is_trivially_packable<T>()) {
		T* data = static_cast<T*>(unpacker.unpack_data(size));
		value.assign(data, data + size);
	} else {
		value.reserve(size);
		for (size_t i = 0; i < size; i++) {
			T v;
			unpack(unpacker, v);
			value.push_back(v);
		}
	}
}

template<typename T1, typename T2> void unpack(Unpacker &unpacker, std::pair<T1, T2> &value) {
	unpack(value.first);
	unpack(value.second);
}

template<typename T1, typename T2> void pack(Packer &packer, const std::pair<T1, T2> &value) {
	pack(packer, value.first);
	pack(packer, value.second);
}
}

#endif
