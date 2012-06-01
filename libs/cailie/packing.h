
#ifndef CAILIE_PACKING_H
#define CAILIE_PACKING_H

#include <string>
#include <string.h>

class CaUnpacker {

	public:
		CaUnpacker() {}
		CaUnpacker(void *mem) { buffer_pos = (char*) mem; }
		void * unpack(size_t size) { void *p = buffer_pos; buffer_pos += size; return p; }
		void unpack(void *data, size_t size) { memcpy(data, buffer_pos, size); buffer_pos += size; }
		size_t unpack_size() { size_t *data = (size_t*)unpack(sizeof(size_t)); return *data; }
		double unpack_double() { double *data = (double*)unpack(sizeof(double)); return *data; }
		float unpack_float() { float *data = (float*)unpack(sizeof(float)); return *data; }
		int unpack_int() { int *data = (int*)unpack(sizeof(int)); return *data; }
		std::string unpack_string() { size_t s = unpack_size(); return std::string((char*) unpack(s), s); }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
	protected:
		char *buffer_pos;
};

const size_t CA_PACKER_DEFAULT_SIZE = 4000;

class CaPacker {

	public:
		CaPacker(size_t size);
		CaPacker(size_t size, size_t reserved);
		void pack(const void *mem, size_t size) { check_size(size); memcpy(buffer_pos, mem, size); buffer_pos += size;  }
		void pack_size(size_t data) { pack(&data, sizeof(size_t)); }
		void pack_string(std::string str) { size_t s = str.size(); pack_size(s); pack(str.c_str(), s); }
		void pack_int(int data) { pack(&data, sizeof(int)); }
		void pack_float(float data) { pack(&data, sizeof(float)); }
		void pack_double(double data) { pack(&data, sizeof(double)); }
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

#endif
