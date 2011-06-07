
#ifndef CAILIE_PACKING_H
#define CAILIE_PACKING_H

#include <string>
#include <string.h>

class CaPacker {

	public:
		CaPacker(size_t size);
		CaPacker(size_t size, size_t reserved);
		void pack(const void *mem, size_t size) { memcpy(buffer_pos, mem, size); buffer_pos += size;  }
		void pack_size(size_t data) { pack(&data, sizeof(size_t)); }
		void pack_string(std::string str) { size_t s = str.size(); pack_size(s); pack(str.c_str(), s); }
		void pack_int(int data) { pack(&data, sizeof(int)); }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
		size_t get_size() const { return size; }
		char * get_buffer() const { return buffer; }
	protected:
		char *buffer_pos;
		size_t size;
		char *buffer;
};

class CaUnpacker {

	public:
		CaUnpacker() {}
		CaUnpacker(void *mem) { buffer_pos = (char*) mem; }
		void * unpack(size_t size) { void *p = buffer_pos; buffer_pos += size; return p; }
		void unpack(void *data, size_t size) { memcpy(data, buffer_pos, size); buffer_pos += size; }
		size_t unpack_size() { size_t *data = (size_t*)unpack(sizeof(size_t)); return *data; }
		int unpack_int() { int *data = (int*)unpack(sizeof(int)); return *data; }
		std::string unpack_string() { size_t s = unpack_size(); return std::string((char*) unpack(s), s); }
		void * peek() { return buffer_pos; }
		void move(size_t size) { buffer_pos += size; }
	protected:
		char *buffer_pos;
};

#endif
