
#include "packing.h"
#include <stdlib.h>

using namespace ca;

ca::Packer::Packer(size_t size) : size(size) {
	buffer = (char*) malloc (size);
	buffer_pos = buffer;
}

ca::Packer::Packer(size_t size, size_t reserved) : size(size + reserved) {
	buffer = (char*) malloc (size + reserved);
	buffer_pos = buffer + reserved;
}

void ca::Packer::check_size(size_t data_size) {
	if (buffer_pos + data_size > buffer + size) {
		size += data_size;
		size *= 2;
		size_t p = buffer_pos - buffer;
		buffer = (char*) realloc(buffer, size);
		// TODO: ALLOCTESTT
		buffer_pos = buffer + p;
	}
}

void ca::Packer::free()
{
	::free(buffer);
}

template<> int ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<int>(unpacker);
}

template<> size_t ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<size_t>(unpacker);
}

template<> double ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<double>(unpacker);
}

template<> std::string ca::unpack(Unpacker &unpacker)
{
	size_t size = unpack<size_t>(unpacker);
	return std::string((char*) unpacker.unpack_data(size), size);
}
