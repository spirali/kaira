
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

template<> int32_t ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<int32_t>(unpacker);
}

template<> int64_t ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<int64_t>(unpacker);
}

template<> uint32_t ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<uint32_t>(unpacker);
}

template<> uint64_t ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<uint64_t>(unpacker);
}

template<> double ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<double>(unpacker);
}

template<> float ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<float>(unpacker);
}

template<> bool ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<bool>(unpacker);
}

template<> char ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<char>(unpacker);
}

template<> unsigned char ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<unsigned char>(unpacker);
}

template<> std::string ca::unpack(Unpacker &unpacker)
{
	size_t size = unpack<size_t>(unpacker);
	return std::string((char*) unpacker.unpack_data(size), size);
}
