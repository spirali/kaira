
#include "packing.h"
#include <stdlib.h>
#include <stdio.h>

using namespace ca;

ca::Packer::Packer(size_t size) : size(size) {
	buffer = (char*) malloc (size);
	buffer_pos = buffer;
}

ca::Packer::Packer(size_t size, size_t reserved) : size(size + reserved) {
	buffer = (char*) malloc (size + reserved);
	buffer_pos = buffer + reserved;
}

void ca::Packer::reserve(size_t data_size) {
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

void ca::Packer::write_to_file(const char *filename)
{
	FILE *f = fopen(filename, "w");
	if (f == NULL) {
		perror("ca::Packer::write_to_file");
		exit(-1);
	}
	fwrite(buffer, buffer_pos - buffer, 1, f);
	fclose(f);
}

template<> int ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<int>(unpacker);
}

template<> long ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<long>(unpacker);
}

template<> long long ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<long long>(unpacker);
}

template<> unsigned int ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<unsigned int>(unpacker);
}

template<> unsigned long ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<unsigned long>(unpacker);
}

template<> unsigned long long ca::unpack(Unpacker &unpacker)
{
	return direct_unpack<unsigned long long>(unpacker);
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
