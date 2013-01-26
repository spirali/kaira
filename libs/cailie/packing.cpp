
#include "packing.h"
#include <stdlib.h>

CaPacker::CaPacker(size_t size) : size(size) {
	buffer = (char*) malloc (size);
	buffer_pos = buffer;
}

CaPacker::CaPacker(size_t size, size_t reserved) : size(size + reserved) {
	buffer = (char*) malloc (size + reserved);
	buffer_pos = buffer + reserved;
}

void CaPacker::check_size(size_t data_size) {
	if (buffer_pos + data_size > buffer + size) {
		size += data_size;
		size *= 2;
		size_t p = buffer_pos - buffer;
		buffer = (char*) realloc(buffer, size);
		// TODO: ALLOCTESTT
		buffer_pos = buffer + p;
	}
}

void CaPacker::free()
{
	::free(buffer);
}

template<> int unpack(CaUnpacker &unpacker)
{
	return direct_unpack<int>(unpacker);
}
