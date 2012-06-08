
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

void CaPacker::check_size(size_t new_size) {
	if (buffer_pos + new_size > buffer + size) {
		size += new_size;
		size *= 2;
		buffer = (char*) realloc(buffer, size);
		// TODO: ALLOCTESTT
	}
}

void CaPacker::free()
{
	::free(buffer);
}
