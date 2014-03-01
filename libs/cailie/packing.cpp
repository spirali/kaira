
#include "packing.h"
#include "utils.h"
#include <stdlib.h>
#include <stdio.h>

using namespace ca;

ca::Packer::Packer(size_t size) : size(size) {
	buffer = (char*) malloc (size);
	CA_ALLOC_TEST(buffer);
	buffer_pos = buffer;
}

ca::Packer::Packer(size_t size, size_t reserved) : size(size + reserved) {
	buffer = (char*) malloc (size + reserved);
	CA_ALLOC_TEST(buffer);
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
