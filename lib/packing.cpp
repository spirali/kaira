
#include "packing.h"
#include <stdlib.h>

CaPacker::CaPacker(size_t size) : size(size) {
	buffer = (char*) malloc (size);
	buffer_pos = buffer;
}
#include <stdio.h> // DEBUG
CaPacker::CaPacker(size_t size, size_t reserved) : size(size + reserved) {
	buffer = (char*) malloc (size + reserved);
	buffer_pos = buffer + reserved;
}
