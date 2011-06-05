
#include "packing.h"

CaPacker::CaPacker(size_t size) : size(size) {
	buffer = new char[size];
	buffer_pos = buffer;
}

CaPacker::CaPacker(size_t size, size_t reserved) {
	buffer = new char[size];
	buffer_pos = buffer + reserved;
}
