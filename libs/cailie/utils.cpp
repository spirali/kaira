
#include <stdlib.h>
#include "utils.h"

size_t ca::parse_size_string(const std::string &str)
{
	char *end;
	size_t value = strtol(str.c_str(), &end, 10);

	switch (*end) {
		case 0 : return value;
		case 'K': value *= 1024; break;
		case 'M': value *= 1024 * 1024; break;
		case 'G': value *= 1024 * 1024 * 1024; break;
		default:
			return 0;
	}
	if ((*(end + 1)) == 0) {
		return value;
	} else {
		return 0;
	}
}
