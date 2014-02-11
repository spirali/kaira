
#ifndef CAILIE_UTILS_H
#define CAILIE_UTILS_H

#include <string>

namespace ca {

/* parse_size_string("100") == 100
 * parse_size_string("20M") == 20 * 1024 * 1024
 * parse_size_string("ABC") == 0
 * support suffixes: K, M, G
 */

#define CA_ALLOC_TEST(x) // TODO

size_t parse_size_string(const std::string &str);

}

#endif
