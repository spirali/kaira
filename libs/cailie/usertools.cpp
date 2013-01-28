
#include "usertools.h"

std::vector<int> ca::range(int from, int upto)
{
	std::vector<int> v;
	int t;
	for (t = from; t < upto; t++) {
		v.push_back(t);
	}
	return v;
}
