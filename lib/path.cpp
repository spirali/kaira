
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sstream>
#include "path.h"

CaPath caRoot(0);

CaPath::CaPath(int *nodes)
{
	set_data(nodes);
}

CaPath::CaPath(const CaPath &path)
{
	set_data(path.nodes);
}

CaPath & CaPath::operator= (const CaPath & path) {
	if (this != &path)
	{
		set_data(path.nodes);
	}
	return *this;
}

bool CaPath::operator== (const CaPath & path) {
	int *a = nodes;
	int *b = path.nodes;
	while(*a == *b) {
		if (*a == -1)
			return 1;
	}
	return 0;
}

CaPath::~CaPath()
{
	delete [] nodes;
}

CaPath::CaPath(int count, ...)
{
	nodes = new int[count + 1];
	int t;
	va_list vl;
	va_start(vl, count);
	for (t = 0; t < count; t++) {
		nodes[t] = va_arg(vl, int);
	}
	nodes[t] = -1;
	va_end(vl);
}

int CaPath::depth() const
{
	int d = 0;
	while (nodes[d] != -1) {
		d++;
	}
	return d;
}

void CaPath::set_data(int *nodes)
{
	int d = 0;
	while (nodes[d] != -1) {
		d++;
	}
	this->nodes = new int[d + 1];
	memcpy(this->nodes, nodes, sizeof(int) * (d + 1));
}

CaPath CaPath::apply(int levelup, int count, ...)
{
	if (levelup != 0) {
		printf("levelup not implemented\n");
		abort();
	}

	int d = depth();

	int nodes[d + count + 1];
	int t;
	va_list vl;
	va_start(vl, count);
	for (t = 0; t < d; t++) {
		nodes[t] = this->nodes[t];
	}
	for (; t < count; t++) {
		nodes[t] = va_arg(vl, int);
	}
	nodes[t] = -1;
	va_end(vl);
	return CaPath(nodes);
}

std::string CaPath::as_string() const
{
    if (*nodes == -1) {
        return "/";
    }

    std::stringstream s;
    int *i = nodes;
    do {
        s << "/" << *i;
        i++;
    } while(*i != -1);
    return s.str();
}
