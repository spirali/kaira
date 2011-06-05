#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <sstream>

#include "path.h"
#include "output.h"
#include "process.h"

CaPath caRoot(0);

CaPath::CaPath(int *nodes)
{
	set_data(nodes);
}

CaPath::CaPath(const CaPath &path)
{
	set_data(path.nodes);
}

CaPath & CaPath::operator= (const CaPath & path)
{
	if (this != &path) {
		set_data(path.nodes);
	}
	return *this;
}

bool CaPath::operator== (const CaPath & path)
{
	int *a = nodes;
	int *b = path.nodes;
	while(*a == *b) {
		if (*a == -1)
			return 1;
		a++;
		b++;
	}
	return 0;
}

CaPath::CaPath(const char *string)
{
	int len = strlen(string);
	char *tmp = (char*) alloca(len + 1);
	int *nodes = (int*) alloca(sizeof(int) * (len + 1));
	int i = 0;
	int n = 0;

	if (string[i] == '/')
		i++;

	while (string[i] != 0) {
		char *c = tmp;
		while(isdigit(string[i])) {
			*c = string[i];
			c++;
			i++;
		}
		*c = 0;
		sscanf(tmp, "%i", &nodes[n]);
		n++;
		if (string[i] == '/')
			i++;
	}
	nodes[n] = -1;
	set_data(nodes);
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

int CaPath::owner_id(CaProcess *process, int unit_id) const
{
	if (nodes[0] == -1) {
		return 0;
	} else {
		return (nodes[0] + 1) % process->get_process_count();
	}
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
	int d = depth() - levelup;
	if (d < 0) {
		d = 0;
	}

	int nodes[d + count + 1];
	int t;
	va_list vl;
	va_start(vl, count);
	for (t = 0; t < d; t++) {
		nodes[t] = this->nodes[t];
	}
	for (t = 0; t < count; t++) {
		nodes[t + d] = va_arg(vl, int);
	}
	nodes[t + d] = -1;
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

int CaPath::last_component() const
{
	int i = 0;
	int t = 0;
	while(nodes[t] != -1) {
		i = nodes[t];
		t++;
	}
	return i;
}

CaMultiPath::CaMultiPath(const std::string &definition, ...) : definition(definition)
{
	size_t count = args_count();
	args = new int[count];
	int i = 0;
	va_list vl;
	va_start(vl, definition);
	for (size_t t = 0; t < definition.size(); t++) {
		switch (definition[t]) {
		case 's':
			args[i++] = va_arg(vl, int);
			break;
		case 'r':
			args[i++] = va_arg(vl, int);
			args[i++] = va_arg(vl, int);
			break;
		}

	}
	va_end(vl);
}

CaMultiPath & CaMultiPath::operator= (const CaMultiPath & mpath)
{
	int count = mpath.args_count();
	args = new int[count];
	memcpy(args, mpath.args, sizeof(int) * count);
	return *this;
}

CaMultiPath::~CaMultiPath()
{
	delete [] args;
}

CaMultiPath::CaMultiPath(const CaMultiPath &mpath) : definition(mpath.definition)
{
	int count = mpath.args_count();
	args = new int[count];
	memcpy(args, mpath.args, sizeof(int) * count);
}

size_t CaMultiPath::args_count() const
{
	size_t c = 0;
	for (size_t t = 0; t < definition.size(); t++) {
		switch (definition[t]) {
		case 's':
			c += 1;
			break;
		case 'r':
			c += 2;
			break;
		}
	}
	return c;
}

CaMultiPath::Iterator::Iterator(const CaMultiPath &mpath) : mpath(mpath), has_next_path(true)
{
	size_t path_size = mpath.path_size();
	nodes = new int[path_size + 1];
	int i = 0;
	for (size_t t = 0; t < path_size; t++) {
		switch (mpath.definition[t]) {
		case 's':
			nodes[t] = mpath.args[i];
			i++;
			break;
		case 'r':
			if (mpath.args[i + 1] < mpath.args[i]) {
				has_next_path = false;
				return;
			}
			nodes[t] = mpath.args[i];
			i+=2;
			break;
		}
	}
	nodes[path_size] = -1;
}

CaPath CaMultiPath::Iterator::next()
{
	CaPath path(nodes);
	size_t path_size = mpath.path_size();
	int i = 0;
	for (size_t t = 0; t < path_size; t++) {
		i++;
		if (mpath.definition[t] == 'r') {
			nodes[t]++;
			if (nodes[t] > mpath.args[i]) {
				nodes[t] = mpath.args[i - 1];
			} else {
				return path;
			}
			i++;
		}

	}
	has_next_path = false;
	return path;
}

CaMultiPath::Iterator::~Iterator()
{
	delete [] nodes;
}
