
#include "output.h"
#include <stdio.h>
#include <sstream>
#include <assert.h>

std::string ca_int_to_string(const int &i)
{
	std::stringstream osstream;
	osstream << i;
	return osstream.str();
}

std::string ca_float_to_string(const float &f)
{
	std::stringstream osstream;
	osstream << f;
	return osstream.str();
}

std::string ca_double_to_string(const double &d)
{
	std::stringstream osstream;
	osstream << d;
	return osstream.str();
}

std::string ca_bool_to_string(const bool &b)
{
	return b ? "true" : "false";
}

std::string ca_string_to_string(const std::string &s)
{
	return s;
}

CaOutput::CaOutput(FILE *file) : file(file), open_tag(false)
{
}

void CaOutput::child(const std::string & name)
{
	if (open_tag) {
		fprintf(file, ">");
	}
	fprintf(file, "<%s", name.c_str());
	stack.push(name);
	open_tag = true;
}

void CaOutput::back()
{
	if (open_tag) {
		open_tag = false;
		fprintf(file, " />");
	} else {
		fprintf(file, "</%s>", stack.top().c_str());
	}
	stack.pop();
}

static void find_and_replace(std::string &s, const char c, const std::string replace)
{
	size_t i;
	while ((i = s.find(c)) != std::string::npos)
	{
		s.replace(i, 1, replace);
	}
}

void CaOutput::set(const std::string & name, const std::string & value)
{
	std::string v = value;
	find_and_replace(v, '&', "&amp;");
	find_and_replace(v, '<', "&lt;");
	find_and_replace(v, '>', "&gt;");
	find_and_replace(v, '\n', "\\n");
	find_and_replace(v, '\t', "\\t");
	find_and_replace(v, '\r', "\\r");
	find_and_replace(v, '\'', "\\'");
	_set(name, v);
}

void CaOutput::_set(const std::string & name, const std::string & value)
{
	fprintf(file, " %s='%s'", name.c_str(), value.c_str());
}

void CaOutput::set(const std::string & name, const bool value)
{
	if (value) {
		_set(name, "true");
	} else {
		_set(name, "false");
	}
}

void CaOutput::set(const std::string & name, const int value)
{
	fprintf(file, " %s='%i'", name.c_str(), value);
}

void CaOutput::set(const std::string & name, const unsigned int value)
{
	fprintf(file, " %s='%u'", name.c_str(), value);
}
