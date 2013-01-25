
#include "output.h"
#include <stdio.h>
#include <assert.h>

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
	size_t i = 0;
	while ((i = s.find(c, i)) != std::string::npos)
	{
		s.replace(i, 1, replace);
		i++;
	}
}

static void sanitize_string(std::string &s)
{
	find_and_replace(s, '&', "&amp;");
	find_and_replace(s, '<', "&lt;");
	find_and_replace(s, '>', "&gt;");
}

void CaOutput::set(const std::string & name, const std::string & value)
{
	std::string v = value;
	sanitize_string(v);
	find_and_replace(v, '\n', "\\n");
	find_and_replace(v, '\t', "\\t");
	find_and_replace(v, '\r', "\\r");
	find_and_replace(v, '\'', "\\'");
	_set(name, v);
}

void CaOutput::text(const std::string &text)
{
	if (open_tag) {
		fprintf(file, ">");
		open_tag = false;
	}
	std::string v = text;
	sanitize_string(v);
	fputs(v.c_str(), file);
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

void CaOutput::set(const std::string & name, const size_t value)
{
	fprintf(file, " %s='%llu'", name.c_str(), (unsigned long long) value);
}
