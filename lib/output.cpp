
#include "output.h"
#include <stdio.h>
#include <sstream>
#include <assert.h>

std::string ca_int_to_string(int i)
{
	std::stringstream osstream;
	osstream << i;
	return osstream.str();
}

std::string ca_float_to_string(float f)
{
	std::stringstream osstream;
	osstream << f;
	return osstream.str();
}

std::string ca_double_to_string(double d)
{
	std::stringstream osstream;
	osstream << d;
	return osstream.str();
}

std::string ca_bool_to_string(bool b)
{
	return b ? "true" : "false";
}

CaOutput::~CaOutput()
{
	while (!_stack.empty()) {
		delete _stack.top();
		_stack.pop();
	}
}

void CaOutput::child(const std::string & name)
{
	CaOutputBlock *block = new CaOutputBlock(name);
	_stack.push(block);
}

CaOutputBlock * CaOutput::back()
{
	CaOutputBlock *block = _stack.top();
	_stack.pop();
	if (!_stack.empty()) {
		CaOutputBlock *parent = _stack.top();
		parent->add_child(block);
	}
	return block;
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
	_set(name, v);
}

void CaOutput::_set(const std::string & name, const std::string & value)
{
	assert(!_stack.empty());
	CaOutputBlock *block = _stack.top();
	block->set(name, value);
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
	_set(name, ca_int_to_string(value));
}

void CaOutputBlock::set(const std::string &name, const std::string & value)
{
	std::pair<std::string,std::string> p(name, value);
	_attributes.push_back(p);
}

void CaOutputBlock::write(FILE *file)
{
	fprintf(file,"<%s", _name.c_str());

	std::vector<std::pair<std::string, std::string> >::iterator i;
	for (i = _attributes.begin(); i != _attributes.end(); i++) {
		fprintf(file, " %s='%s'", (*i).first.c_str(), (*i).second.c_str());
	}

	if (_children.size() > 0) {
		fprintf(file,">");
		std::vector<CaOutputBlock*>::iterator i;
		for (i = _children.begin(); i != _children.end(); i++) {
			(*i)->write(file);
		}
		fprintf(file,"</%s>", _name.c_str());
	} else {
		fprintf(file, " />");
	}
}

CaOutputBlock::~CaOutputBlock()
{
		std::vector<CaOutputBlock*>::iterator i;
		for (i = _children.begin(); i != _children.end(); i++) {
			delete (*i);
		}
}
