
#include "parameters.h"
#include <stdio.h>
#include <stdlib.h>

using namespace ca;

bool ca::Parameter::check_mode_before_set()
{
	if (mode == PARAMETER_CONSTANT) {
		fprintf(stderr, "Parameter '%s' is constant and cannot be modifed.\n", \
						name.c_str());
		return false;
	}
	return true;
}

bool ca::Parameter::check_mode_before_run()
{
	if (mode == PARAMETER_MANDATORY) {
		fprintf(stderr, "Mandatory parameter '%s' required\n", name.c_str());
		return false;
	}
	return true;
}

bool ca::ParameterInt::parse_value(const std::string &str)
{
	char *err = NULL;
	value = strtol(str.c_str(), &err, 10);
	if (*err != '\0') {
		fprintf(stderr, "Invalid parameter value\n");
		return false;
	}
	mode = PARAMETER_SETTED;
	return true;
}

bool ca::ParameterDouble::parse_value(const std::string &str)
{
	char *err = NULL;
	value = strtod(str.c_str(), &err);
	if (*err != '\0') {
		fprintf(stderr, "Invalid parameter value\n");
		return false;
	}
	mode = PARAMETER_SETTED;
	return true;
}

bool ca::ParameterString::parse_value(const std::string &str)
{
    value = str;
	mode = PARAMETER_SETTED;
	return true;
}

void ca::set_parameter(
	std::vector<Parameter*> &parameters,
	char *name,
	char *value)
{
	for (size_t t = 0; t < parameters.size(); t++) {
		if (parameters[t]->get_name() == name) {
			if (!parameters[t]->check_mode_before_set()) {
				exit(1);
			}
			if (!parameters[t]->parse_value(value)) {
				exit(1);
			}
			return;
		}
	}
	fprintf(stderr, "Unknown parameter '%s'\n", name);
	exit(1);
}


