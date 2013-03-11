
#ifndef CAILIE_PARAMETERS_H
#define CAILIE_PARAMETERS_H

#include <string>
#include <vector>

namespace ca {

enum ParameterMode {
	PARAMETER_MANDATORY,
	PARAMETER_OPTIONAL,
	PARAMETER_CONSTANT,
	PARAMETER_SETTED, // Parameter was explicitely setted
};

class Parameter{

	public:
		Parameter(const std::string &name,
					const std::string &description,
					ParameterMode mode)
						: name(name), description(description), mode(mode)
						{}
		virtual ~Parameter() {};
		virtual bool parse_value(const std::string &str) = 0;
		bool check_mode_before_set();
		bool check_mode_before_run();

		const std::string& get_name() const { return name; }
		const std::string& get_description() const { return description; }
	protected:
		std::string name;
		std::string description;
		ParameterMode mode;
};

class ParameterInt : public Parameter{
	public:
		ParameterInt(
			const std::string &name,
			const std::string &description,
			ParameterMode mode,
			int default_value = 0) :
				Parameter(name, description, mode), value(default_value)
			{}
		bool parse_value(const std::string &str);
		int operator()() const { return value; }
		void __set_value(int v) { value = v; }
	protected:
		int value;
};

class ParameterDouble : public Parameter{
	public:
		ParameterDouble(
			const std::string &name,
			const std::string &description,
			ParameterMode mode,
			double default_value = 0.0) :
				Parameter(name, description, mode), value(default_value)
			{}
		bool parse_value(const std::string &str);
		double operator()() const { return value; }
		void __set_value(double v) { value = v; }
	protected:
		double value;
};

class ParameterString : public Parameter{
	public:
		ParameterString(
			const std::string &name,
			const std::string &description,
			ParameterMode mode,
			std::string default_value = "") :
				Parameter(name, description, mode), value(default_value)
			{}
		bool parse_value(const std::string &str);
        std::string operator()() const { return value; }
		void __set_value(std::string v) { value = v; }
	protected:
        std::string value;
};

void set_parameter(
	std::vector<Parameter*> &parameters,
	char *name,
	char *value);

}

#endif // CAILIE_PARAMETERS_H
