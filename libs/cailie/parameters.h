
#ifndef CAILIE_PARAMETERS_H
#define CAILIE_PARAMETERS_H

#include <string>
#include <vector>

enum CaParameterMode {
	CA_PARAMETER_MANDATORY,
	CA_PARAMETER_OPTIONAL,
	CA_PARAMETER_CONSTANT,
	CA_PARAMETER_SETTED, // Parameter was explicitely setted
};

class CaParameter{

	public:
		CaParameter(const std::string &name,
					const std::string &description,
					CaParameterMode mode)
						: name(name), description(description), mode(mode)
						{}
		virtual ~CaParameter() {};
		virtual bool parse_value(const std::string &str) = 0;
		bool check_mode_before_set();
		bool check_mode_before_run();

		const std::string& get_name() const { return name; }
		const std::string& get_description() const { return description; }
	protected:
		std::string name;
		std::string description;
		CaParameterMode mode;
};

class CaParameterInt : public CaParameter{
	public:
		CaParameterInt(
			const std::string &name,
			const std::string &description,
			CaParameterMode mode,
			int default_value = 0) :
				CaParameter(name, description, mode), value(default_value)
			{}
		bool parse_value(const std::string &str);
		int operator()() const { return value; }
		void __set_value(int v) { value = v; }
	protected:
		int value;
};

void ca_set_parameter(
	std::vector<CaParameter*> &parameters,
	char *name,
	char *value);

#endif // CAILIE_PARAMETERS_H
