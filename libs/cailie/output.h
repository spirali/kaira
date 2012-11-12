
#ifndef CAILIE_OUTPUT_H
#define CAILIE_OUTPUT_H

#include <string>
#include <vector>
#include <stack>

class CaOutput {
	public:
		CaOutput(FILE *file);

		void child(const std::string &name);
		void back();
		void set(const std::string &name, const int i);
		void set(const std::string &name, const unsigned int i);
		void set(const std::string &name, const std::string &s);
		void set(const std::string &name, const char *s) {
			set(name, std::string(s));
		}
		void set(const std::string &name, const bool value);

		void text(const std::string &text);

	protected:
		void _set(const std::string &name, const std::string &s);
		FILE *file;
		std::stack<std::string> stack;
		bool open_tag;
};

std::string ca_int_to_string(const int &i);
std::string ca_double_to_string(const double &d);
std::string ca_float_to_string(const float &f);
std::string ca_bool_to_string(const bool &b);
std::string ca_string_to_string(const std::string &s);

#endif // CAILIE_OUTPUT_H
