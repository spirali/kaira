
#ifndef CAILIE_OUTPUT_H
#define CAILIE_OUTPUT_H

#include <string>
#include <vector>
#include <stack>
#include <sstream>
#include <stdint.h>

namespace ca {

class Output {
	public:
		Output(FILE *file);

		void child(const std::string &name);
		void back();
		void set(const std::string &name, const int i);
		void set(const std::string &name, const size_t i);
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

template<typename T> std::string to_string(const T &value) {
	std::stringstream s;
	s << value;
	return s.str();
}

template<typename T> std::string token_name(const T &value) {
	return to_string(value);
}

inline std::string token_name(const std::string &value) {
	return value;
}

inline std::string token_name(const bool &value) {
	return value ? "true" : "false";
}

template<typename T> std::string token_name(const std::vector<T> &value) {
	std::stringstream s;
	s << "[";
	typename std::vector<T>::const_iterator i = value.begin();

	if (i != value.end()) {
		s << token_name(*i);
		i++;
		for (; i != value.end(); i++) {
			s << "," << token_name(*i);
		}
	}

	s << "]";
	return s.str();
}

}

#endif // CAILIE_OUTPUT_H
