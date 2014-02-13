
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
		void set(const std::string &name, void *p);
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

#define CA_TOKEN_NAME(TYPE, VALUE) \
	template<> inline std::string token_name<TYPE > (const TYPE &VALUE)

#define CA_STREAM_TOKEN_NAME(TYPE) \
	CA_TOKEN_NAME(TYPE, value) { \
		std::stringstream s; \
		s << value; \
		return s.str(); \
	}

template<typename T> std::string token_name(const T &value) {
	return value.token_name();
}

CA_TOKEN_NAME(std::string, value) {
	return value;
}

CA_TOKEN_NAME(bool, value) {
	return value ? "true" : "false";
}

CA_STREAM_TOKEN_NAME(char);
CA_STREAM_TOKEN_NAME(int);
CA_STREAM_TOKEN_NAME(long);
CA_STREAM_TOKEN_NAME(long long);
CA_STREAM_TOKEN_NAME(unsigned char);
CA_STREAM_TOKEN_NAME(unsigned int);
CA_STREAM_TOKEN_NAME(unsigned long);
CA_STREAM_TOKEN_NAME(unsigned long long);
CA_STREAM_TOKEN_NAME(double);
CA_STREAM_TOKEN_NAME(float);

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

template<typename T1, typename T2> std::string token_name(const std::pair<T1, T2> &value) {
	std::stringstream s;
	s << "<" << token_name(value.first) << "," << token_name(value.second) << ">";
	return s.str();
}

template<typename T> std::string token_name(T *value) {
	std::stringstream s;
	s << value;
	return s.str();
}

}

#endif // CAILIE_OUTPUT_H
