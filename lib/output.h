
#ifndef CAILIE_OUTPUT_H
#define CAILIE_OUTPUT_H

#include <string>
#include <vector>
#include <stack>

class CaOutputBlock {
	public:

		CaOutputBlock(const std::string & name) { _name = name; }
		~CaOutputBlock();

		void add_child(CaOutputBlock *block) { _children.push_back(block); }
		void set(const std::string & name, const std::string & value);

		void write(FILE *file);

	protected:
		std::string _name;
		std::vector<std::pair<std::string, std::string> > _attributes;
		std::vector<CaOutputBlock*> _children;
};

class CaOutput {
	public:
		~CaOutput();

		void child(const std::string &name);
		CaOutputBlock * back();
		void set(const std::string &name, const int i);
		void set(const std::string &name, const std::string &s);
		void set(const std::string &name, const char *s) {
			set(name, std::string(s));
		}
		void set(const std::string &name, const bool value);
	private:
		void _set(const std::string &name, const std::string &s);
		std::stack<CaOutputBlock*> _stack;
};

std::string ca_int_to_string(int i);
std::string ca_double_to_string(double d);
std::string ca_float_to_string(float f);

#endif // CAILIE_OUTPUT_H
