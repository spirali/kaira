#ifndef CAILIE_INTERNAL_H
#define CAILIE_INTERNAL_H

#include "cailie.h"

class CaModule {
	public:
		virtual ~CaModule() {};
		virtual int main(int nodes_count, InitFn *main_fn) = 0;
		virtual void send(CaContext *ctx, int target, int data_id, void *data, size_t size) = 0;
		virtual int recv(CaContext *ctx, RecvFn *recv, void *places) = 0;
		virtual void idle() {};
		virtual void quit(CaContext *ctx) = 0;

		void start_sheduler(CaContext *ctx);
};

class CaOutputBlock {
	public:
	
		CaOutputBlock(std::string name) { _name = name; }
		~CaOutputBlock();

		void add_child(CaOutputBlock *block) { _children.push_back(block); }
		void set(std::string name, std::string value);

		void write(FILE *file);
	
	protected:
		std::string _name;
		std::vector<std::pair<std::string, std::string> > _attributes;
		std::vector<CaOutputBlock*> _children;
};

#endif
