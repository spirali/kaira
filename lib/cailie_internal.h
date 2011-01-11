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

		void start_scheduler(CaContext *ctx);
};

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

class CaJob {
	public:
		CaJob(const CaTransition &t, CaJob *next = NULL) : transition(t), next(next) {};

		CaTransition const & transition;
		CaJob *next;
};

#endif
