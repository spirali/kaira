#ifndef CAILIE_INTERNAL_H
#define CAILIE_INTERNAL_H

#include "cailie.h"
#include <map>
#include <string>

typedef std::map <int, CaContext*> CaContextsMap;

class CaModule {
	public:
		virtual ~CaModule() {}
		virtual int main(int nodes_count, InitFn *main_fn) = 0;
};

class CaProcess {
      public:
		CaProcess(int process_id);
		virtual ~CaProcess();

		/* send method is responsible for freeing "data" */
		virtual void send(CaContext *ctx, int target, int data_id, void *data, size_t size) = 0;
		virtual int recv() = 0;
		virtual void idle() {}
		virtual void quit(CaContext *ctx) = 0;
		void context_halted(CaContext *ctx) { _running_nodes--; }

		void start_scheduler();
		int get_process_id() { return _process_id; }
		CaLogger * get_logger() { return _logger; }

		virtual size_t get_reserved_prefix_size() = 0;

		/* Logging */
		void init_log(const std::string& logname);
		void stop_log();
		void log_enabled_transitions(int skip_node, int skip_transition);
		virtual void start_logging(CaContext *ctx, const std::string& logname) = 0;
		virtual void stop_logging(CaContext *ctx) = 0;

      protected:
		void write_report(FILE *out);
		int _process_id;
		CaContextsMap _contexts;
		int _running_nodes;
		CaLogger *_logger;
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
		CaJob(CaContext *ctx, const CaTransition &t, CaJob *next = NULL) : ctx(ctx), transition(t), next(next) {};

		int call() { return transition.call(ctx); }
		CaContext * get_context() { return ctx; }

		CaContext *ctx;
		CaTransition const & transition;
		CaJob *next;
};

extern NodeToProcessFn *ca_node_to_process;
extern std::string ca_log_default_name;

#endif
