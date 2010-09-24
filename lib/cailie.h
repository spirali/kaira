
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <string>
#include <stack>
#include <vector>

class CaContext;
class CaOutput;

/* Callbacks */
typedef int(TransitionFn)(CaContext * ctx, void *places);
typedef void(InitFn)(CaContext * ctx);
typedef void(RecvFn)(void *places,  int data_id, void *data, size_t size);
typedef void(ReportFn)(CaContext * ctx, void *data, CaOutput *out);

class CaModule;
class CaTransition;

class CaContext {
	
	public:
		CaContext(int node, CaModule *module);

		/* CaContext user API */
		int node() { return _node; }
		int iid() { return _iid; }
		int instances() { return _instances; }
		int halt() { return _halt_flag = true; }
		void quit();
		
		/* Internal */
		CaModule * _get_module() { return _module; }
		void _init(int iid, int instances, void * places, RecvFn *recv_fn, ReportFn *report_fn);
		void _register_transition(int id, TransitionFn *fn);
		int _check_halt_flag() { return _halt_flag; }
		void * _get_places() { return _places; }
		RecvFn * _get_recv_fn() { return _recv_fn; }
		std::vector<CaTransition> _get_transitions() { return _transitions; }
		bool _find_transition(int id, CaTransition &transition);
		ReportFn * _get_report_fn() { return _report_fn; }
	protected:
		int _node;
		int _iid;
		int _instances;
		bool _halt_flag;
		CaModule *_module;
		RecvFn *_recv_fn;
		void *_places;
		std::vector<CaTransition> _transitions;
		ReportFn * _report_fn;
};

class CaOutputBlock;

class CaOutput {
	public:
		~CaOutput();

		void child(std::string name);
		CaOutputBlock * back();
		void set(std::string name, int i);
		void set(std::string name, std::string s);

	private:	
		std::stack<CaOutputBlock*> _stack;
};

class CaTransition {
	public:
		CaTransition() {} 
		CaTransition(int id, TransitionFn *fn) { this->id = id; this->function = fn; }
		int call(CaContext *ctx, void *places) { return function(ctx, places); }
		int get_id() { return id; }
	protected:
		TransitionFn *function;
		int id;
};



/* Init functions */
void ca_start(CaContext *ctx, void *data, TransitionFn **tf, RecvFn *recv_fn);
void ca_main(int nodes_count, InitFn *init_fn);

/* Communications */
void ca_send(CaContext *ctx, int node, int data_id, void *data, size_t data_size);

/* Others */
void ca_parse_args(int argc, char **argv, int params_count, const char **param_names, int **param_data, const char **param_descs);

/* Helper utils */
std::string ca_int_to_string(int i);

#endif
