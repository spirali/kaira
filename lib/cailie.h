
#ifndef CAILIE_H
#define CAILIE_H

class CaModule;

class CaContext {
	
	public:
		CaContext(int node, CaModule *module);

		/* CaContext user API */
		int node() { return _node; }
		int iid() { return _iid; }
		int instances() { return _instances; }
		int halt() { return _halt_flag = true; }
		
		/* Internal */
		CaModule * _get_module() { return _module; }
		void _init(int iid, int instances);
		int _check_halt_flag() { return _halt_flag; }
	protected:
		int _node;
		int _iid;
		int _instances;
		bool _halt_flag;
		CaModule *_module;
};


/* Callbacks */
typedef int(TransitionFn)(CaContext * ctx, void *places);
typedef void(MainFn)(CaContext * ctx);
typedef void(RecvFn)(void *places,  int data_id, void *data, size_t size);

/* Init functions */
void ca_start(CaContext *ctx, void *data, TransitionFn **tf, RecvFn *recv_fn);
void ca_main(int nodes_count, MainFn *main);

/* Communications */
void ca_send(CaContext *ctx, int node, int data_id, void *data, size_t data_size);

/* Others */
void ca_parse_args(int argc, char **argv, int params_count, const char **param_names, int **param_data, const char **param_descs);

#endif
