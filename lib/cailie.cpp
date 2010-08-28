
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cailie.h"
#include "cailie_threads.h"
#include "cailie_internal.h"

CaContext::CaContext(int node, CaModule *module) 
{
	_node = node;
	_module = module;
	_halt_flag = false;
}

void CaContext::_init(int iid, int instances) 
{
	_iid = iid;
	_instances = instances;
}

static int ca_recv(CaContext *ctx, RecvFn *recv_fn, void *data)
{
	return ctx->_get_module()->recv(ctx, recv_fn, data);
}

void ca_start(CaContext *ctx, void *data, TransitionFn **wtransitions, RecvFn *recv_fn) {
	TransitionFn **wt = wtransitions + 1;
	TransitionFn **last_executed = wtransitions;
	for(;;) {
		if ((*wt) == NULL) {
			wt = wtransitions;
		}
		if (wt == last_executed) {
			if ((*wt)(ctx, data)) {
				if (ctx->_check_halt_flag()) {
					return;
				}
				ca_recv(ctx, recv_fn, data);
			} else {
				while(!ca_recv(ctx, recv_fn, data)) {}	
			}
		} else {
			if ((*wt)(ctx, data)) {
				ca_recv(ctx, recv_fn, data);
				last_executed = wt;
			}
		}
		wt++;
	}
}

void ca_main(int nodes_count, MainFn *main_fn) {
	CaModule *m = new CaThreadsModule();
	m->main(nodes_count, main_fn);
}

void ca_send(CaContext *ctx, int node, int data_id, void *data, size_t data_size)
{
	ctx->_get_module()->send(ctx, node, data_id, data, data_size);
}
