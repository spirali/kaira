#ifndef CAILIE_INTERNAL_H
#define CAILIE_INTERNAL_H

#include "cailie.h"

class CaModule {
	public:
		virtual ~CaModule() {};
		virtual int main(int nodes_count, MainFn *main_fn) = 0;
		virtual void send(CaContext *ctx, int target, int data_id, void *data, size_t size) = 0;
		virtual int recv(CaContext *ctx, RecvFn *recv, void *places) = 0;
};

void ca_init_context(CaContext *ctx, int node);

#endif
