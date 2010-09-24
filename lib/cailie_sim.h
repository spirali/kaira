
#ifndef CAILIE_SIM
#define CAILIE_SIM

#include "cailie_internal.h"
#include <vector>
#include <stdio.h>

class CaSimModule : public CaModule {
	public:
		int main(int nodes_count, InitFn *init_fn);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		int recv(CaContext *ctx, RecvFn *recv, void *places);
		void quit(CaContext *ctx);

		int run_listener();
		void error(const char *str) { fprintf(comm_out, "%s\n", str); }

	protected:

		void fire_transition(int transition_id, int iid);

		std::vector<CaContext> ctxs;
		FILE *comm_in;
		FILE *comm_out;
};

#endif // CAILIE_SIM
