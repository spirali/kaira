
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

		int run_listener();
		void error(const char *str) { printf("%s\n", str); }

	protected:
		std::vector<CaContext> ctxs;
};

#endif // CAILIE_SIM
