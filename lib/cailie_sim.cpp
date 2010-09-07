
#include "cailie_sim.h"
#include <string.h>
#include <assert.h>

#define LINE_LENGTH_LIMIT 4096

int CaSimModule::main(int nodes_count, InitFn *init_fn) 
{
	int t;
	for (t=0; t < nodes_count; t++) {
		CaContext ctx(t, this);
		init_fn(&ctx);
		ctxs.push_back(ctx);
	}
	run_listener();
}

void CaSimModule::send(CaContext *ctx, int target, int data_id, void *data, size_t size) 
{
	RecvFn *f = ctxs[target]._get_recv_fn();
	f(ctxs[target]._get_places(), data_id, data, size); 
}

int CaSimModule::recv(CaContext *ctx, RecvFn *recv, void *places)
{
	/* We dont need this function in simulator, because
		standard transtion scheduler is not started */
}

int CaSimModule::run_listener()
{
	char line[LINE_LENGTH_LIMIT];
	for(;;) {
		char *s = fgets(line, LINE_LENGTH_LIMIT, stdin);
		if (s == NULL) {
			error("Read failed");
			return -1;
		}
		if (!strcmp(line, "QUIT\n")) { 
			return 0; 
		}
		if (!strcmp(line, "REPORTS\n")) {
			CaOutput output;
			std::vector<CaContext>::iterator i;
			output.child("report");
			for (i = ctxs.begin(); i != ctxs.end(); i++) {
				ReportFn *f = (*i)._get_report_fn();
				assert(f != NULL);
				output.child("node");
				f(&(*i), (*i)._get_places(), &output);
				output.back();
			}
			CaOutputBlock *block = output.back();
			block->write(stdout);
			continue;
		}
		error("Unknown command");
	}
}
