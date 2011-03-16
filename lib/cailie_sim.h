
#ifndef CAILIE_SIM
#define CAILIE_SIM

#include "cailie_internal.h"
#include <vector>
#include <stdio.h>

class CaSimModule : public CaModule {
    public:
	int main(int nodes_count, InitFn *init_fn);  	
};

class CaSimProcess : public CaProcess {
	public:
		CaSimProcess() : CaProcess(0) {}
		int main(int nodes_count, InitFn *init_fn);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		int recv();
		void quit(CaContext *ctx);

		int run_listener();
		void error(const char *str) { fprintf(comm_out, "%s\n", str); }
		size_t get_reserved_prefix_size() { return 0; }

		void start_logging(CaContext *ctx, const std::string& logname);
		void stop_logging(CaContext *ctx);
	protected:

		void fire_transition(int transition_id, int iid);

		FILE *comm_in;
		FILE *comm_out;
};

#endif // CAILIE_SIM
