
#ifndef CAILIE_MPI_H
#define CAILIE_MPI_H

#include "cailie.h"
#include "cailie_internal.h"
#include <mpi.h>

class CaMpiModule : public CaModule {

	public:
		int main(int nodes_count, InitFn *init_fn);
};

class CaMpiProcess : public CaProcess {

	public:
		CaMpiProcess(int process_id);
		~CaMpiProcess();
		int main(int nodes_count, InitFn *init_fn);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		int recv();
		void quit(CaContext *ctx);
		size_t get_reserved_prefix_size();
	protected:
		void check_requests();
		MPI_Request *_requests;
		void **_requests_data;
		size_t _requests_count;
		size_t _requests_capacity;
		int _nodes_count;
};

#endif // CAILIE_MPI_H
