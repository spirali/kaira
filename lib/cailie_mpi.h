
#ifndef CAILIE_MPI_H
#define CAILIE_MPI_H

#include "cailie.h"
#include "cailie_internal.h"
#include <mpi.h>

class CaMpiModule : public CaModule {

	public:
		CaMpiModule();
		~CaMpiModule();
		int main(int nodes_count, InitFn *init_fn);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		int recv(CaContext *ctx, RecvFn *recv, void *places);
		void quit(CaContext *ctx);

	protected:
		void check_requests();
		MPI_Request *requests;
		size_t requests_count;
		size_t requests_capacity;
};

#endif // CAILIE_MPI_H
