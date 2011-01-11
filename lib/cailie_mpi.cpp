
#include "cailie_mpi.h"
#include <stdio.h>

#define HALT_COMMAND 0

CaMpiModule::CaMpiModule()
{
	requests_capacity = 4;
	requests = (MPI_Request *) malloc(sizeof(MPI_Request) * requests_capacity);
	// FIXME: Alloc test
	requests_count = 0;
}

CaMpiModule::~CaMpiModule()
{
	free(requests);
}

int CaMpiModule::main(int nodes_count, InitFn *init_fn)
{
	int myrank; 
	int size;
	MPI_Comm_rank(MPI_COMM_WORLD, &myrank);
	
	if (myrank >= nodes_count) {
		// We don't need this node
		MPI_Finalize();
		return 0;
	}
	MPI_Comm_size(MPI_COMM_WORLD, &size);

	if (size < nodes_count) {
		if (myrank == 0) {
			fprintf(stderr, "There are %i processes, but %i nodes are required.\n", size, nodes_count);
		}
		MPI_Finalize();
		return 1;
	}

	CaContext ctx(myrank, this);
	init_fn(&ctx);
	ctx._get_module()->start_scheduler(&ctx);

	MPI_Finalize();
	return 0;
}

void CaMpiModule::send(CaContext *ctx, int target, int data_id, void *data, size_t size)
{
	if (requests_count == requests_capacity) {
		requests_capacity *= 2;
		requests = (MPI_Request*) realloc(requests, requests_capacity * sizeof(MPI_Request));
		// FIXME: Alloc test
	}
	MPI_Isend(data, size, MPI_CHAR, target, data_id, MPI_COMM_WORLD, &requests[requests_count]);
	requests_count++;
}

void CaMpiModule::check_requests()
{
	while (requests_count > 0) {
		int flag, index;
		MPI_Testany(requests_count, requests, &index, &flag, MPI_STATUS_IGNORE);
		if (flag) {
			--requests_count;
			requests[index] = requests[requests_count];
		} else {
			return;
		}
	}
}

int CaMpiModule::recv(CaContext *ctx, RecvFn *recv_fn, void *places)
{
	check_requests();

	int flag;
	MPI_Status status;
	MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);

	if (!flag) {
		return 0;
	}
	
	for(;;) {

		if (status.MPI_TAG == HALT_COMMAND) {
			ctx->halt();
			break;
		}

		int msg_size;
		MPI_Get_count(&status, MPI_CHAR, &msg_size);

		char buffer[msg_size]; // FIXME: For large packets alloc memory on heap
		
		MPI_Recv(buffer, msg_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

		recv_fn(places, status.MPI_TAG, buffer, msg_size);
		MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
		if (!flag) 
			break;
	}

	return 1;
}


void CaMpiModule::quit(CaContext *ctx)
{
	/* Copied from cailie_threads.cpp, it will be better to use some MPI broadcast API */
	int t;
	int size;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	for (t=0; t < size; t++) {
		if (t == ctx->node()) {
			continue; // Don't send the message to self
		}
		int i;
		send(ctx, t, HALT_COMMAND, &i, sizeof(int));
	}
}
