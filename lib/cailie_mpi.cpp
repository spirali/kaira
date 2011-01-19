
#include "cailie_mpi.h"
#include <stdio.h>

#define HALT_COMMAND 0

struct CaMpiMessage {
	int target_node;
};

int CaMpiModule::main(int nodes_count, InitFn *init_fn)
{
	int myrank;
	MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

	if (ca_process_count != 0) {
		if (myrank == 0) {
			fprintf(stderr, "Count of processes specified by --process parameter. Use mpirun -n for this.\n");
			return 0;
		}
	}

	MPI_Comm_size(MPI_COMM_WORLD, &ca_process_count);
	CaMpiProcess process(myrank);
	return process.main(nodes_count, init_fn);
}

size_t CaMpiProcess::get_reserved_prefix_size() {
	return sizeof(CaMpiMessage);
}

CaMpiProcess::CaMpiProcess(int process_id) : CaProcess(process_id)
{
	_requests_capacity = 4;
	_requests = (MPI_Request *) malloc(sizeof(MPI_Request) * _requests_capacity);
	_requests_data = (void**) malloc(sizeof(void*) * _requests_capacity);
	// FIXME: Alloc test
	_requests_count = 0;
}

CaMpiProcess::~CaMpiProcess()
{
	free(_requests);
}

int CaMpiProcess::main(int nodes_count, InitFn *init_fn)
{
	_nodes_count = nodes_count;
	int t;
	for (t = 0; t < nodes_count; t++) {
		if (ca_node_to_process(t) == _process_id) {
			CaContext *ctx = new CaContext(t, this);
			init_fn(ctx);
			_contexts[t] = ctx;
		}
	}

	_running_nodes = _contexts.size();
	start_scheduler();

	CaContextsMap::iterator i;
	for (i = _contexts.begin(); i != _contexts.end(); i++) {
		delete i->second;
	}
	MPI_Finalize();
	return 0;
}

void CaMpiProcess::send(CaContext *ctx, int target, int data_id, void *data, size_t size)
{
	if (_requests_count == _requests_capacity) {
		_requests_capacity *= 2;
		_requests = (MPI_Request *) realloc(_requests, _requests_capacity * sizeof(MPI_Request));
		_requests_data = (void**) realloc(_requests_data, _requests_capacity * sizeof(void*));
		// FIXME: Alloc test
	}
	CaMpiMessage *message = (CaMpiMessage*) data;
	message->target_node = target;
	_requests_data[_requests_count] = data;
	MPI_Isend(message, size + get_reserved_prefix_size(), MPI_CHAR, ca_node_to_process(target), data_id, MPI_COMM_WORLD, &_requests[_requests_count]);
	_requests_count++;
	//free(data);

}

void CaMpiProcess::check_requests()
{
	while (_requests_count > 0) {
		int flag, index;
		MPI_Testany(_requests_count, _requests, &index, &flag, MPI_STATUS_IGNORE);
		if (flag) {
			--_requests_count;
			free(_requests_data[index]);
			_requests_data[index] = _requests_data[_requests_count];
			_requests[index] = _requests[_requests_count];
		} else {
			return;
		}
	}
}

int CaMpiProcess::recv()
{
	check_requests();

	int flag;
	MPI_Status status;
	MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);

	if (!flag) {
		return 0;
	}

	for(;;) {

		int msg_size;
		MPI_Get_count(&status, MPI_CHAR, &msg_size);

		char buffer[msg_size]; // FIXME: For large packets alloc memory on heap
		MPI_Recv(buffer, msg_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
		CaMpiMessage *message = (CaMpiMessage*) buffer;
		CaContext *ctx = _contexts[message->target_node];
		if (status.MPI_TAG == HALT_COMMAND) {
			ctx->halt();
		} else {
			ctx->_call_recv_fn(status.MPI_TAG, message + 1, msg_size - get_reserved_prefix_size());
		}
		MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
		if (!flag)
			break;
	}

	return 1;
}


void CaMpiProcess::quit(CaContext *ctx)
{
	/* Copied from cailie_threads.cpp, it will be better to use some MPI broadcast API */
	int t;
	for (t=0; t < _nodes_count; t++) {
		if (t == ctx->node()) {
			continue; // Don't send the message to self
		}
		void *data = malloc(get_reserved_prefix_size());
		send(ctx, t, HALT_COMMAND, data, 0);
	}
}
