
#include "cailie.h"
#include "utils.h"
#include <mpi.h>

using namespace ca;

/* This code is just proof of concept and it really needs some tweaks */

void Process::broadcast_packet(int tag, void *data, size_t size, int exclude)
{
	thread->get_requests()->check();
	char *d = (char*) data;
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		MPI_Request *request = thread->get_requests()->new_request(d);
		d = NULL;
		MPI_Isend(data, size, MPI_BYTE, t, tag, MPI_COMM_WORLD, request);
	}
}

void Process::send(int target, Net *net, int edge_id, int tokens_count, Packer &packer, Thread *thread)
{
	thread->get_requests()->check();
	char *buffer = packer.get_buffer();
	size_t size = packer.get_size();
	Tokens *data = (Tokens*) packer.get_buffer();
	data->edge_id = edge_id;
	data->tokens_count = tokens_count;
	if(target < 0 || target >= process_count) {
		fprintf(stderr, "Net sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
			tokens_count, target, process_count - 1);
		exit(1);
	}
	CA_DLOG("SEND index=%i target=%i process=%i\n", place_index, target, get_process_id());

	MPI_Request *request = thread->get_requests()->new_request(buffer);
	MPI_Isend(buffer, size, MPI_BYTE, target, CA_TAG_TOKENS, MPI_COMM_WORLD, request);
}

void Process::send_multicast(
	const std::vector<int> &targets,
	Net *net,
	int edge_id,
	int tokens_count,
	Packer &packer,
	Thread *thread)
{
	if (targets.size() == 0) {
		packer.free();
		return;
	}

	thread->get_requests()->check();
	char *buffer = packer.get_buffer();
	size_t size = packer.get_size();
	std::vector<int>::const_iterator i;
	Tokens *data = (Tokens*) packer.get_buffer();
	data->edge_id = edge_id;
	data->tokens_count = tokens_count;
	char *d = buffer;
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i;
		if(target < 0 || target >= process_count) {
			fprintf(stderr, "Net sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
				tokens_count, target, process_count - 1);
			exit(1);
		}
		CA_DLOG("SEND index=%i target=%i process=%i\n", place_index, target, get_process_id());

		MPI_Request *request = thread->get_requests()->new_request(d);
		d = NULL;
		MPI_Isend(buffer, size, MPI_BYTE, target, CA_TAG_TOKENS, MPI_COMM_WORLD, request);
	}
}

int Process::process_packets(Thread *thread)
{
	int flag;
	MPI_Status status;
	MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);

	if (flag) {
		bool net_changed = false;
		for(;;) {
			int msg_size;
			MPI_Get_count(&status, MPI_BYTE, &msg_size);
			char *buffer = (char*) malloc(msg_size); // FIXME: alloca for small packets
			MPI_Recv(buffer, msg_size, MPI_BYTE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			/* Now we have to be sure that all thread messages
			   are processed and we know about all nets */
			thread->process_thread_messages();
			net_changed |= process_packet(thread, status.MPI_SOURCE, status.MPI_TAG, buffer);

			MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
			if (!flag)
				break;
		}
		TraceLog *tracelog = thread->get_tracelog();
		if (net_changed && tracelog) {
			tracelog->event_end();
		}
		return 1;
	}
	return 0;
}

void Process::wait()
{
	MPI_Status status;
	MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

	int msg_size;
	MPI_Get_count(&status, MPI_BYTE, &msg_size);
	char *buffer = (char*) malloc(msg_size); // FIXME: alloca for small packets
	MPI_Recv(buffer, msg_size, MPI_BYTE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	this->get_thread()->process_thread_messages();
	process_packet(this->get_thread(), status.MPI_SOURCE, status.MPI_TAG, buffer);
}

// Scatter ------------------------------------------------------

void ca::Process::collective_scatter_root(int transition_id, const void *data, size_t size) {
	// MPI_Scatter should take const void * as first argument, but it takes just void * so we have to cast
	MPI_Scatter(const_cast<void*>(data), size, MPI_BYTE, MPI_IN_PLACE, size, MPI_BYTE, process_id, MPI_COMM_WORLD);
}

void ca::Process::collective_scatter_nonroot(int transition_id, int root, void *out, size_t size) {
	MPI_Scatter(NULL, size, MPI_BYTE, out, size, MPI_BYTE, root, MPI_COMM_WORLD);
}

void ca::Process::collective_scatterv_root(int transition_id, const void *data, int *sizes, int *displs) {
	// MPI_Scatter should take const void * as first argument, but it takes just void * so we have to cast
	MPI_Scatterv(const_cast<void*>(data), sizes, displs, MPI_BYTE,
			MPI_IN_PLACE, 0, MPI_BYTE, process_id, MPI_COMM_WORLD);
}

void ca::Process::collective_scatterv_nonroot(int transition_id, int root, void *out, size_t size) {
	MPI_Scatterv(NULL, NULL, NULL, MPI_BYTE, out, size, MPI_BYTE, root, MPI_COMM_WORLD);
}


// Gather -------------------------------------------------------
// TODO: Implement MPI_IN_PLACE for gather operations

void ca::Process::collective_gather_root(int transition_id, const void *data, size_t size, void *out) {
	MPI_Gather(const_cast<void*>(data), size, MPI_BYTE, out, size, MPI_BYTE, process_id, MPI_COMM_WORLD);
}

void ca::Process::collective_gather_nonroot(int transition_id, int root, const void *data, size_t size) {
	// MPI_Gather should take const void * as first argument, but it takes just void * so we have to cast
	MPI_Gather(const_cast<void*>(data), size, MPI_BYTE, NULL, size, MPI_BYTE, root, MPI_COMM_WORLD);
}

void ca::Process::collective_gatherv_root(
		int transition_id, const void *data, int size, void *out, int *sizes, int *displs) {
	MPI_Gatherv(const_cast<void*>(data), size, MPI_BYTE,
		out, sizes, displs, MPI_BYTE, process_id, MPI_COMM_WORLD);
}

void ca::Process::collective_gatherv_nonroot(
		int transition_id, int root, const void *data, int size) {
	MPI_Gatherv(const_cast<void*>(data), size, MPI_BYTE,
		NULL, NULL, NULL, MPI_BYTE, root, MPI_COMM_WORLD);
}

// Bcast ----------------------------------------------------------

void ca::Process::collective_bcast_root(int transition_id, const void *data, size_t size) {
	MPI_Bcast(const_cast<void*>(data), size, MPI_BYTE, process_id, MPI_COMM_WORLD);
}

void ca::Process::collective_bcast_nonroot(int transition_id, int root, void *out, size_t size) {
	MPI_Bcast(out, size, MPI_BYTE, root, MPI_COMM_WORLD);
}

// Barrier --------------------------------------------------------

void ca::Process::collective_barrier(int transition_id) {
	MPI_Barrier(MPI_COMM_WORLD);
}

// Allgather ------------------------------------------------------

void ca::Process::collective_allgather(int transition_id, const void *data, size_t size, void *out) {
	MPI_Allgather(const_cast<void*>(data), size, MPI_BYTE, out, size, MPI_BYTE, MPI_COMM_WORLD);
}

void ca::Process::collective_allgatherv(
		int transition_id, const void *data, int size, void *out, int *sizes, int *displs) {
	MPI_Allgatherv(const_cast<void*>(data), size, MPI_BYTE,
		out, sizes, displs, MPI_BYTE, MPI_COMM_WORLD);
}
