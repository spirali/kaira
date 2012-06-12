
#include "cailie.h"
#include <mpi.h>

/* This code is just proof of concept and it really needs some tweaks */

void CaProcess::broadcast_packet(int tag, void *data, size_t size, CaThread *thread, int exclude)
{
	thread->get_requests()->check();
	char *d = (char*) malloc(size);
	memcpy(d, data, size);
	int count = 0;
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		MPI_Request *request = thread->get_requests()->new_request(d, count++);
		MPI_Isend(d, size, MPI_CHAR, t, tag, MPI_COMM_WORLD, request);
	}
}

void CaProcess::multisend_multicast(const std::vector<int> &targets, CaNet *net, int place_index, int tokens_count, const CaPacker &packer, CaThread *thread)
{
	thread->get_requests()->check();
	char *buffer = packer.get_buffer();
	size_t size = packer.get_size();
	std::vector<int>::const_iterator i;
	CaTokens *data = (CaTokens*) packer.get_buffer();
	data->place_index = place_index;
	data->net_id = net->get_id();
	data->tokens_count = tokens_count;
	char *d = (char*) malloc(packer.get_size());
	memcpy(d, data, packer.get_size());
	int count = 0;
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i;
		if(target < 0 || target >= process_count) {
			fprintf(stderr, "Net %i sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
				net->get_id(), tokens_count, target, process_count - 1);
			exit(1);
		}
		CA_DLOG("SEND index=%i target=%i process=%i\n", place_index, target, get_process_id());

		MPI_Request *request = thread->get_requests()->new_request(d, count++);
		MPI_Isend(d, size, MPI_CHAR, target, CA_TAG_TOKENS, MPI_COMM_WORLD, request);
	}
	free(buffer);
}

int CaProcess::process_packets(CaThread *thread)
{
	int flag;
	MPI_Status status;
	MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);

	if (flag) {
		for(;;) {
			int msg_size;
			MPI_Get_count(&status, MPI_CHAR, &msg_size);
			char *buffer = (char*) malloc(msg_size); // FIXME: alloca for small packets
			MPI_Recv(buffer, msg_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
			/* Now we have to be sure that all thread messages
			   are processed and we know about all nets */
			thread->process_thread_messages();
			process_packet(thread, status.MPI_TAG, buffer);

			MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
			if (!flag)
				break;
		}
		return 1;
	}
	return 0;
}

void CaProcess::wait()
{
	MPI_Status status;
	MPI_Probe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);

	int msg_size;
	MPI_Get_count(&status, MPI_CHAR, &msg_size);
	char *buffer = (char*) malloc(msg_size); // FIXME: alloca for small packets
	MPI_Recv(buffer, msg_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);
	this->get_thread(0)->process_thread_messages();
	process_packet(this->get_thread(0), status.MPI_TAG, buffer);
}
