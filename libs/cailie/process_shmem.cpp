
#include "cailie.h"

using namespace ca;

namespace ca {
extern Process **processes;
}

int ca::Process::collective_transition_id = 0;
int ca::Process::collective_root = -1;

pthread_mutex_t ca::Process::collective_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_barrier_t ca::Process::collective_barrier1;
pthread_barrier_t ca::Process::collective_barrier2;

void ca::Process::broadcast_packet(int tag, void *data, size_t size, int exclude)
{
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		void *d = malloc(size);
		memcpy(d, data, size);
		processes[t]->add_packet(process_id, tag, d, size);
	}
	free(data);
}

void ca::Process::add_packet(int from_process, int tag, void *data, size_t size)
{
	ShmemPacket *packet = new ShmemPacket;
	packet->from_process = from_process;
	packet->tag = tag;
	packet->data = data;
	packet->size = size;
	packet->next = NULL;
	pthread_mutex_lock(&packet_mutex);
	if (packets == NULL) {
		packets = packet;
	} else {
		ShmemPacket *p = packets;
		while (p->next) {
			p = p->next;
		}
		p->next = packet;
	}
	pthread_mutex_unlock(&packet_mutex);
}

void Process::send(int target, Net *net, int edge_id, int tokens_count, Packer &packer, Thread *thread)
{
	std::vector<int> a(1);
	a[0] = target;
	send_multicast(a, net, edge_id, tokens_count, packer, thread);
}

void ca::Process::send_multicast(
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
	std::vector<int>::const_iterator i;
	Tokens *data = (Tokens*) packer.get_buffer();
	data->edge_id = edge_id;
	data->tokens_count = tokens_count;
	size_t size = packer.get_size();
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i;
		if(target < 0 || target >= process_count) {
			fprintf(stderr,
					"Process %i sends %i token(s) to invalid process id %i (valid ids: [0 .. %i])\n",
					thread->get_process_id(), tokens_count, target, process_count-1);
			exit(1);
		}
		CA_DLOG("SEND index=%i target=%i process=%i\n", edge_id, target, get_process_id());
		void *d;
		if ((i + 1) == targets.end()) {
			d = data;
		} else {
			d = malloc(size);
			memcpy(d, data, size);
		}
		Process *p = processes[target];
		p->add_packet(process_id, CA_TAG_TOKENS, d, size);
	}
}

int ca::Process::process_packets(Thread *thread)
{
	if (packets) {
		pthread_mutex_lock(&packet_mutex);
		ShmemPacket *p = packets;
		packets = NULL;
		pthread_mutex_unlock(&packet_mutex);

		/* Now we have to be sure that all thread messages
           are processed and we know about all nets */
		thread->process_thread_messages();
		bool net_changed = false;
		while (p) {
			net_changed |= process_packet(thread, p->from_process, p->tag, p->data);
			ShmemPacket *next = p->next;
			delete p;
			p = next;
		}
		TraceLog *tracelog = thread->get_tracelog();
		if (net_changed && tracelog) {
			tracelog->event_end();
		}
		return 1;
	}
	return 0;
}

void ca::Process::init_collective_operations(int process_count)
{
	collective_transition_id = 0;
	pthread_barrier_init(&collective_barrier1, NULL, process_count);
	pthread_barrier_init(&collective_barrier2, NULL, process_count);
}


void ca::Process::setup_collective_operation(int transition_id, bool use_root, int root)
{
	if (use_root && (root < 0 || root >= get_process_count())) {
		fprintf(stderr, "Invalid value of root (root=%i)\n", root);
		exit(1);
	}

	pthread_mutex_lock(&collective_mutex);
	if (collective_transition_id == transition_id) {
		// Someone already setuped collective operation
		if (use_root && collective_root != root) {
			fprintf(stderr, "Two collective transition was fired with different roots\n");
			fprintf(stderr, "Process %i sets root %i, but collective communication"
					" was already executed with root %i\n",
					process_id, root, collective_root);
			exit(1);
		}
	} else if (collective_transition_id == 0) {
		// We are the first
		collective_transition_id = transition_id;
		collective_root = root;
	} else {
		fprintf(stderr, "Two different collective transition was fired in the same time\n");
		exit(1);
	}


	pthread_mutex_unlock(&collective_mutex);
	pthread_barrier_wait(&collective_barrier1);

	if (process_id == 0) {
		collective_transition_id = 0;
		root = -1;
	}
}

// Scatter -------------------------------------------------------

void ca::Process::collective_scatter_root(int transition_id, const void *data, size_t size) {
	collective_data = data;
	setup_collective_operation(transition_id, true, process_id);
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_scatter_nonroot(int transition_id, int root, void *out, size_t size) {
	setup_collective_operation(transition_id, true, root);
	const char *data = &static_cast<const char*>(processes[root]->collective_data)[size * process_id];
	memcpy(out, data, size);
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_scatterv_root(int transition_id, const void *data, int *sizes, int *displs) {
	collective_data = data;
	collective_displs = displs;
	setup_collective_operation(transition_id, true, process_id);
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_scatterv_nonroot(int transition_id, int root, void *out, size_t size) {
	setup_collective_operation(transition_id, true, root);
	int displ = processes[root]->collective_displs[process_id];
	const char *data = &static_cast<const char*>(processes[root]->collective_data)[displ];
	memcpy(out, data, size);
	pthread_barrier_wait(&collective_barrier2);
}

// Gather -------------------------------------------------------

void ca::Process::collective_gather_root(int transition_id, const void *data, size_t size, void *out) {
	collective_data = data;
	setup_collective_operation(transition_id, true, get_process_id());
	char *dest = static_cast<char*>(out);
	for (int i = 0; i < process_count; i++) {
		memcpy(&dest[i * size], processes[i]->collective_data, size);
	}
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_gather_nonroot(int transition_id, int root, const void *data, size_t size) {
	collective_data = data;
	setup_collective_operation(transition_id, true, root);
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_gatherv_root(
		int transition_id, const void *data, int size, void *out, int *sizes, int *displs) {
	collective_data = data;
	setup_collective_operation(transition_id, true, get_process_id());
	char *dest = static_cast<char*>(out);
	for (int i = 0; i < process_count; i++) {
		memcpy(&dest[displs[i]], processes[i]->collective_data, sizes[i]);
	}
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_gatherv_nonroot(
		int transition_id, int root, const void *data, int size) {
	collective_gather_nonroot(transition_id, root, data, size);
}

// Bcast ----------------------------------------------------------

void ca::Process::collective_bcast_root(int transition_id, const void *data, size_t size) {
	collective_data = data;
	setup_collective_operation(transition_id, true, process_id);
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_bcast_nonroot(int transition_id, int root, void *out, size_t size) {
	setup_collective_operation(transition_id, true, root);
	memcpy(out, processes[root]->collective_data, size);
	pthread_barrier_wait(&collective_barrier2);
}

// Barrier --------------------------------------------------------

void ca::Process::collective_barrier(int transition_id) {
	setup_collective_operation(transition_id, false, 0);
	pthread_barrier_wait(&collective_barrier2);
}

// Allgather -------------------------------------------------------

void ca::Process::collective_allgather(int transition_id, const void *data, size_t size, void *out) {
	collective_data = data;
	setup_collective_operation(transition_id, false, -1);
	char *dest = static_cast<char*>(out);
	for (int i = 0; i < process_count; i++) {
		memcpy(&dest[i * size], processes[i]->collective_data, size);
	}
	pthread_barrier_wait(&collective_barrier2);
}

void ca::Process::collective_allgatherv(
		int transition_id, const void *data, int size, void *out, int *sizes, int *displs) {
	collective_data = data;
	setup_collective_operation(transition_id, false, -1);
	char *dest = static_cast<char*>(out);
	for (int i = 0; i < process_count; i++) {
		memcpy(&dest[displs[i]], processes[i]->collective_data, sizes[i]);
	}
	pthread_barrier_wait(&collective_barrier2);
}
