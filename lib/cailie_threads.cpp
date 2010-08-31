#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sched.h>

#include "cailie.h"
#include "cailie_internal.h"
#include "cailie_threads.h"

struct CaThreadsPacket {
	int data_id;
	size_t size;
	CaThreadsPacket *next;
};

struct CaThreadsNodeQueue {
	pthread_mutex_t lock;
	CaThreadsPacket *packet;
};

struct CaThreadsData {
	int node;
	MainFn *main_fn;
	CaModule *module;
};

void CaThreadsModule::queue_add(int node, CaThreadsPacket *packet)
{
	CaThreadsNodeQueue *queue = &_queues[node];
	pthread_mutex_lock(&queue->lock);
	packet->next = queue->packet;
	queue->packet = packet;
	pthread_mutex_unlock(&queue->lock);
}

void CaThreadsModule::send(CaContext *ctx, int target, int data_id, void *data, size_t size)
{
	CaThreadsPacket *packet = (CaThreadsPacket *) malloc(sizeof(CaThreadsPacket) + size);
	// FIXME: Alloc test
	packet->data_id = data_id;
	packet->size = size;
	memcpy(packet + 1, data, size);
	queue_add(target, packet);
}

static void ca_threads_start_main(MainFn *main_fn, int node, CaModule *module)
{
	CaContext ctx(node, module);
	main_fn(&ctx);
}

static void * ca_threads_starter(void *data)
{
	CaThreadsData *d = (CaThreadsData*) data;
	ca_threads_start_main(d->main_fn, d->node, d->module);
	return NULL;
}

int CaThreadsModule::recv(CaContext *ctx, RecvFn *recv_fn, void *places) 
{
	CaThreadsNodeQueue *queue = &_queues[ctx->node()];
	CaThreadsPacket *packet;
	pthread_mutex_lock(&queue->lock);
	packet = queue->packet;
	queue->packet = NULL;
	pthread_mutex_unlock(&queue->lock);

	if (packet == NULL) {
		return 0;
	}

	while(packet) {
		recv_fn(places, packet->data_id, packet + 1, packet->size);

		CaThreadsPacket *p = packet->next;
		free(packet);
		packet = p;
	}
	return 1;
}

int CaThreadsModule::main(int nodes, MainFn *main_fn)
{
	assert(nodes > 0);
	int t;
	pthread_t threads[nodes];
	CaThreadsData data[nodes];

	CaThreadsNodeQueue queues[nodes];
	this->_queues = queues;

	for (t = 0; t < nodes; t++) {
		pthread_mutex_init(&queues[t].lock, NULL);
		queues[t].packet = NULL;
	}

	for (t = 0; t < nodes; t++) {
		data[t].node = t;
		data[t].main_fn = main_fn;
		data[t].module = this;

		/* FIXME: Check returns code */
		pthread_create(&threads[t], NULL, ca_threads_starter, &data[t]);
	}

	for (t = 0; t < nodes; t++) {
		pthread_join(threads[t], NULL);
	}

	for (t = 0; t < nodes; t++) {
		pthread_mutex_destroy(&queues[t].lock);
	}

	// TODO: destroy nonempty places
	return 0;
}

void CaThreadsModule::idle() {
	sched_yield();
}
