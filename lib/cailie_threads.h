
#ifndef CAILIE_THREADS_H
#define CAILIE_THREADS_H

#include "cailie.h"
#include "cailie_internal.h"

struct CaThreadsNodeQueue;
struct CaThreadsPacket;

class CaThreadsModule : public CaModule {

	public:
		int main(int nodes_count, MainFn *main_fn);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		int recv(CaContext *ctx, RecvFn *recv, void *places);

	protected:
		CaThreadsNodeQueue *_queues;
		void queue_add(int node, CaThreadsPacket *packet);
};

#endif 
