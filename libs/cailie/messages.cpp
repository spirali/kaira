
#ifdef CA_MPI
#include <mpi.h>
#endif

#include <stdio.h>
#include "messages.h"
#include "thread.h"


void CaThreadMessageHaltNet::process(CaThread *thread)
{
	CaNet *net = thread->get_process()->get_net();
	if (net == NULL) {
		return;
	}
	net->lock();
	net->finalize(thread);
	net->unlock();
}

void CaThreadMessageBarriers::process(CaThread *thread)
{
	pthread_barrier_wait(barrier1);
	pthread_barrier_wait(barrier2);
}
