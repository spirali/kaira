
#include <stdio.h>
#include "messages.h"
#include "thread.h"

#ifdef CA_MPI
#include <mpi.h>
#endif

void CaThreadMessageNewNet::process(CaThread *thread)
{
		thread->set_net(net);
}

void CaThreadMessageHaltNet::process(CaThread *thread)
{
		CaNet *net = thread->remove_net();
		if (net == NULL) {
			return;
		}
		net->lock();
		net->finalize(thread);
		int r = net->decr_ref_count();
		net->unlock();
		if (r == 0 && !net->get_manual_delete()) {
			delete net;
		}
}

void CaThreadMessageBarriers::process(CaThread *thread)
{
	pthread_barrier_wait(barrier1);
	pthread_barrier_wait(barrier2);
}
