
#include <stdio.h>
#include "messages.h"
#include "process.h"

#ifdef CA_MPI
#include <mpi.h>
#endif

void CaThreadMessageNewNet::process(CaThread *thread)
{
		thread->add_network(net);
}

void CaThreadMessageHaltNet::process(CaThread *thread)
{
		CaNet *net = thread->remove_net(net_id);
		net->lock();
		net->finalize(thread);
		net->set_finalizer(NULL, NULL);
		int r = net->decr_ref_count();
		net->unlock();
		if (r == 0) {
			delete net;
		}
}

void CaThreadMessageBarriers::process(CaThread *thread)
{
	pthread_barrier_wait(barrier1);
	pthread_barrier_wait(barrier2);
}

void CaThreadMessageLogInit::process(CaThread *thread)
{
	#ifdef CA_MPI
	if (thread->get_id() == 0) {
		MPI_Barrier(MPI_COMM_WORLD);
	}
	#endif // CA_MPI

	pthread_barrier_wait(barrier1);
	thread->init_log(logname);

	#ifdef CA_MPI
	if (thread->get_id() == 0) {
		MPI_Barrier(MPI_COMM_WORLD);
	}
	#endif // CA_MPI

	pthread_barrier_wait(barrier2);

	if (thread->get_id() == 0) {
		pthread_barrier_destroy(barrier1);
		pthread_barrier_destroy(barrier2);
		delete barrier1;
		delete barrier2;
	}
}

void CaThreadMessageLogClose::process(CaThread *thread)
{
	thread->close_log();
}
