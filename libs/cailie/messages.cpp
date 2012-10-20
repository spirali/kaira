
#ifdef CA_MPI
#include <mpi.h>
#endif

#include <stdio.h>
#include "messages.h"
#include "thread.h"


void CaThreadMessageBarriers::process(CaThread *thread)
{
	pthread_barrier_wait(barrier1);
	pthread_barrier_wait(barrier2);
}
