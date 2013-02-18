
#ifdef CA_MPI
#include <mpi.h>
#endif

#include <stdio.h>
#include "messages.h"
#include "thread.h"

using namespace ca;

void ThreadMessageBarriers::process(Thread *thread)
{
	pthread_barrier_wait(barrier1);
	pthread_barrier_wait(barrier2);
}
