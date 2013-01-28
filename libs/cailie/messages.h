
#ifndef CAILIE_MESSAGES_H
#define CAILIE_MESSAGES_H

#include <pthread.h>
#include <string>

namespace ca {

class Thread;
class Net;

class ThreadMessage {
	public:
		ThreadMessage() : next(NULL) {}
		virtual ~ThreadMessage() {}
		ThreadMessage *next;
		virtual void process(Thread *thread) = 0;
};

class ThreadMessageBarriers  : public ThreadMessage {
	public:
		ThreadMessageBarriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2) : barrier1(barrier1), barrier2(barrier2) {}
		void process(Thread *thread);
	protected:
		pthread_barrier_t *barrier1;
		pthread_barrier_t *barrier2;
};

}

#endif
