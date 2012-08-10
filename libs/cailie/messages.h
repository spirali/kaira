
#ifndef CAILIE_MESSAGES_H
#define CAILIE_MESSAGES_H

#include <pthread.h>
#include <string>

class CaThread;
class CaNet;

class CaThreadMessage {
	public:
		CaThreadMessage() : next(NULL) {}
		virtual ~CaThreadMessage() {}
		CaThreadMessage *next;
		virtual void process(CaThread *thread) = 0;
};

class CaThreadMessageHaltNet  : public CaThreadMessage {
	public:
		CaThreadMessageHaltNet() {};
		void process(CaThread *thread);
};

class CaThreadMessageNewNet  : public CaThreadMessage {
	public:
		CaThreadMessageNewNet(CaNet *net) : net(net) {}
		void process(CaThread *thread);
	protected:
		CaNet *net;
};

class CaThreadMessageBarriers  : public CaThreadMessage {
	public:
		CaThreadMessageBarriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2) : barrier1(barrier1), barrier2(barrier2) {}
		void process(CaThread *thread);
	protected:
		pthread_barrier_t *barrier1;
		pthread_barrier_t *barrier2;
};

#endif
