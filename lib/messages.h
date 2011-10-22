
#ifndef CAILIE_MESSAGES_H
#define CAILIE_MESSAGES_H

#include <pthread.h>
#include <string>

class CaThread;
class CaNet;

class CaMessage {
	public:
		CaMessage() : next(NULL) {}
		virtual ~CaMessage() {}
		CaMessage *next;
		virtual void process(CaThread *thread) = 0;
};

class CaMessageNewNet  : public CaMessage {
	public:
		CaMessageNewNet(CaNet *net) : net(net) {}
		void process(CaThread *thread);
	protected:
		CaNet *net;
};

class CaMessageBarriers  : public CaMessage {
	public:
		CaMessageBarriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2) : barrier1(barrier1), barrier2(barrier2) {}
		void process(CaThread *thread);
	protected:
		pthread_barrier_t *barrier1;
		pthread_barrier_t *barrier2;
};

class CaMessageLogInit  : public CaMessage {
	public:
		CaMessageLogInit(std::string logname, pthread_barrier_t *barrier1, pthread_barrier_t *barrier2) 
			: barrier1(barrier1), barrier2(barrier2), logname(logname) {}
		void process(CaThread *thread);
	protected:
		pthread_barrier_t *barrier1;
		pthread_barrier_t *barrier2;
		std::string logname;
};

class CaMessageLogClose : public CaMessage {
	public:
		CaMessageLogClose() : CaMessage() {}
		void process(CaThread *thread);
};


#endif
