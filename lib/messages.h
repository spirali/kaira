
#ifndef CAILIE_MESSAGES_H
#define CAILIE_MESSAGES_H

#include <pthread.h>
#include "unit.h"

class CaThread;

class CaMessage {
	public:
		CaMessage() : next(NULL) {}
		virtual ~CaMessage() {}
		CaMessage *next;
		virtual void process(CaThread *thread) = 0;
};

class CaMessageNewUnit  : public CaMessage {
	public:
		CaMessageNewUnit(CaUnitDef *def, CaUnit *unit) : def(def),unit(unit) {}
		void process(CaThread *thread);
	protected:
		CaUnitDef *def;
		CaUnit *unit;
};

class CaMessageBarriers  : public CaMessage {
	public:
		CaMessageBarriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2) : barrier1(barrier1), barrier2(barrier2) {}
		void process(CaThread *thread);
	protected:
		pthread_barrier_t *barrier1;
		pthread_barrier_t *barrier2;
};



#endif
