
#ifndef CAILIE_MESSAGES_H
#define CAILIE_MESSAGES_H

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

#endif
