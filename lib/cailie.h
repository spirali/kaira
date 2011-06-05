
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>

#include "place.h"
#include "unit.h"
#include "process.h"
#include "output.h"
#include "packing.h"

class CaContext {
	public:
		CaContext(CaThread *thread, CaUnit *unit) : thread(thread), unit(unit) {}

		void quit() { thread->quit_all(); }
		int iid() { return unit->path.last_component(); }
		int process_id() { return thread->get_process()->get_process_id(); }
		const CaPath & path() { return unit->path; }

	protected:
		CaThread *thread;
		CaUnit *unit;
};

/* Start */
void ca_main(int defs_count, CaUnitDef **defs);

/* Others */
void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_project_description(const char *str);

#endif
