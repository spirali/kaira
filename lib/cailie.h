
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

#ifdef CA_LOG_ON

#define CA_LOG_TRANSITION_START(thread, unit, transition_id) ((ctx)->log_transition_start(transition_id))
#define CA_LOG_TRANSITION_END(thread, unit, transition_id) ((ctx)->log_transition_end(transition_id))
#define CA_LOG_TOKEN_ADD(thread, unit, place_id, token_string) ((ctx)->log_token_add(place_id, token_string))
#define CA_LOG_TOKEN_REMOVE(thread, unit, place_id, token_string) ((ctx)->log_token_remove(place_id, token_string))
#define CA_LOG_TOKEN_CLEAN(thread, unit, place_id) ((ctx)->log_token_clean(place_id, token_string))

#else

#define CA_LOG_TRANSITION_START(thread, transition_id)
#define CA_LOG_TRANSITION_END(thread, transition_id)
#define CA_LOG_TOKEN_ADD(thread, place_id, token_string)
#define CA_LOG_TOKEN_REMOVE(thread, place_id, token_string)
#define CA_LOG_TOKEN_CLEAN(thread, place_id)

#endif // CA_LOG

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
int ca_main(int defs_count, CaUnitDef **defs);

/* Others */
void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_project_description(const char *str);

#endif
