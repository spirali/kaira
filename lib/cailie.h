
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <stdarg.h>

#include "place.h"
#include "process.h"
#include "net.h"

/*
#ifdef CA_LOG

#define CA_LOG_TRANSITION_START(thread, unit, transition_id) ((thread)->log_transition_start(unit, transition_id))
#define CA_LOG_TRANSITION_END(thread, unit, transition_id) ((thread)->log_transition_end(unit, transition_id))
#define CA_LOG_TOKEN_ADD(thread, unit, place_id, token_string) ((thread)->log_token_add(unit, place_id, token_string))
#define CA_LOG_TOKEN_ADD_MORE(thread, unit, place_id, array, type, expr) { for (std::vector<type>::iterator __i = (array).begin(); __i != (array).end(); ++__i) { ((thread)->log_token_add(unit, place_id, expr)); } }
#define CA_LOG_TOKEN_REMOVE(thread, unit, place_id, token_string) ((thread)->log_token_remove(unit, place_id, token_string))
#define CA_LOG_UNIT_STATUS(thread, unit, def_id) ((thread)->log_unit_status(unit, def_id))

#define CA_LOG_ITEM (*__i)

#else

#define CA_LOG_TRANSITION_START(thread, unit, transition_id)
#define CA_LOG_TRANSITION_END(thread, unit, transition_id)
#define CA_LOG_TOKEN_ADD(thread, unit, place_id, token_string)
#define CA_LOG_TOKEN_ADD_MORE(thread, unit, place_id, array, type, expr)
#define CA_LOG_TOKEN_REMOVE(thread, unit, place_id, token_string)
#define CA_LOG_UNIT_STATUS(thread, unit, def_id)

#endif // CA_LOG
*/

class CaContext {
	public:
		CaContext(CaThread *thread) : thread(thread) {}

		void quit() { thread->quit_all(); }
		int process_id() { return thread->get_process()->get_process_id(); }
		/*
		void start_logging(std::string &logname) { thread->start_logging(logname); }
		void stop_logging() { thread->stop_logging(); }
		*/
	protected:
		CaThread *thread;
};

/* Start */
int ca_main(int defs_count, CaNetDef **defs);

/* Others */
void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_project_description(const char *str);


template <typename T>
class ca_array
{
	public:
	ca_array(const T& val) {
		vec.push_back(val);
	}

	ca_array& operator()(T val)
	{
		vec.push_back(val);
		return *this;
	}

	std::vector<T> end()
	{
		return vec;
	}
	private:
	std::vector<T> vec;
};

std::vector<int> ca_range(int from, int upto);

#endif
