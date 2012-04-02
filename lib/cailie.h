
#ifndef CAILIE_H
#define CAILIE_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <stdarg.h>
#include <algorithm>

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

#define CA_DLOG(...)

// Uncommed two following two lines for enabling debug output
//#undef CA_DLOG
//#define CA_DLOG(...) printf(__VA_ARGS__)

class CaContext {
	public:
		CaContext(CaThread *thread, CaNet *net) : thread(thread), net(net), halt_flag(false) {}

		void quit() { thread->quit_all(); }
		void halt() { halt_flag = true; }
		int process_id() { return thread->get_process_id(); }
		int process_count() { return thread->get_process_count(); }
		int threads_count() { return thread->get_threads_count(); }
		/*
		void start_logging(std::string &logname) { thread->start_logging(logname); }
		void stop_logging() { thread->stop_logging(); }
		*/
		bool get_halt_flag() { return halt_flag; }
	protected:
		CaThread *thread;
		CaNet *net;

		/** We need halt_flag because we need pospone thread->quit. We cannot call
			thread->halt directly because other thread could process finalizer
			before current transition put its token into output places */
		bool halt_flag;
};

/* Main functions */
void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_setup(int defs_count, CaNetDef **defs);
void ca_spawn_toplevel_net(int def_id);
int ca_main();
void ca_project_description(const char *str);

/* In SHMEM returns first net of process[0], can be called only between ca_spawn_toplevel_net and ca_main */
CaNet *ca_get_main_net();


template <typename T>
class ca_array
{
	public:
	ca_array() {}
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
	void clear()
	{
		vec.clear();
	}
	private:
	std::vector<T> vec;
};

std::vector<int> ca_range(int from, int upto);

template <typename T> std::vector<T> ca_repeat(int count, std::vector<T> vector)
{
	size_t l = count * vector.size();
	std::vector<T> out(l);
	for (size_t t = 0; t < l; t++) {
		out[t] = vector[t % vector.size()];
	}
	return out;
}

template <typename T> std::vector<T> ca_array_diff(std::vector<T> vector1, std::vector<T> vector2)
{
	std::vector<T> v;
	for (typename std::vector<T>::iterator i = vector1.begin(); i != vector1.end(); i++) {
		if (std::find (vector2.begin(), vector2.end(), *i) == vector2.end()) {
			v.push_back(*i);
		}
	}
	return v;
}

template <typename T> std::vector<T> ca_array_join(std::vector<T> vector1, std::vector<T> vector2)
{
	std::vector<T> v = vector1;
	v.insert(v.end(), vector2.begin(), vector2.end());
	return v;
}

#endif
