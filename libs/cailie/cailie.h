
#ifndef CAILIE_H
#define CAILIE_H

#ifdef CA_MPI
#include <mpi.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <string>
#include <stdarg.h>
#include <algorithm>

#include "place.h"
#include "thread.h"
#include "net.h"

#define CA_DLOG(...)

// Uncommed two following two lines for enabling debug output
//#undef CA_DLOG
//#define CA_DLOG(...) printf(__VA_ARGS__)

class CaContext {
	public:
		CaContext(CaThreadBase *thread, CaNetBase *net) : thread(thread), net(net) {}

		void quit() { thread->quit_all(); }
		int process_id() const { return thread->get_process_id(); }
		int process_count() const { return thread->get_process_count(); }
		int threads_count() const { return thread->get_threads_count(); }
	protected:
		CaThreadBase *thread;
		CaNetBase *net;
};

/* Main functions */
void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs);
void ca_setup(int defs_count, CaNetDef **defs);
void ca_spawn_net(int def_id);
int ca_main();
void ca_project_description(const char *str);

/* In SHMEM returns first net of process[0], can be called only between ca_spawn_toplevel_net and ca_main */
CaNet *ca_get_main_net();
/* This method is used by module to send tokens to another process. Can be called after ca_spawn_toplevel_net*/
CaProcess * ca_get_first_process();

void ca_write_header(FILE *out, int process_count, int threads_count);

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
