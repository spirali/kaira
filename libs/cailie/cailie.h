
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

#include "token.h"
#include "thread.h"
#include "net.h"
#include "parameters.h"
#include "usertools.h"

#define CA_DLOG(...)

// Uncommed two following two lines for enabling debug output
//#undef CA_DLOG
//#define CA_DLOG(...) printf(__VA_ARGS__)

namespace ca {

class Context {
	public:
		Context(ThreadBase *thread, NetBase *net) : thread(thread), net(net) {}

		void quit() { thread->quit_all(); }
		int process_id() const { return thread->get_process_id(); }
		int process_count() const { return thread->get_process_count(); }
		int threads_count() const { return thread->get_threads_count(); }

		void trace_value(const std::string &str) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(str);
			}
		}
		void trace(const int value) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(value);
			}
		}
		void trace(const double value) {
			TraceLog *tracelog = thread->get_tracelog();
			if (tracelog) {
				tracelog->trace_value(value);
			}
		}

	protected:
		ThreadBase *thread;
		NetBase *net;
};

/* Main functions */
void init(
	int argc,
	char **argv,
	std::vector<Parameter*> &parameters,
	const std::string& extra_args = "",
	void extra_args_callback(char, char*, void*) = NULL,
	void *extra_args_data = NULL);

void setup(int defs_count, NetDef **defs, bool start_process);
void spawn_net(int def_id);
int main();
void check_parameters();
void project_description(const char *str);

/* In SHMEM returns first net of process[0], can be called only between spawn_toplevel_net and main */
Net *get_main_net();
/* This method is used by module to send tokens to another process. n be called after spawn_toplevel_net*/
Process * get_first_process();

void write_header(FILE *out, int process_count, int threads_count);

extern int process_count;
extern int threads_count;
}

#endif
