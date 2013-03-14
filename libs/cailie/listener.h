
#ifndef CAILIE_LISTENER_H
#define CAILIE_LISTENER_H

#include <pthread.h>
#include "state.h"

namespace ca {

class Process;

class Listener {
	public:
		Listener() : process_count(0), processes(NULL) {}
		void init(int port);
		void wait_for_connection();
		int get_port();
		void set_processes(int process_count, Process **processes) {
			this->process_count = process_count;
			this->processes = processes;
		}
		void set_start_barrier(pthread_barrier_t *barrier) { start_barrier = barrier; }
		void start();
		void main();
		void process_commands(FILE *comm_in, FILE *comm_out);
	protected:

		void prepare_state();
		void cleanup_state();

		int listen_socket;
		int process_count;
		Process **processes;
		pthread_t thread;
		pthread_barrier_t *start_barrier;
		State *state;
};

}
#endif
