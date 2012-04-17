
#ifndef CAILIE_LISTENER_H
#define CAILIE_LISTENER_H

#include <pthread.h>

class CaProcess;

class CaListener {
	public:
		CaListener(int process_count, CaProcess **processes) : process_count(process_count), processes(processes), start_barrier(NULL) {}
		void init(int port);
		int get_port();
		void set_start_barrier(pthread_barrier_t *barrier) { start_barrier = barrier; }
		void start();
		void main();
		void process_commands(FILE *comm_in, FILE *comm_out);
	protected:
		int listen_socket;
		int process_count;
		CaProcess **processes;
		pthread_barrier_t *start_barrier;
		pthread_t thread;
};

#endif
