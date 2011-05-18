
#ifndef CAILIE_LISTENER_H
#define CAILIE_LISTENER_H

#include <pthread.h>

class CaProcess;

class CaListener {
	public:
		CaListener(CaProcess *process) : process(process), start_barrier(NULL) {}
		void init(int port);
		int get_port();
		void set_start_barrier(pthread_barrier_t *barrier) { start_barrier = barrier; }
		void start();
		void main();
		void process_commands(FILE *comm_in, FILE *comm_out);
	protected:
		int listen_socket;
		CaProcess *process;
		pthread_barrier_t *start_barrier;
		pthread_t thread;
};

#endif
