
#ifndef CAILIE_THREADS_H
#define CAILIE_THREADS_H

#include "cailie.h"
#include "cailie_internal.h"

class CaThreadsProcess;
struct CaThreadsPacket;

class CaThreadsModule : public CaModule {

	public:
		int main(int nodes_count, InitFn *init_fn);

		CaContext * get_context(int node) { return _contexts[node]; }
		CaThreadsProcess * get_process(int process_id) { return _processes[process_id]; }
		int get_nodes_count() { return nodes_count; }
		int process_commands(FILE *comm_in, FILE *comm_out);
	protected:

		void write_reports(FILE *out);
		void fire_transition(int transition_id, int iid);

		CaThreadsProcess **_processes;
		CaContext **_contexts;
		void queue_add(int node, CaThreadsPacket *packet);
		int nodes_count;
};

class CaThreadsProcess : public CaProcess {

	public:
		CaThreadsProcess(CaThreadsModule *module, int process_id);
		~CaThreadsProcess();
		void start(InitFn *init_fn);
		void queue_add(CaThreadsPacket *packet);
		void send(CaContext *ctx, int target, int data_id, void *data, size_t size);
		void send_to_all(CaContext *ctx, int data_id, const void *data, size_t size);
		void send_to_all_processes(int data_id, const void *data, size_t size);
		int recv();
		void idle();
		void quit(CaContext *ctx);

		void add_context(CaContext *ctx);

		size_t get_reserved_prefix_size();

		void start_logging(CaContext *ctx, const std::string& logname);
		void stop_logging(CaContext *ctx);

		CaThreadsModule * get_module() { return _module; }
	protected:
		void process_packets(CaThreadsPacket *packet);

		pthread_mutex_t _lock;
		CaThreadsPacket *_packet;
		CaThreadsModule *_module;
};

#endif
