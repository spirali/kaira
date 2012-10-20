
#ifndef CAILIE_THREAD_H
#define CAILIE_THREAD_H

#include "process.h"
#include "tracelog.h"
#include <limits.h>

class CaThread {
	public:
		CaThread();
		~CaThread();
		int get_id() { return id; }
		void start();
		void join();
		void run_scheduler();

		int get_process_id() { return process->get_process_id(); }
		int get_process_count() { return process->get_process_count(); }
		int get_threads_count() { return process->get_threads_count(); }

		#ifdef CA_MPI
		CaMpiRequests * get_requests() { return &requests; }
		#endif

		void add_message(CaThreadMessage *message);
		bool process_thread_messages();
		int process_messages();
		void clean_thread_messages();
		void process_message(CaThreadMessage *message);
		void quit_all();

		void send(int target, CaNet *net, int place, const CaPacker &packer) {
			process->multisend(target, net, place, 1, packer, this);
		}
		void multisend(int target, CaNet *net, int place, int tokens_count, const CaPacker &packer) {
			process->multisend(target, net, place, tokens_count, packer, this);
		}
		void send_multicast(const std::vector<int> &targets, CaNet *net, int place, const CaPacker &packer) {
			process->multisend_multicast(targets, net, place, 1, packer, this);
		}
		void multisend_multicast(const std::vector<int> &targets, CaNet *net,
			int place, int tokens_count, const CaPacker &packer)
		{
			process->multisend_multicast(targets, net, place, tokens_count, packer, this);
		}
		CaProcess * get_process() const { return process; }

		CaNet * spawn_net(int def_index);

		void set_process(CaProcess *process, int id) { this->process = process; this->id = id; }

		CaTraceLog* get_tracelog() { return tracelog; }
		void set_tracelog(CaTraceLog *tracelog, int id) { this->tracelog = tracelog; this->msg_id = id; }
		int get_msg_id() { return msg_id; };
		int get_new_msg_id();


	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaThreadMessage *messages;
		int id;

		#ifdef CA_MPI
		CaMpiRequests requests;
		#endif

		CaTraceLog *tracelog;
		int msg_id;
};

#endif
