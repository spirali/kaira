
#ifndef CAILIE_THREAD_H
#define CAILIE_THREAD_H

#include "process.h"
#include "tracelog.h"
#include "messages.h"
#include <limits.h>

namespace ca {

class ThreadBase {
	public:
		ThreadBase() : tracelog(NULL) {}

		int get_id() { return id; }
		virtual ~ThreadBase() {}

		virtual void quit_all() = 0;
		virtual int get_process_count() const = 0;
		virtual int get_threads_count() const = 0;
		virtual int get_process_id() const = 0;

		TraceLog* get_tracelog() { return tracelog; }
	protected:
		int id;
		TraceLog *tracelog;
};

class Thread : public ThreadBase {
	public:
		Thread();
		~Thread();
		void start();
		void join();
		void run_scheduler();
		void run_one_step();

		int get_process_id() const { return process->get_process_id(); }
		int get_process_count() const { return process->get_process_count(); }
		int get_threads_count() const { return process->get_threads_count(); }

		#ifdef CA_MPI
		MpiRequests * get_requests() { return &requests; }
		#endif

		void add_message(ThreadMessage *message);
		bool process_thread_messages();
		int process_messages();
		void clean_thread_messages();
		void process_message(ThreadMessage *message);
		void quit_all();

		void send(int target, Net *net, int place, const Packer &packer) {
			process->multisend(target, net, place, 1, packer, this);
		}
		void multisend(int target, Net *net, int place, int tokens_count, const Packer &packer) {
			process->multisend(target, net, place, tokens_count, packer, this);
		}
		void send_multicast(const std::vector<int> &targets, Net *net, int place, const Packer &packer) {
			process->multisend_multicast(targets, net, place, 1, packer, this);
		}
		void multisend_multicast(const std::vector<int> &targets, Net *net,
			int place, int tokens_count, const Packer &packer)
		{
			process->multisend_multicast(targets, net, place, tokens_count, packer, this);
		}
		Process * get_process() const { return process; }

		Net * spawn_net(int def_index);

		void set_process(Process *process, int id) {
			this->process = process;
			this->id = id;
		}

		void set_tracelog(TraceLog *tracelog, int id) { this->tracelog = tracelog; this->msg_id = id; }
		int get_msg_id() { return msg_id; };
		int get_new_msg_id();


	protected:
		Process *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		ThreadMessage *messages;

		#ifdef CA_MPI
		MpiRequests requests;
		#endif

		int msg_id;
};

}

#endif
