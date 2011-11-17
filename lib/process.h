
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include "messages.h"
#include "net.h"
#include "packing.h"
#include "logging.h"

#ifdef CA_MPI

#include "campi.h"
#define CA_RESERVED_PREFIX(path) (sizeof(CaPacket) + (path).get_size())

#else // CA_MPI not defined

#define CA_RESERVED_PREFIX(path) 0

#endif

class CaProcess;
class CaThread;

class CaProcess {
	public:
		CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaNetDef **defs);
		virtual ~CaProcess();
		void start();
		void join();
		void inform_new_network(CaNet *net);
		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);

		int get_threads_count() const { return threads_count; }
		int get_process_count() const { return process_count; }
		int get_process_id() const { return process_id; }
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id, int instance_id);

		void quit_all();
		void quit() { quit_flag = true; }

		void start_logging(const std::string &logname);
		void stop_logging();

		CaNet * spawn_net(CaThread *thread, int def_index, int id);
		int new_net_id();

		CaNet * get_net(int id);

		CaThread *get_thread(int id);

		bool quit_flag;

		void multisend(int target, int net_id, int place, int tokens_count, const CaPacker &packer);
		void multisend_multicast(const std::vector<int> &targets, int net_id, int place, int tokens_count, const CaPacker &packer);

		#ifndef CA_MPI
		void set_processes(CaProcess **processes) {
			this->processes = processes;
		}
		#endif

	protected:
		int process_id;
		int process_count;
		int threads_count;
		int defs_count;
		CaNetDef **defs;
		CaThread *threads;
		std::vector<CaNet*> nets;
		int id_counter;
		pthread_mutex_t counter_mutex;

		#ifndef CA_MPI
		CaProcess **processes;
		#endif
};

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

		void add_message(CaMessage *message);
		int process_messages();
		void quit_all();

		void send(int target, int net_id, int place, const CaPacker &packer) {
			process->multisend(target, net_id, place, 1, packer);
		}
		void multisend(int target, int net_id, int place, int tokens_count, const CaPacker &packer) {
			process->multisend(target, net_id, place, tokens_count, packer);
		}
		void send_multicast(const std::vector<int> &targets, int net_id, int place, const CaPacker &packer) {
			process->multisend_multicast(targets, net_id, place, 1, packer);
		}
		void multisend_multicast(const std::vector<int> &targets, int net_id, int place, int tokens_count, const CaPacker &packer) {
			process->multisend_multicast(targets, net_id, place, tokens_count, packer);
		}
		CaProcess * get_process() const { return process; }

		void init_log(const std::string &logname);
		void close_log() { if (logger) { delete logger; logger = NULL; } }

		CaNet * spawn_net(int def_index);
		/*
		void log_transition_start(CaUnit *unit, int transition_id) {
			if (logger) { logger->log_transition_start(unit, transition_id); }
		}

		void log_transition_end(CaUnit *unit, int transition_id) {
			if (logger) { logger->log_transition_end(unit, transition_id); }
		}

		void log_token_add(CaUnit *unit, int place_id, const std::string &token_string) {
			if (logger) { logger->log_token_add(unit, place_id, token_string); }
		}

		void log_token_remove(CaUnit *unit, int place_id, const std::string &token_string) {
			if (logger) { logger->log_token_remove(unit, place_id, token_string); }
		}

		void log_unit_status(CaUnit *unit, int def_id) {
		//	if (logger) { unit->log_status(logger, process->get_def(def_id)); }
		}
		*/

		void add_network(CaNet *net) {
			nets.push_back(net);
		}

		/*
		void start_logging(const std::string &logname) { process->start_logging(logname); }
		void stop_logging() { process->stop_logging(); }
		*/

		int get_nets_count() { return nets.size(); }
		const std::vector<CaNet*> & get_nets() { return nets; }

		void set_process(CaProcess *process, int id) { this->process = process; this->id = id; }

	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaMessage *messages;
		std::vector<CaNet*> nets;
		int id;

		#ifdef CA_MPI
		CaMpiRequests requests;
		#endif

		CaLogger *logger;
};

#endif
