
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include "path.h"
#include "unit.h"
#include "messages.h"
#include "logging.h"
#include "network.h"

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
		CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaNetworkDef **defs);
		virtual ~CaProcess();
		void start();
		void inform_new_network(CaNetwork *network);
		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);

		int get_threads_count() const { return threads_count; }
		int get_process_count() const { return process_count; }
		int get_process_id() const { return process_id; }
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id, int instance_id, const CaPath &path);

		void quit_all();
		void quit() { quit_flag = true; }

		void start_logging(const std::string &logname);
		void stop_logging();

		CaThread *get_thread(int id);

		bool quit_flag;
	protected:
		int process_id;
		int process_count;
		int threads_count;
		int defs_count;
		CaNetworkDef **defs;
		CaThread *threads;
};

class CaThread {
	public:
		CaThread();
		~CaThread();
		int get_id() { return id; }
		/* If unit is local then returns existing unit or start new one, 
			if unit is not local then return NULL */
		CaUnit * get_unit(CaNetwork *network, const CaPath &path, int def_id); 
		void set_process(CaProcess *process, int id) { this->process = process; this->id = id; }

		void start();
		void join();
		void run_scheduler();

		void add_message(CaMessage *message);
		int process_messages();

		void quit_all();

		void send(CaNetwork *network, const CaPath &path, int unit_id, int place_pos, const CaPacker &packer) {
			multisend(network, path, unit_id, place_pos, 1, packer);
		}
		void multisend(CaNetwork *network, const CaPath &path, int unit_id, int place_pos, int tokens_count, const CaPacker &packer);
		CaProcess * get_process() const { return process; }

		void init_log(const std::string &logname);
		void close_log() { if (logger) { delete logger; logger = NULL; } }

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

		void add_network(CaNetwork *network) {
			networks.push_back(network);
		}

		void start_logging(const std::string &logname) { process->start_logging(logname); }
		void stop_logging() { process->stop_logging(); }

		int get_networks_count() { return networks.size(); }
		const std::vector<CaNetwork*> & get_networks() { return networks; }

	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaMessage *messages;
		std::vector<CaNetwork*> networks;
		int id;

		#ifdef CA_MPI
		CaMpiRequests requests;
		#endif

		CaLogger *logger;
};

#endif
