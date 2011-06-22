
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include "path.h"
#include "unit.h"
#include "messages.h"
#include "logging.h"

#ifdef CA_MPI

#include "campi.h"
#define CA_RESERVED_PREFIX(path) (sizeof(CaPacket) + (path).get_size())

#else // CA_MPI not defined

#define CA_RESERVED_PREFIX(path) 0

#endif

class CaProcess;
class CaThread;

class CaJob  {
	public:
	CaJob(CaUnit *unit, CaTransition *transition) {
		this->unit = unit;
		this->transition = transition;
	}

	int test_and_fire(CaThread *thread);

	CaJob *next;

	protected:
	CaUnit *unit;
	CaTransition *transition;
};

class CaProcess {
	public:
		CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaUnitDef **defs);
		virtual ~CaProcess();
		void start();
		CaJob * create_jobs() const;

		CaUnitDef *get_def(int def_id) const { return defs[def_id]; }

		void inform_new_unit(CaUnitDef *def, CaUnit *unit);

		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);

		int get_threads_count() const { return threads_count; }
		int get_units_count() const;
		int get_process_count() const { return process_count; }
		int get_process_id() const { return process_id; }
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id, const CaPath &path);

		void quit_all();
		void quit() { quit_flag = true; }

		void start_logging(const std::string &logname);
		void stop_logging();

		bool quit_flag;
	protected:
		int process_id;
		int process_count;
		int threads_count;
		int defs_count;
		CaUnitDef **defs;
		CaThread *threads;
};

class CaThread {
	public:
		CaThread();
		~CaThread();
		int get_id() { return id; }
		CaUnit * get_unit(const CaPath &path, int def_id); 
		CaUnit * get_local_unit(const CaPath &path, int def_id);
		void set_process(CaProcess *process, int id) { this->process = process; this->id = id; }

		void start();
		void join();
		void run_scheduler();

		void add_message(CaMessage *message);
		int process_messages();

		void add_job(CaJob *job) {
			job->next = NULL;
			if (last_job) {
				last_job->next = job;
				last_job = job;
			} else {
				first_job = job;
				last_job = job;
			}
		}
		void quit_all();

		void send(const CaPath &path, int unit_id, int place_pos, const CaPacker &packer) {
			multisend(path, unit_id, place_pos, 1, packer);
		}
		void multisend(const CaPath &path, int unit_id, int place_pos, int tokens_count, const CaPacker &packer);
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
			if (logger) { unit->log_status(logger, process->get_def(def_id)); }
		}

		void start_logging(const std::string &logname) { process->start_logging(logname); }
		void stop_logging() { process->stop_logging(); }

	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaMessage *messages;
		CaJob *first_job;
		CaJob *last_job;
		int id;

		#ifdef CA_MPI
		CaMpiRequests requests;
		#endif

		CaLogger *logger;
};

#endif
