
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include "path.h"
#include "unit.h"
#include "messages.h"

class CaProcess;
class CaThread;

class CaJob  {
	public:
	CaJob(CaUnit *unit, CaTransition *transition) {
		this->unit = unit;
		this->transition = transition;
	}

	int test_and_fire(CaThread *thread);

	protected:
	CaUnit *unit;
	CaTransition *transition;
};

class CaThread {
	public:
		CaThread();
		~CaThread();
		CaUnit * get_unit(const CaPath &path, int def_id);
		void set_process(CaProcess *process) { this->process = process; }

		void start();
		void join();
		void run_scheduler();

		void add_message(CaMessage *message);
		int process_messages();

		void add_job(const CaJob &job) { jobs.push_back(job); }

		void quit_all();
	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaMessage *messages;
		std::vector<CaJob> jobs;
};

class CaProcess {
	public:
		CaProcess(int threads_count, int defs_count, CaUnitDef **defs);
		virtual ~CaProcess() { delete [] threads; }
		void start();
		std::vector<CaJob> create_jobs() const;

		CaUnitDef *get_def(int def_id) const { return defs[def_id]; }

		void inform_new_unit(CaUnitDef *def, CaUnit *unit);

		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);

		int get_threads_count() const { return threads_count; }
		int get_units_count() const;
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id, const CaPath &path);

		void quit_all();

		bool quit_flag;
	protected:
		int threads_count;
		int defs_count;
		CaUnitDef **defs;
		CaThread *threads;
};

#endif
