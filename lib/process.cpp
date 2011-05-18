
#include <alloca.h>
#include <stdio.h>

#include "process.h"
#include "listener.h"

extern int ca_listen_port;
extern int ca_block_on_start;

CaThread::CaThread()
{
	messages = NULL;
	pthread_mutex_init(&messages_mutex, NULL);
}

CaThread::~CaThread()
{
	pthread_mutex_destroy(&messages_mutex);
}


CaUnit * CaThread::get_unit(const CaPath &path, int def_id)
{
	CaUnitDef *def = process->get_def(def_id);
	def->lock();
	int spawn_flag;
	CaUnit *unit = def->lookup_or_start(path, &spawn_flag);
	if (spawn_flag) {
		process->inform_new_unit(def, unit);
	}
	def->unlock();
	return unit;
}

void CaThread::add_message(CaMessage *message)
{
	pthread_mutex_lock(&messages_mutex);
	if (messages) {
		CaMessage *m = messages;
		while(m->next) { m = m->next; }
		m->next = message;
	} else {
		messages = message;
	}
	pthread_mutex_unlock(&messages_mutex);
}

int CaThread::process_messages()
{
	CaMessage *m;
	pthread_mutex_lock(&messages_mutex);
	m = messages;
	messages = NULL;
	pthread_mutex_unlock(&messages_mutex);
	if (m == NULL) {
		return 0;
	}
	
	do {
		m->process(this);
		CaMessage *next = m->next;
		delete m;
		m = next;
	} while (m);
	
	return 1;
}

static void * thread_run(void *data)
{
	CaThread *thread = (CaThread*) data;
	thread->run_scheduler();
	return NULL;
}

void CaThread::start()
{
	pthread_create(&thread, NULL, thread_run, this);
}

void CaThread::join()
{
	pthread_join(thread, NULL);
}

void CaThread::run_scheduler()
{
	process_messages();
	jobs = process->create_jobs();
	for (;;) {
		int i;
		for (i = 0; i < jobs.size(); i++) {
			jobs[i].test_and_fire(this);
		}
		process_messages();
	}
}

CaProcess::CaProcess(int threads_count, int defs_count, CaUnitDef **defs)
{
	this->defs_count = defs_count;
	this->defs = defs;
	this->threads_count = threads_count;
	threads = new CaThread[threads_count];
	// TODO: ALLOCTEST
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].set_process(this);
	}
}

void CaProcess::start()
{
	CaListener listener(this);
	pthread_barrier_t start_barrier;

	if (ca_listen_port != -1) {
		listener.init(ca_listen_port);
		if (ca_listen_port == 0) {
			printf("%i\n", listener.get_port());
			fflush(stdout);
		}

		if (ca_block_on_start) {
			pthread_barrier_init(&start_barrier, NULL, 2);
			listener.set_start_barrier(&start_barrier);
		}

		listener.start();

		if (ca_block_on_start) {
			pthread_barrier_wait(&start_barrier);
			pthread_barrier_destroy(&start_barrier);
		}
	}

	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].start();
	}

	for (t = 0; t < threads_count; t++) {
		threads[t].join();
	}
}

std::vector<CaJob> CaProcess::create_jobs() const
{
	std::vector<CaJob> jobs;
	int t;
	for (t = 0; t < defs_count; t++) {
		defs[t]->lock();
		std::vector<CaUnit*> units = defs[t]->get_units();
		std::vector<CaTransition*> transitions = defs[t]->get_transitions();
		defs[t]->unlock();
		std::vector<CaUnit*>::iterator i;
		std::vector<CaTransition*>::iterator j;
		for (j = transitions.begin(); j != transitions.end(); j++) {
			for (i = units.begin(); i != units.end(); i++) {
				jobs.push_back(CaJob(*i, *j));
			}
		}
	}
	return jobs;
}

void CaProcess::inform_new_unit(CaUnitDef *def, CaUnit *unit)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaMessageNewUnit(def, unit));
	}
}

void CaProcess::send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaMessageBarriers(barrier1, barrier2));
	}
}

void CaProcess::write_reports(FILE *out) const
{
	CaOutput output;
	output.child("units");
	for (int t = 0; t < defs_count; t++) {
		defs[t]->reports(output);
	}
	CaOutputBlock *block = output.back();
	block->write(out);
	fprintf(out, "\n");
	fflush(stdout);
	delete block;
}

int CaProcess::get_units_count() const
{
	int count = 0;
	for (int t = 0; t < defs_count; t++) {
		count += defs[t]->get_units_count();
	}
	return count;
}

int CaJob::test_and_fire(CaThread *thread)
{
	unit->lock();
	
	void *vars = alloca(transition->get_var_size());
	int r = transition->is_enabled(unit, vars);
	if (!r) {
		unit->unlock();
		return 0;
	}
	transition->fire(thread, unit, vars);
	return 1;
}
