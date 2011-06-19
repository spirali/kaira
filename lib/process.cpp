
#include <alloca.h>
#include <stdio.h>

#include "process.h"
#include "listener.h"

extern int ca_listen_port;
extern int ca_block_on_start;
extern int ca_log_on;
extern std::string ca_log_default_name;
extern const char *ca_project_description_string;

CaThread::CaThread() : messages(NULL),first_job(NULL), last_job(NULL)
{
	pthread_mutex_init(&messages_mutex, NULL);
}

CaThread::~CaThread()
{
	pthread_mutex_destroy(&messages_mutex);
	if (logger)
		close_log();
}

CaUnit * CaThread::get_unit(const CaPath &path, int def_id)
{
 	return path.owner_id(process, def_id) == process->get_process_id() ? get_local_unit(path, def_id) : NULL;
}

CaUnit * CaThread::get_local_unit(const CaPath &path, int def_id)
{
	CaUnitDef *def = process->get_def(def_id);
	def->lock();
	int spawn_flag;
	CaUnit *unit = def->lookup_or_start(this, path, &spawn_flag);
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
	int result = 0;
	#ifdef CA_MPI
		requests.check();
		int flag;
		MPI_Status status;
		MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);

		if (flag) {
			if (logger) {
				logger->log_receive();
			}
			for(;;) {
				int msg_size;
				MPI_Get_count(&status, MPI_CHAR, &msg_size);

				char *buffer = (char*) alloca(msg_size); // FIXME: For large packets alloc memory on heap
				MPI_Recv(buffer, msg_size, MPI_CHAR, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

				if (status.MPI_TAG == CA_MPI_TAG_TOKENS) {
					CaPacket *packet = (CaPacket*) buffer;
					char *next_data = (char*) (packet + 1);
					CaPath path((int*) (next_data));
					next_data += path.get_size();
					CaUnpacker unpacker(next_data);
					CaUnit *unit = get_local_unit(path, packet->unit_id);
					unit->lock();
					for (int t = 0; t < packet->tokens_count; t++) {
						unit->receive(this, packet->place_pos, unpacker);
					}
					unit->unlock();
				} else if (status.MPI_TAG == CA_MPI_TAG_QUIT) {
					process->quit();
				} else {
					fprintf(stderr, "Invalid message tag\n");
				}
				MPI_Iprobe(MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &flag, &status);
				if (!flag)
					break;
			}
			result = 1;
		}
	#endif

	CaMessage *m;
	pthread_mutex_lock(&messages_mutex);
	m = messages;
	messages = NULL;
	pthread_mutex_unlock(&messages_mutex);
	if (m == NULL) {
		return result;
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

void CaThread::quit_all()
{
	process->quit_all();
}

void CaThread::init_log(const std::string &logname)
{
	if (logger) {
		return;
	}
	logger = new CaLogger(logname, process->get_process_id() * process->get_threads_count() + id);
	
	if (id == 0) {
		int lines = 1;
		for (const char *c = ca_project_description_string; (*c) != 0; c++) {
			if ((*c) == '\n') {
				lines++;
			}
		}

		FILE *out = logger->get_file();
		CaOutput output;
		output.child("header");
		output.set("process-count", process->get_process_count());
		output.set("threads-count", process->get_threads_count());
		output.set("description-lines", lines);
		CaOutputBlock *block = output.back();
		block->write(out);
		delete block;
		fputs("\n", out);
		fputs(ca_project_description_string, out);
		fputs("\n", out);
		process->write_reports(out);
	}
	logger->log_time();
	logger->flush();
}

void CaThread::multisend(const CaPath &path, int unit_id, int place_pos, int tokens_count, const CaPacker &packer)
{
	char *buffer = packer.get_buffer();

	#ifdef CA_MPI
		CaPacket *packet = (CaPacket*) packer.get_buffer();
		packet->unit_id = unit_id;
		packet->place_pos = place_pos;
		packet->tokens_count = tokens_count;
		path.copy_to_mem(packet + 1);
		MPI_Request *request = requests.new_request(buffer);
		MPI_Isend(packet, packer.get_size(), MPI_CHAR, path.owner_id(process, unit_id), CA_MPI_TAG_TOKENS, MPI_COMM_WORLD, request);
	#else
		/* Normally this is never called for threads backend, because all units are local 
			and packing is not used. But in with "forced packing" enabled this function is called
			so we have to deliver data */
		CaUnit *unit = get_local_unit(path, unit_id);
		unit->lock();
		CaUnpacker unpacker(buffer);
		for (int t = 0; t < tokens_count; t++) {
			unit->receive(this, place_pos, unpacker);
		}
		unit->unlock();
		free(buffer);
	#endif
}

void CaThread::run_scheduler()
{
	process_messages();
	first_job = process->create_jobs();
	if (first_job) {
		CaJob *j = first_job->next;
		last_job = first_job;
		while (j) {
			last_job = j;
			j = j->next;
		}
	} else {
		last_job = NULL;
	}
	while (!process->quit_flag) {
		CaJob *j = first_job;
		CaJob *prev = NULL;
		while(j) {
			if (j->test_and_fire(this)) {
				if (j->next) {
					if (prev) {
						prev->next = j->next;
					} else {
						first_job = j->next;
					}
					last_job->next = j;
					last_job = j;
					j->next = NULL;
				}
				break;
			}
			prev = j;
			j = j->next;
		}
		process_messages();
	}
}

CaProcess::CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaUnitDef **defs)
{
	this->process_id = process_id;
	this->process_count = process_count;
	this->defs_count = defs_count;
	this->defs = defs;
	this->threads_count = threads_count;
	threads = new CaThread[threads_count];
	// TODO: ALLOCTEST
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].set_process(this, t);
	}
}

CaProcess::~CaProcess()
{
	delete [] threads;
}

void CaProcess::start()
{
	quit_flag = false;
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

	for (t = 0; t < defs_count; t++) {
		defs[t]->init_all(&threads[0]);
	}

	if (ca_log_on) {
		start_logging(ca_log_default_name);
	}

	for (t = 0; t < threads_count; t++) {
		threads[t].start();
	}

	for (t = 0; t < threads_count; t++) {
		threads[t].join();
	}
}

CaJob * CaProcess::create_jobs() const
{
	CaJob *first = new CaJob(NULL, NULL);
	CaJob *job = first;
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
				job->next = new CaJob(*i, *j);
				job = job->next;
			}
		}
	}
	job->next = NULL;
	job = first->next;
	delete first;
	return job;
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


void CaProcess::start_logging(const std::string &logname)
{
	pthread_barrier_t *barrier1 = new pthread_barrier_t;
	pthread_barrier_t *barrier2 = new pthread_barrier_t;
	pthread_barrier_init(barrier1, NULL, threads_count);
	pthread_barrier_init(barrier2, NULL, threads_count);
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaMessageLogInit(logname, barrier1, barrier2));
	}
}

void CaProcess::stop_logging()
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaMessageLogClose);
	}
}

void CaProcess::quit_all()
{
	#ifdef CA_MPI
	ca_mpi_send_to_all(NULL, 0, MPI_CHAR, CA_MPI_TAG_QUIT, process_count);
	#endif
	quit();
}

void CaProcess::write_reports(FILE *out) const
{
	CaOutput output;
	output.child("units");
	output.set("running", !quit_flag);
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

void CaProcess::fire_transition(int transition_id, const CaPath &path)
{
	int t;
	for (t = 0; t < defs_count; t++) {
		CaTransition *tr = defs[t]->get_transition(transition_id);
		if (tr) {
			CaUnit *u = defs[t]->lookup(path);
			if (u) {
				CaJob job(u, tr);
				job.test_and_fire(&threads[0]);
			}
			break;
		}
	}
}

int CaJob::test_and_fire(CaThread *thread)
{
	unit->lock();
	int r = transition->call(thread, unit);
	if (!r) {
		unit->unlock();
		return 0;
	}
	return 1;
}
