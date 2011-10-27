
#include <alloca.h>
#include <stdio.h>

#include "process.h"
#include <sched.h>

extern std::string ca_log_default_name;
extern const char *ca_project_description_string;
extern int ca_log_on;
extern CaProcess **ca_processes;

CaThread::CaThread() : messages(NULL), logger(NULL)
{
	pthread_mutex_init(&messages_mutex, NULL);
}

CaThread::~CaThread()
{
	pthread_mutex_destroy(&messages_mutex);
	if (logger)
		close_log();
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
					if (logger) {
						unit->log_status(logger, process->get_def(packet->unit_id));
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

void CaThread::multisend(int target, int net_id, int place_pos, int tokens_count, const CaPacker &packer)
{
	/* It is called only when we have more CaProcesses in non-MPI backed */
	char *buffer = packer.get_buffer();

	CaUnpacker unpacker(buffer);
	CaProcess *p = ca_processes[target % process->get_process_count()];
	// Dirty hack!
	CaNet *net = p->get_thread(0)->nets[0];
	for (int t = 0; t < tokens_count; t++) {
		net->receive(place_pos, unpacker);
	}
	/*
	#ifdef CA_MPI
		CaPacket *packet = (CaPacket*) packer.get_buffer();
		packet->unit_id = unit_id;
		packet->place_pos = place_pos;
		packet->tokens_count = tokens_count;
		path.copy_to_mem(packet + 1);
		MPI_Request *request = requests.new_request(buffer);
		MPI_Isend(packet, packer.get_size(), MPI_CHAR, path.owner_id(process, unit_id), CA_MPI_TAG_TOKENS, MPI_COMM_WORLD, request);
	#else
	#endif */
}

void CaThread::run_scheduler()
{
	process_messages();

	std::vector<CaNet*>::iterator net = nets.begin();
	while(!process->quit_flag) {
		process_messages();
		net++;
		if (net == nets.end()) {
			net = nets.begin();
		}
		CaNet *n = *net;
		n->lock();
		CaTransition *tr = n->pick_active_transition();

		if (tr == NULL) {
			n->unlock();
			continue;
		}
		printf("TR: %i\n", tr->id);
		tr->set_active(false);
		if (!tr->fire(this, n)) {
			printf("F: %i\n", tr->id);
			n->unlock();
		}
	}
}

CaProcess::CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaNetDef **defs)
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

	CaNet *net = defs[0]->spawn(&threads[0], 0);
	inform_new_network(net);
}

CaProcess::~CaProcess()
{
	delete [] threads;
}

void CaProcess::start()
{
	quit_flag = false;

	if (ca_log_on) {
		start_logging(ca_log_default_name);
	}

	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].start();
	}
}

void CaProcess::join()
{
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].join();
	}
}

CaThread * CaProcess::get_thread(int id)
{
	return &threads[id];
}

void CaProcess::inform_new_network(CaNet *net)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaMessageNewNet(net));
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
	output.child("process");
	output.set("id", process_id);
	output.set("running", !quit_flag);

	const std::vector<CaNet*> &nets = threads[0].get_nets();
	std::vector<CaNet*>::const_iterator i;
	for (i = nets.begin(); i != nets.end(); i++) {
		(*i)->write_reports(&threads[0], output);
	}
	CaOutputBlock *block = output.back();
	block->write(out);
	delete block;
}

void CaProcess::fire_transition(int transition_id, int instance_id)
{
	const std::vector<CaNet*> &nets = threads[0].get_nets();
	std::vector<CaNet*>::const_iterator i;
	for (i = nets.begin(); i != nets.end(); i++) {
		if ((*i)->get_id() == instance_id) {
			(*i)->fire_transition(&threads[0], transition_id);
			return;
		}
	}
}
