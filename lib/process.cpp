
#include <alloca.h>
#include <stdio.h>

#include "process.h"
#include <sched.h>
#include <stdlib.h>
#include <alloca.h>

#define CA_TAG_TOKENS 0
#define CA_TAG_SERVICE 1

enum CaServiceMessageType { CA_SM_QUIT, CA_SM_NET_CREATE, CA_SM_NET_HALT };

struct CaServiceMessage {
	CaServiceMessageType type;
};

struct CaServiceMessageNetCreate : CaServiceMessage {
	int net_id;
	int def_index;
};

struct CaServiceMessageNetHalt : CaServiceMessage {
	int net_id;
};

class CaPacket {
	public:
	int tag;
	void *data;
	CaPacket *next;
};

extern std::string ca_log_default_name;
extern const char *ca_project_description_string;
extern int ca_log_on;

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

void CaThread::add_message(CaThreadMessage *message)
{
	pthread_mutex_lock(&messages_mutex);
	message->next = messages;
	messages = message;
	pthread_mutex_unlock(&messages_mutex);
}

void CaThread::process_message(CaThreadMessage *message)
{
	if (message->next) {
		process_message(message->next);
	}
	message->process(this);
	delete message;
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

	if (messages) {
		CaThreadMessage *m;
		pthread_mutex_lock(&messages_mutex);
		m = messages;
		messages = NULL;
		pthread_mutex_unlock(&messages_mutex);
		if (m != NULL) {
			process_message(m);
			result = 1;
		}
	}
	if (id == 0) {
		return process->process_packets(this) || result;
	} else {
		return result;
	}
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

void CaProcess::multisend(int target, CaNet *net, int place_pos, int tokens_count, const CaPacker &packer)
{
	std::vector<int> a(1);
	a[0] = target;
	multisend_multicast(a, net, place_pos, tokens_count, packer);
	/*
	#ifdef CA_MPI
		CaPacket *packet = (CaPacket*) packer.get_buffer();
		packet->unit_id = unit_id;
		packet->place_pos = place_pos;
		eacket->tokens_count = tokens_count;
		path.copy_to_mem(packet + 1);
		MPI_Request *request = requests.new_request(buffer);
		MPI_Isend(packet, packer.get_size(), MPI_CHAR, path.owner_id(process, unit_id), CA_MPI_TAG_TOKENS, MPI_COMM_WORLD, request);
	#else
	#endif */
}

void CaProcess::multisend_multicast(const std::vector<int> &targets, CaNet *net, int place_pos, int tokens_count, const CaPacker &packer)
{
	char *buffer = packer.get_buffer();
	std::vector<int>::const_iterator i;
	CaTokens *data = (CaTokens*) packer.get_buffer();
	data->place_index = place_pos;
	data->net_id = net->get_id();
	data->tokens_count = tokens_count;
	for (i = targets.begin(); i != targets.end(); i++) {
		int target = *i % process_count;
		CaProcess *p = processes[target];
		void *d = malloc(packer.get_size());
		memcpy(d, data, packer.get_size());
		p->add_packet(CA_TAG_TOKENS, d);

	}
	free(buffer);
}

void CaProcess::process_packet(CaThread *thread, int tag, void *data)
{
	if (tag == CA_TAG_SERVICE) {
		process_service_message(thread, (CaServiceMessage*) data);
		return;
	}
	CaTokens *tokens = (CaTokens*) data;
	CaUnpacker unpacker(tokens + 1);
	CaNet *n = thread->get_net(tokens->net_id);
	if (n == NULL) {
		// Net is stopped in process
		return;
	}
	n->lock();
	int place_index = tokens->place_index;
	int tokens_count = tokens->tokens_count;
	for (int t = 0; t < tokens_count; t++) {
		n->receive(place_index, unpacker);
	}
	n->unlock();
}

void CaProcess::process_service_message(CaThread *thread, CaServiceMessage *smsg)
{
	switch (smsg->type) {
		case CA_SM_QUIT:
			quit();
			break;
		case CA_SM_NET_CREATE:
		{
			CaServiceMessageNetCreate *m = (CaServiceMessageNetCreate*) smsg;
			CaNet *net = spawn_net(thread, m->def_index, m->net_id, NULL, false);
			net->unlock();
			break;
		}
		case CA_SM_NET_HALT:
			CaServiceMessageNetHalt *m = (CaServiceMessageNetHalt*) smsg;
			inform_halt_network(m->net_id);
			break;
	}
}

int CaProcess::process_packets(CaThread *thread)
{
	if (packets) {
		pthread_mutex_lock(&packet_mutex);
		CaPacket *p = packets;
		packets = NULL;
		pthread_mutex_unlock(&packet_mutex);
		while (p) {
			process_packet(thread, p->tag, p->data);
			CaPacket *next = p->next;
			free(p->data);
			delete p;
			p = next;
		}
		return 1;
	}
	return 0;
}

void CaThread::run_scheduler()
{
	process_messages();
	std::vector<CaNet*>::iterator net = nets.begin();
	while(!process->quit_flag) {
		if (process_messages()) {
			// Vector nets could be changed
			net = nets.begin();
		}
		net++;
		if (net == nets.end()) {
			net = nets.begin();
		}
		CaNet *n = *net;
		n->lock();
		CaTransition *tr = n->pick_active_transition();

		if (tr == NULL) {
			if (n->is_autohalt() && n->get_running_transitions() == 0) {
				halt(n);
			}
			n->unlock();
			continue;
		}
		tr->set_active(false);
		if (!tr->fire(this, n)) {
			n->unlock();
		}
	}
}

CaNet * CaThread::spawn_net(int def_index, CaNet *parent_net)
{
	return process->spawn_net(this, def_index, process->new_net_id(), parent_net, true);
}

CaNet * CaProcess::spawn_net(CaThread *thread, int def_index, int id, CaNet *parent_net, bool globally)
{
	if (globally && !defs[def_index]->is_local()) {
		CaServiceMessageNetCreate *m =
			(CaServiceMessageNetCreate *) alloca(sizeof(CaServiceMessageNetCreate));
		m->type = CA_SM_NET_CREATE;
		m->net_id = id;
		m->def_index = def_index;
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessageNetCreate), process_id);
	}

	CaNet *net = defs[def_index]->spawn(thread, id, parent_net);
	net->lock();
	inform_new_network(net);
	return net;
}

CaNet * CaThread::remove_net(int id)
{
	std::vector<CaNet*>::iterator i;
	for (i = nets.begin(); i != nets.end(); i++) {
		if ((*i)->get_id() == id) {
			CaNet *net = *i;
			nets.erase(i);
			return net;
		}
	}
	return NULL;
}

CaNet * CaThread::get_net(int id)
{
	std::vector<CaNet*>::iterator i;
	for (i = nets.begin(); i != nets.end(); i++) {
		if ((*i)->get_id() == id) {
			return *i;
		}
	}
	return NULL;
}

CaProcess::CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaNetDef **defs)
{
	this->id_counter = process_id + process_count;
	this->process_id = process_id;
	this->process_count = process_count;
	this->defs_count = defs_count;
	this->defs = defs;
	this->threads_count = threads_count;
	this->packets = NULL;
	pthread_mutex_init(&counter_mutex, NULL);
	pthread_mutex_init(&packet_mutex, NULL);
	threads = new CaThread[threads_count];
	// TODO: ALLOCTEST
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].set_process(this, t);
	}

	CaNet *net = spawn_net(&threads[0], 0, 0, NULL, false);
	net->unlock();
}

int CaProcess::new_net_id()
{
	pthread_mutex_lock(&counter_mutex);
	id_counter += process_count;
	int id = id_counter;
	pthread_mutex_unlock(&counter_mutex);
	return id;
}

CaProcess::~CaProcess()
{
	delete [] threads;
	pthread_mutex_destroy(&counter_mutex);
	pthread_mutex_destroy(&packet_mutex);
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
		threads[t].add_message(new CaThreadMessageNewNet(net));
	}
}

void CaProcess::inform_halt_network(int net_id)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaThreadMessageHaltNet(net_id));
	}
}

void CaProcess::send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaThreadMessageBarriers(barrier1, barrier2));
	}
}


void CaProcess::start_logging(const std::string &logname)
{
	pthread_barrier_t *barrier1 = new pthread_barrier_t;
	pthread_barrier_t *barrier2 = new pthread_barrier_t;
	pthread_barrier_init(barrier1, NULL, threads_count);
	pthread_barrier_init(barrier2, NULL, threads_count);
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaThreadMessageLogInit(logname, barrier1, barrier2));
	}
}

void CaProcess::stop_logging()
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new CaThreadMessageLogClose);
	}
}

void CaProcess::quit_all()
{
	CaServiceMessage *m = (CaServiceMessage*) alloca(sizeof(CaServiceMessage));
	m->type = CA_SM_QUIT;
	broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessage), process_id);
	quit();
}

void CaProcess::write_reports(FILE *out) const
{
	CaOutput output;
	output.child("process");
	output.set("id", process_id);
	output.set("running", !quit_flag);

	std::vector<CaNet*>::const_iterator i;
	const std::vector<CaNet*> &nets = threads[0].get_nets();
	for (i = nets.begin(); i != nets.end(); i++) {
		(*i)->write_reports(&threads[0], output);
	}
	CaOutputBlock *block = output.back();
	block->write(out);
	delete block;
}

// Designed for calling during simulation
void CaProcess::fire_transition(int transition_id, int instance_id)
{
	std::vector<CaNet*>::const_iterator i;
	const std::vector<CaNet*> &nets = threads[0].get_nets();
	for (i = nets.begin(); i != nets.end(); i++) {
		CaNet *n = *i;
		if (n->get_id() == instance_id) {
			n->fire_transition(&threads[0], transition_id);
			if (n->is_autohalt() && n->get_running_transitions() == 0
				&& !n->is_something_enabled(&threads[0])) {
				halt(n);
			}
			return;
		}
	}
}

// Halt net net, sends information about halting if net is nonlocal
void CaProcess::halt(CaNet *net)
{
	if (!net->is_local()) {
		CaServiceMessageNetHalt *m =
			(CaServiceMessageNetHalt*) alloca(sizeof(CaServiceMessageNetHalt));
		m->type = CA_SM_NET_HALT;
		m->net_id = net->get_id();
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessageNetHalt), process_id);
	}
	inform_halt_network(net->get_id());
}

void CaProcess::broadcast_packet(int tag, void *data, size_t size, int exclude)
{
	for (int t = 0; t < process_count; t++) {
		if (t == exclude)
			continue;
		void *d = malloc(size);
		memcpy(d, data, size);
		processes[t]->add_packet(tag, d);
	}
}

void CaProcess::add_packet(int tag, void *data)
{
	CaPacket *packet = new CaPacket;
	packet->tag = tag;
	packet->data = data;
	packet->next = NULL;
	pthread_mutex_lock(&packet_mutex);
	if (packets == NULL) {
		packets = packet;
	} else {
		CaPacket *p = packets;
		while (p->next) {
			p = p->next;
		}
		p->next = packet;
	}
	pthread_mutex_unlock(&packet_mutex);
}
