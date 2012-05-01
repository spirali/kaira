
#include <alloca.h>
#include <stdio.h>

#include "cailie.h"
#include <sched.h>
#include <stdlib.h>
#include <alloca.h>


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

bool CaThread::process_thread_messages()
{
	if (messages) {
		CaThreadMessage *m;
		pthread_mutex_lock(&messages_mutex);
		m = messages;
		messages = NULL;
		pthread_mutex_unlock(&messages_mutex);
		if (m != NULL) {
			process_message(m);
		}
		return true;
	}
	return false;
}

void CaThread::clean_thread_messages()
{
	while(messages) {
		CaThreadMessage *next = messages->next;
		delete messages;
		messages = next;
	}
}

int CaThread::process_messages()
{
	int result = process_thread_messages();

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
	CA_DLOG("Starting thread process=%i thread=%i\n", get_process_id(), id);
	pthread_create(&thread, NULL, thread_run, this);
}

void CaThread::join()
{
	pthread_join(thread, NULL);
	#ifdef CA_MPI
	get_requests()->check();
	#endif
}

void CaThread::clear()
{
	nets.clear();
}

void CaThread::quit_all()
{
	process->quit_all(this);
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

void CaProcess::multisend(int target, CaNet *net, int place_pos, int tokens_count, const CaPacker &packer, CaThread *thread)
{
	std::vector<int> a(1);
	a[0] = target;
	multisend_multicast(a, net, place_pos, tokens_count, packer, thread);
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

void CaProcess::process_packet(CaThread *thread, int tag, void *data)
{
	if (tag == CA_TAG_SERVICE) {
		CA_DLOG("SERVICE process=%i thread=%i\n", get_process_id(), thread->get_id());
		process_service_message(thread, (CaServiceMessage*) data);
		free(data);
		for(unsigned int i = 0 ; i < undeliver_message.size() ; i++)
		{
			if(is_created(undeliver_message[i].net_id))
			{
				CA_DLOG("Receive undeliver message, process=%d, net_id=%d\n", get_process_id(), undeliver_message[i].net_id);
				process_packet(thread, CA_TAG_TOKENS, undeliver_message[i].data);
				undeliver_message.erase(undeliver_message.begin() + i);
			}
		}
		return;
	}
	CaTokens *tokens = (CaTokens*) data;
	if(!is_created(tokens->net_id)) {
		CA_DLOG("Undeliver packets on process=%d net_id=%d\n", get_process_id(), tokens->net_id);
		CaUndeliverMessage msg;
		msg.net_id = tokens->net_id;
		msg.data = data;
		undeliver_message.push_back(msg);
		return;
	}
	CaUnpacker unpacker(tokens + 1);
	CaNet *n = thread->get_net(tokens->net_id);
	if (n == NULL) {
		CA_DLOG("Net not found net=%i process=%i thread=%i\n",
			tokens->net_id, get_process_id(), thread->get_id());
		// Net is already stopped therefore we can throw tokens away
		return;
	}
	n->lock();
	int place_index = tokens->place_index;
	int tokens_count = tokens->tokens_count;
	CA_DLOG("RECV net=%i index=%i process=%i thread=%i\n",
		tokens->net_id, place_index, get_process_id(), thread->get_id());
	for (int t = 0; t < tokens_count; t++) {
		n->receive(place_index, unpacker);
	}
	CA_DLOG("EOR index=%i process=%i thread=%i\n", place_index, get_process_id(), thread->get_id());
	n->unlock();
	free(data);
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
		{
			CaServiceMessageNetHalt *m = (CaServiceMessageNetHalt*) smsg;
			inform_halt_network(m->net_id, thread);
			break;
		}
		case CA_SM_WAKE:
		{
			start();
			join();
			clear();
			#ifdef CA_MPI
			MPI_Barrier(MPI_COMM_WORLD);
			#endif
			break;
		}
		case CA_SM_EXIT:
			free(smsg);
			exit(0);
	}
}

void CaThread::run_scheduler()
{
	process_messages();
	std::vector<CaNet*>::iterator net = nets.begin();
	unsigned int counter = 0;
	while(!process->quit_flag) {
		counter++;
		if (counter > nets.size()) {
			sched_yield();
			counter = 0;
		}
		if (process_messages()) {
			// Vector nets could be changed
			net = nets.begin();
		}
		if(nets.size() == 0) {
			continue;
		}
		net++;

		if (net == nets.end()) {
			net = nets.begin();
		}
		CaNet *n = *net;
		if (!n->try_lock())
			continue;
		CaTransition *tr = n->pick_active_transition();
		if (tr == NULL) {
			if (n->is_autohalt() && n->get_running_transitions() == 0) {
				n->unlock();
				halt(n);
				if (process_messages()) {
					// Vector nets could be changed
					net = nets.begin();
				}
			} else {
				n->unlock();
			}
			continue;
		}
		tr->set_active(false);
		CA_DLOG("Transition tried id=%i process=%i thread=%i\n", tr->id, get_process_id(), id);
		int res = tr->fire(this, n);
		if (res == CA_NOT_ENABLED) {
			CA_DLOG("Transition is dead id=%i process=%i thread=%i\n", tr->id, get_process_id(), id);
			n->unlock();
		} else {
			counter = 0;
			if (res == CA_TRANSITION_FIRED_WITH_MODULE) {
				net = nets.begin(); // Vector nets was changed
			}
		}
	}
}


CaNet * CaThread::spawn_net(int def_index, CaNet *parent_net)
{
	return process->spawn_net(this, def_index, process->new_net_id(), parent_net, true);
}

CaNet * CaProcess::spawn_net(CaThread *thread, int def_index, int id, CaNet *parent_net, bool globally)
{
	CA_DLOG("Spawning id=%i def_id=%i parent_net=%i globally=%i\n",
		id, def_index, parent_net?parent_net->get_id():-1, globally);
	if (globally && !defs[def_index]->is_local()) {
		CaServiceMessageNetCreate *m =
			(CaServiceMessageNetCreate *) alloca(sizeof(CaServiceMessageNetCreate));
		m->type = CA_SM_NET_CREATE;
		m->net_id = id;
		m->def_index = def_index;
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessageNetCreate), thread, process_id);
	}

	CaNet *net = defs[def_index]->spawn(thread, id, parent_net);
	net->lock();
	actualize_net_id_memory(net->get_id());
	inform_new_network(net, thread);
	return net;
}

void CaProcess::actualize_net_id_memory(int net_id)
{
	int src_proc = net_id % process_count;
	int net_counter = net_id / process_count;
	CA_DLOG("Actualize net id memory, process=%d, net_id=%d\n", process_id, net_id);
	if(net_id_memory[src_proc] < net_counter) {
		net_id_memory[src_proc] = net_counter;
	}
}

bool CaProcess::is_created(int net_id)
{
	int last_net, src_proc = net_id % process_count;
	int net_couter = net_id / process_count;
	last_net = net_id_memory[src_proc];
	if (last_net < net_couter) {
		return false;
	} else {
		return true;
	}
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
	pthread_mutex_init(&counter_mutex, NULL);
	net_id_memory = new int[process_count];
	for(int i = 0 ; i < process_count ; i++)
	{
		net_id_memory[i] = -1;
	}
	threads = new CaThread[threads_count];
	// TODO: ALLOCTEST
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].set_process(this, t);
	}

	#ifdef CA_SHMEM
	this->packets = NULL;
	pthread_mutex_init(&packet_mutex, NULL);
	#endif
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
	delete [] net_id_memory;

	#ifdef CA_SHMEM
	pthread_mutex_destroy(&packet_mutex);
	#endif
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

	// Clean up messages, it is important for reusing process in generated library
	for (t = 0; t < threads_count; t++) {
		threads[t].clean_thread_messages();
	}
}

void CaProcess::start_and_join()
{
	if (threads_count == 1) {
		// If there is only one process them process thread runs scheduler,
		// it is important because if threads_count == 1 we run MPI in MPI_THREAD_FUNELLED mode
		threads[0].run_scheduler();
	} else {
		start();
		join();
	}
}

void CaProcess::clear()
{
	for(int i = 0 ; i < threads_count ; i++)
	{
		get_thread(i)->clear();
	}
}

CaThread * CaProcess::get_thread(int id)
{
	return &threads[id];
}

void CaProcess::inform_new_network(CaNet *net, CaThread *thread)
{
	for (int t = 0; t < threads_count; t++) {
		if (thread && thread->get_id() == t) {
			CaThreadMessageNewNet msg(net);
			msg.process(thread);
		} else {
			threads[t].add_message(new CaThreadMessageNewNet(net));
		}
	}
}

void CaProcess::inform_halt_network(int net_id, CaThread *thread)
{
	for (int t = 0; t < threads_count; t++) {
		if (thread && thread->get_id() == t) {
			CaThreadMessageHaltNet msg(net_id);
			msg.process(thread);
		} else {
			threads[t].add_message(new CaThreadMessageHaltNet(net_id));
		}
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

void CaProcess::quit_all(CaThread *thread)
{
	CaServiceMessage *m = (CaServiceMessage*) alloca(sizeof(CaServiceMessage));
	m->type = CA_SM_QUIT;
	broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessage), thread, process_id);
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
void CaProcess::autohalt_check(CaNet *net)
{
	if (net->is_autohalt() && net->get_running_transitions() == 0
			&& !net->is_something_enabled(&threads[0])) {
			CaNet *parent = net->get_parent_net();
			/* During normal run net is finalized after halt in processing thread message
				But we dont want to wait for message processing because we want
				need right value of get_running_transitions in parent net */
			net->finalize(&threads[0]);
			halt(&threads[0], net);
			if (parent) {
				autohalt_check(parent);
			}
	}
}

// Designed for calling during simulation
void CaProcess::fire_transition(int transition_id, int instance_id)
{
	std::vector<CaNet*>::const_iterator i;
	const std::vector<CaNet*> &nets = threads[0].get_nets();
	for (i = nets.begin(); i != nets.end(); i++) {
		CaNet *n = *i;
		if (n->get_id() == instance_id) {
			if (n->fire_transition(&threads[0], transition_id)
				== CA_TRANSITION_FIRED_WITH_MODULE) {
				// Module was started so we have to checked if it is not dead from start
				threads[0].process_messages();
				n = threads[0].last_net();
			}
			autohalt_check(n);
			return;
		}
	}
}

/* 	Halt net net, sends information about halting if net is nonlocal
	Function inform_halt_network must send thread message to yourself, instance of net isn't free
	instantly, this is the reason why second argument is NULL */
void CaProcess::halt(CaThread *thread, CaNet *net)
{
	if (!net->is_local()) {
		CaServiceMessageNetHalt *m =
			(CaServiceMessageNetHalt*) alloca(sizeof(CaServiceMessageNetHalt));
		m->type = CA_SM_NET_HALT;
		m->net_id = net->get_id();
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessageNetHalt), thread, process_id);
	}
	inform_halt_network(net->get_id(), NULL);
}


