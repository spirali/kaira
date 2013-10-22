
#include "cailie.h"

#include <alloca.h>
#include <stdio.h>

#include <sstream>


namespace ca {
extern size_t tracelog_size;
}

using namespace ca;

void Process::send(int target, Net *net, int edge_id, int tokens_count, const Packer &packer, Thread *thread)
{
	std::vector<int> a(1);
	a[0] = target;
	send_multicast(a, net, edge_id, tokens_count, packer, thread);
}

bool Process::process_packet(Thread *thread, int from_process, int tag, void *data)
{
	if (tag == CA_TAG_SERVICE) {
		process_service_message(thread, (ServiceMessage*) data);
		free(data);
		return false;
	}
	Tokens *tokens = (Tokens*) data;
	if(net == NULL) {
		CA_DLOG("Too early message on process=%d", get_process_id());
		EarlyMessage msg;
		msg.from_process = from_process;
		msg.data = data;
		too_early_message.push_back(msg);
		return false;
	}
	Unpacker unpacker(tokens + 1);
	Net *n = net;
	TraceLog *tracelog = thread->get_tracelog();
	if (tracelog) {
		tracelog->event_receive(from_process);
	}
	if (n == NULL) {
		CA_DLOG("Net not found process=%i thread=%i\n",
			get_process_id(), thread->get_id());
		// Net is already stopped therefore we can throw tokens away
		return false;
	}
	n->lock();
	int edge_id = tokens->edge_id;
	int tokens_count = tokens->tokens_count;
	CA_DLOG("RECV net=%i index=%i process=%i thread=%i\n",
		tokens->net_id, edge_id, get_process_id(), thread->get_id());
	for (int t = 0; t < tokens_count; t++) {
		n->receive(thread, from_process, edge_id, unpacker);
	}
	CA_DLOG("EOR index=%i process=%i thread=%i\n", edge_id, get_process_id(), thread->get_id());
	n->unlock();
	free(data);
	return true;
}

void Process::process_service_message(Thread *thread, ServiceMessage *smsg)
{
	switch (smsg->type) {
		case CA_SM_QUIT:
			CA_DLOG("SERVICE CA_SM_QUIT on process=%i thread=%i\n", get_process_id(), thread->get_id());
			if(net == NULL) {
				CA_DLOG("Quitting not created net on process=%d\n", get_process_id());
				net_is_quit = true;
			}
			too_early_message.clear();
			quit();
			break;
		case CA_SM_NET_CREATE:
		{
			CA_DLOG("SERVICE CA_SM_NET_CREATE on process=%i thread=%i\n", get_process_id(), thread->get_id());
			ServiceMessageNetCreate *m = (ServiceMessageNetCreate*) smsg;
			if(net_is_quit) {
				CA_DLOG("Stop creating quit net on process=%i thread=%i\n", get_process_id(), thread->get_id());
				too_early_message.clear();
				net_is_quit = false;
				break;
			}
			Net *net = (Net *) spawn_net(thread, m->def_index, false);
			net->unlock();
			if(too_early_message.size() > 0) {
				std::vector<EarlyMessage>::const_iterator i;
				for (i = too_early_message.begin(); i != too_early_message.end(); i++) {
					process_packet(thread, i->from_process, CA_TAG_TOKENS, i->data);
				}
				if (too_early_message.size() > 0) {
					TraceLog *tracelog = thread->get_tracelog();
					if (tracelog) {
						tracelog->event_end();
					}
				}
				too_early_message.clear();
			}
			break;
		}
		case CA_SM_WAKE:
		{
			CA_DLOG("SERVICE CA_SM_WAKE on process=%i thread=%i\n", get_process_id(), thread->get_id());
			start_and_join();
			clear();
			#ifdef CA_MPI
			MPI_Barrier(MPI_COMM_WORLD);
			#endif
			break;
		}
		case CA_SM_EXIT:
			CA_DLOG("SERVICE CA_SM_EXIT on process=%i thread=%i\n", get_process_id(), thread->get_id());
			too_early_message.clear();
			free(smsg);
			exit(0);
	}
}

Net * Process::spawn_net(Thread *thread, int def_index, bool globally)
{
	TraceLog *tracelog = thread->get_tracelog();
	if (tracelog) {
		tracelog->event_net_spawn(defs[def_index]->get_id());
	}

	CA_DLOG("Spawning def_id=%i globally=%i\n",
		 def_index, globally);
	if (globally && !defs[def_index]->is_local()) {
		ServiceMessageNetCreate *m =
			(ServiceMessageNetCreate *) malloc(sizeof(ServiceMessageNetCreate));
		m->type = CA_SM_NET_CREATE;
		m->def_index = def_index;
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(ServiceMessageNetCreate), thread, process_id);
	}

	net = (Net *) defs[def_index]->spawn(thread);
	net->lock();
	return net;
}


Process::Process(
	int process_id,
	int process_count,
	int threads_count,
	int defs_count,
	NetDef **defs)
{
	this->process_id = process_id;
	this->process_count = process_count;
	this->defs_count = defs_count;
	this->defs = defs;
	this->threads_count = threads_count;
	this->net_is_quit = false;
	this->quit_flag = false;
	this->net = NULL;
	threads = new Thread[threads_count];
	// TODO: ALLOCTEST
	for (int t = 0; t < threads_count; t++) {
		threads[t].set_process(this, t);
	}

	if (tracelog_size > 0) {
		for (int t = 0; t < threads_count; t++) {
			threads[t].set_tracelog(new RealTimeTraceLog(process_id, t, tracelog_size));
		}
	}

	#ifdef CA_SHMEM
	this->packets = NULL;
	pthread_mutex_init(&packet_mutex, NULL);
	#endif
}

Process::~Process()
{
	delete [] threads;

	#ifdef CA_SHMEM
	pthread_mutex_destroy(&packet_mutex);
	#endif
}

void Process::start()
{
	quit_flag = false;

	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].start();
	}
}

void Process::join()
{
	int t;
	for (t = 0; t < threads_count; t++) {
		threads[t].join();
	}
}

void Process::start_and_join()
{
	if (threads_count == 1) {
		// If there is only one process them process thread runs scheduler,
		// it is important because if threads_count == 1 we run MPI in MPI_THREAD_FUNELLED mode
		quit_flag = false;
		threads[0].run_scheduler();
	} else {
		start();
		join();
	}
}

void Process::clear()
{
	if(net != NULL && !net->get_manual_delete()) {
		delete net;
	}
	net = NULL;
}

Thread * Process::get_thread(int id)
{
	return &threads[id];
}

void Process::send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2)
{
	for (int t = 0; t < threads_count; t++) {
		threads[t].add_message(new ThreadMessageBarriers(barrier1, barrier2));
	}
}

void Process::quit_all(Thread *thread)
{
	ServiceMessage *m = (ServiceMessage*) malloc(sizeof(ServiceMessage));
	m->type = CA_SM_QUIT;
	broadcast_packet(CA_TAG_SERVICE, m, sizeof(ServiceMessage), thread, process_id);
	quit();
}

void Process::quit()
{
	quit_flag = true;
}
