
#include "cailie.h"

#include <alloca.h>
#include <stdio.h>

#include <sstream>


namespace ca {
extern size_t tracelog_size;
}

using namespace ca;

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
	int edge_id = tokens->edge_id;
	int tokens_count = tokens->tokens_count;
	CA_DLOG("RECV net=%i index=%i process=%i thread=%i\n",
		tokens->net_id, edge_id, get_process_id(), thread->get_id());
	for (int t = 0; t < tokens_count; t++) {
		n->receive(thread, from_process, edge_id, unpacker);
	}
	CA_DLOG("EOR index=%i process=%i thread=%i\n", edge_id, get_process_id(), thread->get_id());
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
			spawn_net(m->def_index, false);
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
			start(false);
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

Net * Process::spawn_net(int def_index, bool globally)
{
	TraceLog *tracelog = thread->get_tracelog();
	if (tracelog) {
		tracelog->event_net_spawn(defs[def_index]->get_id());
	}

	CA_DLOG("Spawning def_id=%i globally=%i\n",
		 def_index, globally);
	if (globally) {
		ServiceMessageNetCreate *m =
			(ServiceMessageNetCreate *) malloc(sizeof(ServiceMessageNetCreate));
		m->type = CA_SM_NET_CREATE;
		m->def_index = def_index;
		broadcast_packet(CA_TAG_SERVICE, m, sizeof(ServiceMessageNetCreate), process_id);
	}

	net = (Net *) defs[def_index]->spawn(thread);
	return net;
}


Process::Process(
	int process_id,
	int process_count,
	int defs_count,
	NetDef **defs) : thread(new Thread(this))
{
	this->process_id = process_id;
	this->process_count = process_count;
	this->defs_count = defs_count;
	this->defs = defs;
	this->net_is_quit = false;
	this->quit_flag = false;
	this->net = NULL;

	if (tracelog_size > 0) {
		thread->set_tracelog(new RealTimeTraceLog(process_id, 0, tracelog_size));
	}

	#ifdef CA_SHMEM
	this->packets = NULL;
	pthread_mutex_init(&packet_mutex, NULL);
	#endif
}

Process::~Process()
{
	delete thread;
	#ifdef CA_SHMEM
	pthread_mutex_destroy(&packet_mutex);
	#endif
}

void Process::start(bool own_thread) {
	quit_flag = false;
	if (!own_thread) {
		thread->run_scheduler();
	} else {
		thread->start();
	}
}

void Process::join() {
	thread->join();
}

void Process::clear()
{
	if(net != NULL && !net->get_manual_delete()) {
		delete net;
	}
	net = NULL;
}

// NOTE: Relic of old thread system
void Process::send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2)
{
	thread->add_message(new ThreadMessageBarriers(barrier1, barrier2));
}

void Process::quit_all()
{
	ServiceMessage *m = (ServiceMessage*) malloc(sizeof(ServiceMessage));
	m->type = CA_SM_QUIT;
	broadcast_packet(CA_TAG_SERVICE, m, sizeof(ServiceMessage), process_id);
	quit();
}

void Process::quit()
{
	quit_flag = true;
}
