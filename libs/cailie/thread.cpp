
#include "cailie.h"
#include <sched.h>
#include <stdlib.h>
#include <alloca.h>

#include "thread.h"

CaThread::CaThread() : messages(NULL)
{
	pthread_mutex_init(&messages_mutex, NULL);
}

CaThread::~CaThread()
{
	pthread_mutex_destroy(&messages_mutex);
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
	process_messages();
	if (id == 0) {
		std::vector<CaNet*>::const_iterator it;
		for (it = nets.begin(); it < nets.end(); it++) {
			if (!(*it)->get_manual_delete()) {
				delete (*it);
			}
		}
	}
	nets.clear();
}

void CaThread::quit_all()
{
	process->quit_all(this);
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


