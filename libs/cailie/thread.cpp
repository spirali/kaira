
#include "cailie.h"
#include <sched.h>
#include <stdlib.h>
#include <alloca.h>

#include "thread.h"

CaThread::CaThread() : messages(NULL), tracelog(NULL)
{
	pthread_mutex_init(&messages_mutex, NULL);
}

CaThread::~CaThread()
{
	pthread_mutex_destroy(&messages_mutex);
	if (tracelog) {
		delete tracelog;
	}
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
		if (!net->get_manual_delete()) {
			//delete net;
		}
	}
}

void CaThread::quit_all()
{
	process->quit_all(this);
}

void CaThread::run_scheduler()
{
	process_messages();
	//std::vector<CaNet*>::iterator net = nets.begin();
	unsigned int counter = 0;
	while(!process->quit_flag) {
		counter++;
		if (counter > 1) {
			sched_yield();
			counter = 0;
		}
		process_messages();

		if(net == NULL) {
			continue;
		}

		CaNet *n = net;
		if (!n->try_lock())
			continue;
		CaTransition *tr = n->pick_active_transition();
		if (tr == NULL) {
			if (n->is_autohalt() && n->get_running_transitions() == 0) {
				n->unlock();
				halt(n);
				process_messages();
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
		}
	}
}

CaNet * CaThread::spawn_net(int def_index)
{
	return process->spawn_net(this, def_index, process->new_net_id(), true);
}

CaNet * CaThread::remove_net()
{
	CaNet *n = net;
	net = NULL;
	return n;
}


