
#include "cailie.h"
#include <sched.h>
#include <stdlib.h>
#include <alloca.h>

#include "thread.h"

using namespace ca;

Thread::Thread() : messages(NULL)
{
	pthread_mutex_init(&messages_mutex, NULL);
}

Thread::~Thread()
{
	pthread_mutex_destroy(&messages_mutex);
	if (tracelog) {
		delete tracelog;
	}
}

void Thread::add_message(ThreadMessage *message)
{
	pthread_mutex_lock(&messages_mutex);
	message->next = messages;
	messages = message;
	pthread_mutex_unlock(&messages_mutex);
}

void Thread::process_message(ThreadMessage *message)
{
	if (message->next) {
		process_message(message->next);
	}
	message->process(this);
	delete message;
}

bool Thread::process_thread_messages()
{
	if (messages) {
		ThreadMessage *m;
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

void Thread::clean_thread_messages()
{
	while(messages) {
		ThreadMessage *next = messages->next;
		delete messages;
		messages = next;
	}
}

int Thread::process_messages()
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
	Thread *thread = (Thread*) data;
	thread->run_scheduler();
	return NULL;
}

void Thread::start()
{
	CA_DLOG("Starting thread process=%i thread=%i\n", get_process_id(), id);
	pthread_create(&thread, NULL, thread_run, this);
}

void Thread::join()
{
	pthread_join(thread, NULL);
	#ifdef CA_MPI
	get_requests()->check();
	#endif
}

void Thread::quit_all()
{
	if (get_tracelog()) {
		get_tracelog()->event_net_quit();
	}
	process->quit_all(this);
}

void Thread::run_scheduler()
{
	process_messages();
	bool in_idle = false;
	while(!process->quit_flag) {
		process_messages();
		Net *n = process->get_net();
		if (n == NULL) {
			continue;
		}

		if (!n->try_lock()) {
			sched_yield();
			continue;
		}
		Transition *tr = n->pick_active_transition();
		if (tr == NULL) {
			n->unlock();
			if (!in_idle && tracelog) {
				tracelog->event_idle();
			}
			in_idle = true;
			continue;
		}
	    in_idle = false;
		CA_DLOG("Transition tried id=%i process=%i thread=%i\n",
				 tr->id, get_process_id(), id);
		int res = tr->full_fire(this, n);
		if (res == NOT_ENABLED) {
			CA_DLOG("Transition is not enabled id=%i process=%i thread=%i\n",
					 tr->id, get_process_id(), id);
			tr->set_active(false);
			n->unlock();
		}
	}
}

void Thread::run_one_step()
{
	process_messages();
	Net *net = process->get_net();
	if (net == NULL) {
		return;
	}
	Transition *tr = net->pick_active_transition();
	if (tr == NULL) {
		return;
	}
	tr->set_active(false);
	tr->full_fire(this, net);
}

Net * Thread::spawn_net(int def_index)
{
	return process->spawn_net(this, def_index, true);
}

int Thread::get_new_msg_id()
{
	if (tracelog) {
		if (msg_id >= INT_MAX - process->get_process_count() * process->get_threads_count()) {
			msg_id = msg_id % (process->get_process_count() * process->get_threads_count());
		}
		msg_id += process->get_process_count() * process->get_threads_count();
		return msg_id;
	} else {
		return 0;
	}
}


