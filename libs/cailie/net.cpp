
#include "cailie.h"
#include <stdio.h>
#include <string.h>

CaNetDef::CaNetDef(int index, int id, int transitions_count, CaSpawnFn *spawn_fn, bool local)
{
	this->index = index;
	this->id = id;
	this->transitions_count = transitions_count;
	this->spawn_fn = spawn_fn;
	this->transitions = new CaTransition[transitions_count];
	this->local = local;
}

CaNetDef::~CaNetDef()
{
	delete [] transitions;
}


CaTransition * CaNetDef::copy_transitions()
{
	CaTransition *ts = new CaTransition[transitions_count];
	memcpy(ts, transitions, transitions_count * sizeof(CaTransition));
	return ts;
}

void CaNetDef::register_transition(int i, int id, CaEnableFn *enable_fn,
	CaEnableCheckFn *enable_check_fn)
{
	transitions[i].id = id;
	transitions[i].enable_fn = enable_fn;
	transitions[i].enable_check_fn = enable_check_fn;
}

CaTransition* CaNetDef::get_transition(int transition_id)
{
	for (int t = 0; t < transition_id; t++) {
		if (transitions[t].id == transition_id) {
			return &transitions[t];
		}
	}
	return NULL;
}

CaNetBase * CaNetDef::spawn(CaThreadBase *thread)
{
	return spawn_fn(thread, this);
}

CaNet::CaNet(CaNetDef *def, CaThread *thread) :
	running_transitions(0),
	def(def),
	data(NULL),
	flags(0)
{
	if (thread->get_threads_count() > 1) {
		mutex = new pthread_mutex_t;
		pthread_mutex_init(mutex, NULL);
	} else {
		mutex = NULL;
	}
	transitions = def->copy_transitions();
	activate_all_transitions();
}

CaNet::~CaNet()
{
	delete [] transitions;
	if (mutex) {
		pthread_mutex_destroy(mutex);
		delete mutex;
	}
}

void CaNet::activate_all_transitions()
{
	for (int t = 0; t < def->get_transitions_count(); t++) {
		activate_transition(&transitions[t]);
	}
}

void CaNet::activate_transition_by_pos_id(int pos_id)
{
	activate_transition(&transitions[pos_id]);
}

void CaNet::write_reports(CaThread *thread, CaOutput &output)
{
	write_reports_content(thread, output);
}

CaTransition * CaNet::pick_active_transition()
{
	if (actives.empty()) {
		return NULL;
	}
	CaTransition *tr = actives.front();
	actives.pop();
	return tr;
}

int CaNet::fire_transition(CaThread *thread, int transition_id)
{
	CaTransition *tr = def->get_transition(transition_id);
	if (tr) {
		lock();
		int r = tr->fire(thread, this);
		if (r == CA_NOT_ENABLED) {
			unlock();
		}
		return r;
	}
	return -1;
}

bool CaNet::is_something_enabled(CaThread *thread)
{
	int t;
	for (t = 0; t < def->get_transitions_count(); t++) {
		if (transitions[t].is_enable(thread, this)) {
			return true;
		}
	}
	return false;
}

void CaNet::activate_transition(CaTransition *tr)
{
			if (tr->is_active()) {
				return;
			}
			CA_DLOG("Transition activated id=%i\n", tr->id);
			tr->set_active(true);
			actives.push(tr);
}
