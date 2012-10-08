
#include "cailie.h"
#include <stdio.h>
#include <string.h>

CaNetDef::CaNetDef(int index, int id, CaSpawnFn *spawn_fn, bool local)
{
	this->index = index;
	this->id = id;
	this->spawn_fn = spawn_fn;
	this->local = local;
}

CaNetDef::~CaNetDef()
{
}

CaTransition * CaNetDef::make_transitions()
{
	CaTransition *ts = new CaTransition[transition_defs.size()];
	for (size_t i = 0; i < transition_defs.size(); i++) {
		ts[i].set_def(transition_defs[i]);
	}
	return ts;
}

void CaNetDef::register_transition(CaTransitionDef *transition_def)
{
	transition_defs.push_back(transition_def);
}

CaTransitionDef* CaNetDef::get_transition_def(int transition_id)
{
	for (size_t t = 0; t < transition_defs.size(); t++) {
		if (transition_defs[t]->get_id() == transition_id) {
			return transition_defs[t];
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
	transitions = def->make_transitions();
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
	CaTransitionDef *tr = def->get_transition_def(transition_id);
	if (tr) {
		lock();
		int r = tr->full_fire(thread, this);
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
