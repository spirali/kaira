
#include "net.h"
#include "process.h"
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

void CaNetDef::register_transition(int i, int id, CaEnableFn *enable_fn)
{
	transitions[i].id = id;
	transitions[i].enable_fn = enable_fn;
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

CaNet * CaNetDef::spawn(CaThread *thread, int id)
{
	return spawn_fn(thread, this, id);
}

CaNet::CaNet(int id, int main_process_id, CaNetDef *def, CaThread *thread) :
	def(def), id(id), main_process_id(main_process_id)
{
	pthread_mutex_init(&mutex, NULL);
	transitions = def->copy_transitions();
	activate_all_transitions();
}

CaNet::~CaNet()
{
	delete [] transitions;
	pthread_mutex_destroy(&mutex);
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
	output.child("net-instance");
	output.set("id", id);
	output.set("net-id", def->get_id());
	write_reports_content(thread, output);
	output.back();
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

void CaNet::fire_transition(CaThread *thread, int transition_id)
{
	CaTransition *tr = def->get_transition(transition_id);
	if (tr) {
		lock();
		bool r = tr->fire(thread, this);
		if (!r) {
			unlock();
		}
		return;
	}
}
