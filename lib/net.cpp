
#include "net.h"
#include <stdio.h>

CaNetDef::CaNetDef(int id, int transitions_count, CaSpawnFn *spawn_fn)
{
	this->id = id;
	this->transitions_count = transitions_count;
	this->spawn_fn = spawn_fn;
	this->transitions = new CaTransition[transitions_count];
}

CaNetDef::~CaNetDef()
{
	delete [] transitions;
}

void CaNetDef::register_transition(int i, int id, CaEnableFn *enable_fn)
{
	transitions[i].id = id;
	transitions[i].enable_fn = enable_fn;
}

void CaNetDef::activate_all_transitions(CaNet *net)
{
	for (int t = 0; t < transitions_count; t++) {
		net->activate_transition(&transitions[t]);
	}
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

void CaNetDef::activate_transition_by_pos_id(CaNet *net, int pos_id)
{
	net->activate_transition(&transitions[pos_id]);
}

CaNet::CaNet(int id, CaNetDef *def) : def(def), id(id)
{
	pthread_mutex_init(&mutex, NULL);
	def->activate_all_transitions(this);
}

CaNet::~CaNet()
{
	pthread_mutex_destroy(&mutex);
}

void CaNet::write_reports(CaThread *thread, CaOutput &output)
{
	output.child("net-instance");
	output.set("id", 0);
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
