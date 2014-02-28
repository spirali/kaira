
#include "cailie.h"
#include <stdio.h>
#include <string.h>

using namespace ca;

NetDef::NetDef(int index, int id, SpawnFn *spawn_fn)
{
	this->index = index;
	this->id = id;
	this->spawn_fn = spawn_fn;
}

NetDef::~NetDef()
{
}

Transition * NetDef::make_transitions()
{
	Transition *ts = new Transition[transition_defs.size()];
	for (size_t i = 0; i < transition_defs.size(); i++) {
		ts[i].set_def(transition_defs[i]);
	}
	return ts;
}

void NetDef::register_transition(TransitionDef *transition_def)
{
	transition_defs.push_back(transition_def);
}

TransitionDef* NetDef::get_transition_def(int transition_id)
{
	for (size_t t = 0; t < transition_defs.size(); t++) {
		if (transition_defs[t]->get_id() == transition_id) {
			return transition_defs[t];
		}
	}
	return NULL;
}

NetBase * NetDef::spawn(ThreadBase *thread)
{
	return spawn_fn(thread, this);
}

void NetBase::write_reports(ThreadBase *thread, Output &output)
{
	write_reports_content(thread, output);
}

Net::Net(NetDef *def, Thread *thread) :
	def(def),
	running_transitions(0),
	data(NULL),
	flags(0)
{
	transitions = def->make_transitions();
	activate_all_transitions();
}

Net::~Net()
{
	delete [] transitions;
}

void Net::activate_all_transitions()
{
	for (int t = 0; t < def->get_transitions_count(); t++) {
		transitions[t].set_active(true);
	}
}

Transition * Net::pick_active_transition()
{
	for (int t = 0; t < def->get_transitions_count(); t++) {
		if (transitions[t].is_active()) {
			return &transitions[t];
		}
	}
	return NULL;
}

int Net::fire_transition(Thread *thread, int transition_id)
{
	TransitionDef *tr = def->get_transition_def(transition_id);
	if (tr) {
		int r = tr->full_fire(thread, this);
		return r;
	}
	return -1;
}

bool Net::is_something_enabled(Thread *thread)
{
	int t;
	for (t = 0; t < def->get_transitions_count(); t++) {
		if (transitions[t].is_enable(thread, this)) {
			return true;
		}
	}
	return false;
}
