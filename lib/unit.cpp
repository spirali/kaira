
#include <stdio.h>
#include <alloca.h>

#include "unit.h"
#include "process.h"
#include "network.h"

CaUnitDef::CaUnitDef(int id, CaUnitInitFn *init_fn, int transitions_count) : id(id)
{
	this->init_fn = init_fn;
	this->transitions_count = transitions_count;
	this->transitions = new CaTransition[transitions_count];
}

CaUnitDef::~CaUnitDef()
{
	delete [] transitions;
}

void CaUnitDef::register_transition(int i,
			int id,
			CaEnableFn *enable_fn,
			CaFireFn *fire_fn)
{
	transitions[i].set_enable_fn(enable_fn);
	transitions[i].set_fire_fn(fire_fn);
	transitions[i].set_id(id);
}

std::vector<CaTransition*> CaUnitDef::get_transitions() const
{
	std::vector<CaTransition*> ts;
	for (int i = 0; i < transitions_count; i++) {
		ts.push_back(&transitions[i]);
	}
	return ts;
}

CaUnit * CaUnitDef::new_unit(CaThread *thread, const CaPath &path)
{
	CaUnit *unit = init_fn(thread, this, path);
	return unit;
}

CaTransition * CaUnitDef::get_transition(int transition_id)
{
	int t;
	for (t = 0; t < transitions_count; t++) {
		if (transitions[t].get_id() == transition_id) {
			return &transitions[t];
		}
	}
	return NULL;
}


void CaUnitDef::init_all(CaNetwork *network, CaThread *thread)
{
    std::vector<CaMultiPath>::iterator i;
	CaProcess *process = thread->get_process();
    for (i = init_paths.begin(); i != init_paths.end(); i++)
    {
        CaMultiPath::Iterator iter = i->get_iterator();
        while(iter.has_next()) {
			CaPath p = iter.next();
			if (p.owner_id(process, id) == process->get_process_id() && !network->lookup(p, id)) {
				network->start_unit(this, thread, p);
			}
        }
    }
}

CaUnit::CaUnit(CaUnitDef *def, const CaPath &p) : path(p), def(def), active(false)
{
	pthread_mutex_init(&mutex, NULL);
}

void CaUnit::activate(CaNetwork *network)
{
	if (is_active()) {
		return;
	}
	set_active(true);
	network->lock_active();
	network->add_to_active_units(this);
	network->unlock_active();
}

void CaUnit::report(CaUnitDef *def, CaOutput &out)
{
	out.child("unit");
	out.set("path", path.as_string());
	std::vector<CaTransition*> ts = def->get_transitions();
	std::vector<CaTransition*>::iterator i;
	for (i = ts.begin(); i != ts.end(); i++) {
		out.child("transition");
		out.set("id", (*i)->get_id());
		out.set("enabled", (*i)->is_enabled(this));
		out.back();
	}
	report_places(out);
	out.back();
}

void CaUnit::log_status(CaLogger *logger, CaUnitDef *def)
{
	logger->log("U%s", path.as_string().c_str());
	std::vector<CaTransition*> ts = def->get_transitions();
	std::vector<CaTransition*>::iterator i;
	for (i = ts.begin(); i != ts.end(); i++) {
		if ((*i)->is_enabled(this)) {
			logger->log(" +%i", (*i)->get_id());
		} else {
			logger->log(" -%i", (*i)->get_id());
		}
	}
	logger->log("\n");
}

static void empty_fire(CaThread *thread, CaNetwork *network, CaUnit *unit, void *vars)
{

}

bool CaTransition::is_enabled(CaUnit *unit)
{
	return call(NULL, NULL, unit, empty_fire);
}
