
#include "network.h"

CaNetworkDef::CaNetworkDef(int id, int defs_count)
{
	this->id = id;
	this->defs_count = defs_count;
	this->defs = new CaUnitDef*[defs_count];
}

CaNetworkDef::~CaNetworkDef()
{
	delete [] this->defs;
}

CaNetwork * CaNetworkDef::spawn(CaThread *thread)
{
	 return new CaNetwork(0, this, thread);
}


void CaNetworkDef::register_unit(int unit_id, CaUnitDef *unit)
{
	defs[unit_id] = unit;
}

void CaNetworkDef::init_units(CaNetwork *network, CaThread *thread)
{
	int t;
	for (t = 0; t < defs_count; t++) {
		defs[t]->init_all(network, thread);
	}
}

CaNetwork::CaNetwork(int id, CaNetworkDef *def, CaThread *thread) : def(def), id(id)
{
	units = new std::vector<CaUnit*>[def->get_defs_count()];
	pthread_mutex_init(&mutex_active, NULL);
	mutex_units = new pthread_mutex_t[def->get_defs_count()];
	int t;
	for (t = 0; t < def->get_defs_count(); t++) {
		pthread_mutex_init(&mutex_units[t], NULL);
	}
	def->init_units(this, thread);
}

CaNetwork::~CaNetwork()
{
	delete [] units;
	pthread_mutex_destroy(&mutex_active);
	int t;
	for (t = 0; t < def->get_defs_count(); t++) {
		pthread_mutex_destroy(&mutex_units[t]);
	}
}

CaUnit * CaNetwork::start_unit(CaUnitDef *def, CaThread *thread, const CaPath &path)
{
	CaUnit *unit = def->new_unit(thread, path);
	lock_units(def->get_id());
	units[def->get_id()].push_back(unit);
	unlock_units(def->get_id());
	unit->activate(this);
	return unit;
}

CaUnit * CaNetwork::pick_active_unit()
{
	if (active_units.empty()) {
		return NULL;
	}
	CaUnit *unit = active_units.front();
	active_units.pop();
	return unit;
}

CaUnit * CaNetwork::lookup(const CaPath &path, int def_id)
{
	lock_units(def_id);
	std::vector<CaUnit*> *u = &units[def_id];
	std::vector<CaUnit*>::iterator i;
	for (i = u->begin(); i != u->end(); i++) {
		if ((*i)->path == path) {
			unlock_units(def_id);
			return *i;
		}
	}
	unlock_units(def_id);
	return NULL;
}

void CaNetwork::reports(CaOutput &output) const
{
	output.child("net");
	output.set("id", 0);
	output.set("net-id", def->get_id());
	std::vector<CaUnit*>::iterator i;
	for (int t = 0; t < def->get_defs_count(); t++) {
		for (i = units[t].begin(); i != units[t].end(); i++) {
			(*i)->report(def->get_unit_def(t), output);
		}
	}
	output.back();
}

void CaNetwork::fire_transition(CaThread *thread, int transition_id, const CaPath &path)
{
	int t;
	for (t = 0; t < def->get_defs_count(); t++) {
		CaTransition *tr = def->get_unit_def(t)->get_transition(transition_id);
		if (tr) {
			CaUnit *u = lookup(path, t);
			if (u) {
				u->lock();
				int r = tr->call(thread, this, u);
				if (!r) {
					u->unlock();
				}
				return;
			}

		}
	}
}
