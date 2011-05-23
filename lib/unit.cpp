
#include "unit.h"
#include <stdio.h>
#include <alloca.h>

CaUnitDef::CaUnitDef(CaUnitInitFn *init_fn, int transitions_count)
{
	pthread_mutex_init(&mutex, NULL);
	this->init_fn = init_fn;
	this->transitions_count = transitions_count;
	this->transitions = new CaTransition[transitions_count];
}

CaUnitDef::~CaUnitDef()
{
	pthread_mutex_destroy(&mutex);
	delete [] transitions;
}

void CaUnitDef::register_transition(int i,
			int id,
			CaEnableFn *enable_fn,
			CaFireFn *fire_fn,
			size_t var_size)
{
	transitions[i].set_enable_fn(enable_fn);
	transitions[i].set_fire_fn(fire_fn);
	transitions[i].set_var_size(var_size);
	transitions[i].set_id(id);
}

void CaUnitDef::reports(CaOutput &output)
{
	std::vector<CaUnit*>::iterator i;
	for (i = units.begin(); i != units.end(); i++) {
		(*i)->report(this, output);
	}
}

std::vector<CaTransition*> CaUnitDef::get_transitions() const
{
	std::vector<CaTransition*> ts;
	for (int i = 0; i < transitions_count; i++) {
		ts.push_back(&transitions[i]);
	}
	return ts;
}

CaUnit * CaUnitDef::start_unit(const CaPath &path)
{
	CaUnit *unit = init_fn(this, path);
	units.push_back(unit);
	return unit;
}

CaUnit * CaUnitDef::lookup_or_start(const CaPath &path, int *start_flag)
{
	CaUnit *u = lookup(path);
	if (u) {
		*start_flag = 0;
		return u;
	}
	*start_flag = 1;
	return start_unit(path);
}

CaUnit * CaUnitDef::lookup(const CaPath &path)
{
	std::vector<CaUnit*>::iterator i;
	for (i = units.begin(); i != units.end(); i++) {
		if ((*i)->path == path) {
			return *i;
		}
	}
	return NULL;
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

void CaUnitDef::init_all()
{
    std::vector<CaMultiPath>::iterator i;
    for (i = init_paths.begin(); i != init_paths.end(); i++)
    {
        CaMultiPath::Iterator iter = i->get_iterator();
        while(iter.has_next()) {
            start_unit(iter.next());
        }
    }
    init_paths.clear();
}

CaUnit::CaUnit(CaUnitDef *def, const CaPath &p) : path(p)
{
	pthread_mutex_init(&mutex, NULL);
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

bool CaTransition::is_enabled(CaUnit *unit)
{
	void *vars = alloca(var_size);
	return is_enabled(unit, vars);
}
