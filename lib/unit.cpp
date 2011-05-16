
#include "unit.h"

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
			CaEnableFn *enable_fn, 
			CaFireFn *fire_fn, 
			size_t var_size)
{
	transitions[i].set_enable_fn(enable_fn);
	transitions[i].set_fire_fn(fire_fn);
	transitions[i].set_var_size(var_size);
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
	std::vector<CaUnit*>::iterator i;
	for (i = units.begin(); i != units.end(); i++) {
		if ((*i)->path == path) {
			*start_flag = 0;
			return *i;
		}
	}
	*start_flag = 1;
	return start_unit(path);
}

CaUnit::CaUnit(CaUnitDef *def, const CaPath &p) : path(p) 
{ 
	pthread_mutex_init(&mutex, NULL); 
}
