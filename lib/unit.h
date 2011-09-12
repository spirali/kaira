
#ifndef CAILIE_UNIT_H
#define CAILIE_UNIT_H

#include <stdlib.h>
#include <vector>
#include <pthread.h>

#include "path.h"
#include "output.h"
#include "packing.h"

class CaUnit;
class CaUnitDef;
class CaThread;
class CaLogger;
class CaNetwork;

typedef CaUnit*(CaUnitInitFn)(CaThread *thread, CaUnitDef*, const CaPath &path);
typedef void(CaFireFn)(CaThread *, CaNetwork *, CaUnit *, void *);
typedef int(CaEnableFn)(CaThread *, CaNetwork *, CaUnit*, CaFireFn *);

class CaTransition {
	public:
		void set_enable_fn(CaEnableFn *enable_fn) { this->enable_fn = enable_fn; }
		void set_fire_fn(CaFireFn *fire_fn) { this->fire_fn = fire_fn; }
		bool is_enabled(CaUnit *unit);
		bool call(CaThread * thread, CaNetwork *network, CaUnit *unit, CaFireFn *firefn) 
			{ return enable_fn(thread, network, unit, firefn); }
		bool call(CaThread * thread, CaNetwork *network, CaUnit *unit) 
			{ return call(thread, network, unit, fire_fn); }

		void set_id(int id) { this->id = id; }
		int get_id() { return id; }

	protected:
		CaEnableFn *enable_fn;
		CaFireFn *fire_fn;
		int id;
};

class CaUnitDef {
	public:
		CaUnitDef(int id, CaUnitInitFn *init_fn, int transitions_count);
		~CaUnitDef();

		void register_transition(int i,
			int id,
			CaEnableFn *enable_fn,
			CaFireFn *fire_fn);
		void register_init(const CaMultiPath &mpath) { init_paths.push_back(mpath); }

		void init_all(CaNetwork *network, CaThread *thread);
		CaUnit * new_unit(CaThread *thread, const CaPath &path);
		int get_transition_count() const { return transitions_count; }
		std::vector<CaTransition*> get_transitions() const;
		CaTransition * get_transition(int transition_id);
		CaTransition * get_transition_at_pos(int pos) { return &transitions[pos]; }
		int get_id() { return id; }
	protected:
		CaUnitInitFn *init_fn;
		int transitions_count;
		CaTransition *transitions;
		std::vector<CaMultiPath> init_paths;
		int id;
};

class CaUnit {

	public:
		CaUnit(CaUnitDef *def, const CaPath &p);
		virtual ~CaUnit() { pthread_mutex_destroy(&mutex); }

		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }
		bool is_active() { return active; }
		void set_active(bool value) { active = value; }
		void activate(CaNetwork *network);

		int trylock() { return pthread_mutex_trylock(&mutex); }


		void report(CaUnitDef *def, CaOutput &out);
		virtual void report_places(CaOutput &out) = 0;
		virtual void receive(CaThread *thread, int place_pos, CaUnpacker &unpacker) = 0;

		void log_status(CaLogger *logger, CaUnitDef *def);

		int get_transition_count() const { return def->get_transition_count(); }
		CaTransition * get_transition_at_pos(int pos) { return def->get_transition_at_pos(pos); }


		CaPath path;
	protected:
		pthread_mutex_t mutex;
		CaUnitDef *def;
		bool active;
};

#endif
