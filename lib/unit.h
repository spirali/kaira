
#ifndef CAILIE_UNIT_H
#define CAILIE_UNIT_H

#include <stdlib.h>
#include <vector>
#include <pthread.h>

#include "path.h"
#include "output.h"

class CaUnit;
class CaUnitDef;
class CaThread;

typedef CaUnit*(CaUnitInitFn)(CaUnitDef*, const CaPath &path);
typedef int(CaEnableFn)(CaUnit*, void *);
typedef void(CaFireFn)(CaThread*, CaUnit *, void *);

class CaTransition {
	public:
		void set_enable_fn(CaEnableFn *enable_fn) { this->enable_fn = enable_fn; }
		void set_fire_fn(CaFireFn *fire_fn) { this->fire_fn = fire_fn; }
		void set_var_size(size_t size) { this->var_size = size; }

		size_t get_var_size() const { return var_size; }
		bool is_enabled(CaUnit *unit);
		bool is_enabled(CaUnit *unit, void *vars) { return enable_fn(unit, vars); }
		void fire(CaThread *thread, CaUnit *unit, void *vars) { fire_fn(thread, unit, vars); }

		void set_id(int id) { this->id = id; }
		int get_id() { return id; }

	protected:
		CaEnableFn *enable_fn;
		CaFireFn *fire_fn;
		size_t var_size;
		int id;
};

class CaUnitDef {
	public:
		CaUnitDef(CaUnitInitFn *init_fn, int transitions_count);
		~CaUnitDef();

		void register_transition(int i,
			int id,
			CaEnableFn *enable_fn,
			CaFireFn *fire_fn,
			size_t var_size);
		void register_init(const CaMultiPath &mpath) { init_paths.push_back(mpath); }

		CaUnit * start_unit(const CaPath &path);
		CaUnit * lookup_or_start(const CaPath &path, int *start_flag);
		CaUnit * lookup(const CaPath &path);

		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void reports(CaOutput &output);
		void init_all();

		int get_transition_count() const { return transitions_count; }
		int get_units_count() const { return units.size(); }
		std::vector<CaTransition*> get_transitions() const;
		std::vector<CaUnit*> get_units() const { return units; }
		CaTransition * get_transition(int transition_id);


	protected:
		CaUnitInitFn *init_fn;
		int transitions_count;
		CaTransition *transitions;
		std::vector<CaUnit*> units;
		pthread_mutex_t mutex;
		std::vector<CaMultiPath> init_paths;
};

class CaUnit {

	public:
		CaUnit(CaUnitDef *def, const CaPath &p);
		virtual ~CaUnit() { pthread_mutex_destroy(&mutex); }

		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void report(CaUnitDef *def, CaOutput &out);
		virtual void report_places(CaOutput &out) = 0;

		CaPath path;
	protected:
		pthread_mutex_t mutex;
};

#endif
