
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

typedef CaUnit*(CaUnitInitFn)(CaThread *thread, CaUnitDef*, const CaPath &path);
typedef void(CaFireFn)(CaThread *, CaUnit *, void *);
typedef int(CaEnableFn)(CaThread *, CaUnit*, CaFireFn *);

class CaTransition {
	public:
		void set_enable_fn(CaEnableFn *enable_fn) { this->enable_fn = enable_fn; }
		void set_fire_fn(CaFireFn *fire_fn) { this->fire_fn = fire_fn; }
		void set_var_size(size_t size) { this->var_size = size; }

		size_t get_var_size() const { return var_size; }
		bool is_enabled(CaUnit *unit);
		bool call(CaThread * thread, CaUnit *unit, CaFireFn *firefn) { return enable_fn(thread, unit, firefn); }
		bool call(CaThread * thread, CaUnit *unit) { return call(thread, unit, fire_fn); }

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
		CaUnitDef(int id, CaUnitInitFn *init_fn, int transitions_count);
		~CaUnitDef();

		void register_transition(int i,
			int id,
			CaEnableFn *enable_fn,
			CaFireFn *fire_fn,
			size_t var_size);
		void register_init(const CaMultiPath &mpath) { init_paths.push_back(mpath); }

		CaUnit * start_unit(CaThread *thread, const CaPath &path);
		CaUnit * lookup_or_start(CaThread *thread, const CaPath &path, int *start_flag);
		CaUnit * lookup(const CaPath &path);

		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void reports(CaOutput &output);
		void init_all(CaThread *thread);

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
		int id;
};

class CaUnit {

	public:
		CaUnit(CaUnitDef *def, const CaPath &p);
		virtual ~CaUnit() { pthread_mutex_destroy(&mutex); }

		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void report(CaUnitDef *def, CaOutput &out);
		virtual void report_places(CaOutput &out) = 0;
		virtual void receive(CaThread *thread, int place_pos, CaUnpacker &unpacker) = 0;

		void log_status(CaLogger *logger, CaUnitDef *def);
		CaPath path;
	protected:
		pthread_mutex_t mutex;
};

#endif
