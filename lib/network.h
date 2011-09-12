
#ifndef CAILIE_NETWORK_H
#define CAILIE_NETWORK_H

#include "unit.h"
#include <queue>

class CaNetwork;
class CaThread;

class CaNetworkDef {

	public: 
		CaNetworkDef(int id, int units_count);
		~CaNetworkDef();
		void register_unit(int unit_id, CaUnitDef *unit);

		int get_defs_count() const { return defs_count; }

		CaNetwork *spawn(CaThread *thread);
		void init_units(CaNetwork *network, CaThread *thread);

		CaUnitDef * get_unit_def(int def_id) { return defs[def_id]; }

		int get_id() const { return id; }

	protected:
		int id;
		int defs_count;
		CaUnitDef **defs;
};

class CaNetwork {
	public:
		CaNetwork(int id, CaNetworkDef *def, CaThread *thread);
		~CaNetwork();
		
		int get_id() const { return id; }

		/* Lock for working with active_units */
		void lock_active() { pthread_mutex_lock(&mutex_active); }
		void unlock_active() { pthread_mutex_unlock(&mutex_active); }

		CaUnit *start_unit(CaUnitDef *def, CaThread *thread, const CaPath &path);

		void add_to_active_units(CaUnit *unit) { active_units.push(unit); }

		std::vector<CaUnit*> get_all_units(int i);

		CaUnit * lookup(const CaPath &path, int def_id);

		/* Need lock_active */
		CaUnit * pick_active_unit();

		CaUnitDef * get_unit_def(int def_id) { return def->get_unit_def(def_id); }

		void reports(CaOutput &output) const;
		void fire_transition(CaThread *thread, int transition_id, const CaPath &path);

	protected:

		/* Lock for working with list of all units */
		void lock_units(int def_id) { pthread_mutex_lock(&mutex_units[def_id]); }
		void unlock_units(int def_id) { pthread_mutex_unlock(&mutex_units[def_id]); }

		pthread_mutex_t mutex_active;
		pthread_mutex_t *mutex_units;
		std::vector<CaUnit*> *units;
		std::queue<CaUnit*> active_units;
		CaNetworkDef *def;
		int id;
};

#endif // CAILIE_NETWORK_H
