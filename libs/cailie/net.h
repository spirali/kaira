
#ifndef CAILIE_NET_H
#define CAILIE_NET_H

#include <queue>
#include <stdio.h>
#include "output.h"

#define CA_NOT_ENABLED 0
#define CA_TRANSITION_FIRED 1

class CaNetBase;
class CaNet;
class CaNetDef;
class CaThreadBase;
class CaThread;
class CaUnpacker;

typedef CaNetBase * (CaSpawnFn)(CaThreadBase *, CaNetDef *);

class CaTransitionDef {
	public:
		virtual ~CaTransitionDef() {}
		virtual int get_id() = 0;
		virtual int full_fire(CaThreadBase *thread, CaNetBase *net) = 0;
		virtual void *fire_phase1(CaThreadBase *thread, CaNetBase *net) = 0;
		virtual void fire_phase2(CaThreadBase *thread, CaNetBase *net, void *data) = 0;
		virtual void cleanup_binding(void *data) = 0;
		virtual bool is_enable(CaThreadBase *thread, CaNetBase *net) = 0;
		virtual bool binding_equality(void *data1, void *data2) { return true; }
		virtual size_t binding_hash(void *data) { return 1; }
};

class CaTransition {
	public:
		CaTransition() : active(false), def(NULL) {}

		void set_def(CaTransitionDef *def) { this->def = def; }
		bool is_active() { return active; }
		void set_active(bool value) { active = value; }
		int get_id() { return def->get_id(); }

		int full_fire(CaThreadBase *thread, CaNetBase *net) {
			return def->full_fire(thread, net);
		}

		bool is_enable(CaThreadBase *thread, CaNetBase *net) {
			return def->is_enable(thread, net);
		}

	protected:
		bool active;
		CaTransitionDef *def;
};

class CaNetDef {

	public:
		CaNetDef(int index, int id, CaSpawnFn *spawn_fn, bool local);
		~CaNetDef();

		CaNetBase *spawn(CaThreadBase *thread);
		int get_id() const { return id; }
		int get_index() const { return index; }
		bool is_local() const { return local; }
		void register_transition(CaTransitionDef *transition_def);
		CaTransitionDef* get_transition_def(int transition_id);
		int get_transitions_count() { return transition_defs.size(); }
		CaTransition * make_transitions();
		const std::vector<CaTransitionDef*> & get_transition_defs() { return transition_defs; }
	protected:
		int index;
		int id;
		std::vector<CaTransitionDef*> transition_defs;
		CaSpawnFn *spawn_fn;
		bool local;
};

#define CA_NET_MANUAL_DELETE 1

class CaNetBase {
		public:
			virtual void receive(int place, CaUnpacker &unpacker) = 0;
};

class CaNet : public CaNetBase {
	public:
		CaNet(CaNetDef *def, CaThread *thread);
		virtual ~CaNet();

		int get_def_id() const { return def->get_id(); }
		int get_def_index() const { return def->get_index(); }
		CaNetDef *get_def() const { return def; }
		bool is_local() const { return def->is_local(); }

		/* Lock for working with active_units */
		void lock() { if (mutex) pthread_mutex_lock(mutex); }
		bool try_lock() { return mutex?pthread_mutex_trylock(mutex) == 0:true; }
		void unlock() { if (mutex) pthread_mutex_unlock(mutex); }

		void write_reports(CaThread *thread, CaOutput &output);
		virtual void write_reports_content(CaThread *thread, CaOutput &output) = 0;
		int fire_transition(CaThread *thread, int transition_id);

		CaTransition * pick_active_transition();
		bool has_active_transition() { return !actives.empty(); }

		void activate_transition(CaTransition *tr);

		void activate_all_transitions();
		void activate_transition_by_pos_id(int pos_id);

		bool is_something_enabled(CaThread *thread);

		/* "manual delete" behaviour:
			net is not deleted automaticaly when it is quit and
			all threads remove its reference */
		void set_manual_delete() { flags |= CA_NET_MANUAL_DELETE; }
		bool get_manual_delete() { return flags & CA_NET_MANUAL_DELETE; }
	protected:
		std::queue<CaTransition*> actives;
		int running_transitions;
		CaNetDef *def;
		pthread_mutex_t *mutex;
		CaTransition *transitions;

		void *data;
		int flags;
};

#endif // CAILIE_NET_H
