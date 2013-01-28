
#ifndef CAILIE_NET_H
#define CAILIE_NET_H

#include <queue>
#include <stdio.h>
#include "output.h"

namespace ca {

enum FireResult {
	NOT_ENABLED = 0,
	TRANSITION_FIRED
};

class NetBase;
class Net;
class NetDef;
class ThreadBase;
class Thread;
class Unpacker;

typedef NetBase * (SpawnFn)(ThreadBase *, NetDef *);

class TransitionDef {
	public:
		virtual ~TransitionDef() {}
		virtual int get_id() = 0;
		virtual FireResult full_fire(ThreadBase *thread, NetBase *net) = 0;
		virtual void *fire_phase1(ThreadBase *thread, NetBase *net) = 0;
		virtual void fire_phase2(ThreadBase *thread, NetBase *net, void *data) = 0;
		virtual void cleanup_binding(void *data) = 0;
		virtual bool is_enable(ThreadBase *thread, NetBase *net) = 0;
		virtual bool binding_equality(void *data1, void *data2) { return true; }
		virtual size_t binding_hash(void *data) { return 1; }
};

class Transition {
	public:
		Transition() : active(false), def(NULL) {}

		void set_def(TransitionDef *def) { this->def = def; }
		bool is_active() { return active; }
		void set_active(bool value) { active = value; }
		int get_id() { return def->get_id(); }

		int full_fire(ThreadBase *thread, NetBase *net) {
			return def->full_fire(thread, net);
		}

		bool is_enable(ThreadBase *thread, NetBase *net) {
			return def->is_enable(thread, net);
		}

	protected:
		bool active;
		TransitionDef *def;
};

class NetDef {

	public:
		NetDef(int index, int id, SpawnFn *spawn_fn, bool local);
		~NetDef();

		NetBase *spawn(ThreadBase *thread);
		int get_id() const { return id; }
		int get_index() const { return index; }
		bool is_local() const { return local; }
		void register_transition(TransitionDef *transition_def);
		TransitionDef* get_transition_def(int transition_id);
		int get_transitions_count() { return transition_defs.size(); }
		Transition * make_transitions();
		const std::vector<TransitionDef*> & get_transition_defs() { return transition_defs; }
	protected:
		int index;
		int id;
		std::vector<TransitionDef*> transition_defs;
		SpawnFn *spawn_fn;
		bool local;
};

#define CA_NET_MANUAL_DELETE 1

class NetBase {
		public:
			virtual void receive(ThreadBase *thread, int process, int place, Unpacker &unpacker) = 0;
};

class Net : public NetBase {
	public:
		Net(NetDef *def, Thread *thread);
		virtual ~Net();

		int get_def_id() const { return def->get_id(); }
		int get_def_index() const { return def->get_index(); }
		NetDef *get_def() const { return def; }
		bool is_local() const { return def->is_local(); }

		/* Lock for working with active_units */
		void lock() { if (mutex) pthread_mutex_lock(mutex); }
		bool try_lock() { return mutex?pthread_mutex_trylock(mutex) == 0:true; }
		void unlock() { if (mutex) pthread_mutex_unlock(mutex); }

		void write_reports(Thread *thread, Output &output);
		virtual void write_reports_content(Thread *thread, Output &output) = 0;
		int fire_transition(Thread *thread, int transition_id);

		Transition * pick_active_transition();
		bool has_active_transition() { return !actives.empty(); }

		void activate_transition(Transition *tr);

		void activate_all_transitions();
		void activate_transition_by_pos_id(int pos_id);

		bool is_something_enabled(Thread *thread);

		/* "manual delete" behaviour:
			net is not deleted automaticaly when it is quit and
			all threads remove its reference */
		void set_manual_delete() { flags |= CA_NET_MANUAL_DELETE; }
		bool get_manual_delete() { return flags & CA_NET_MANUAL_DELETE; }
	protected:
		std::queue<Transition*> actives;
		int running_transitions;
		NetDef *def;
		pthread_mutex_t *mutex;
		Transition *transitions;

		void *data;
		int flags;
};

}

#endif // CAILIE_NET_H
