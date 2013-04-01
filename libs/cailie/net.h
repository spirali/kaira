
#ifndef CAILIE_NET_H
#define CAILIE_NET_H

#include <queue>
#include <stdio.h>
#include "output.h"
#include "packing.h"

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
		TransitionDef(int id, bool immediate) : id(id), immediate(immediate) {}
		virtual ~TransitionDef() {}
		int get_id() { return id; }
		int is_immediate() { return immediate; }
		virtual FireResult full_fire(ThreadBase *thread, NetBase *net) = 0;
		virtual void* fire_phase1(ThreadBase *thread, NetBase *net) = 0;
		virtual void fire_phase2(ThreadBase *thread, NetBase *net, void *data) = 0;
		virtual void cleanup_binding(void *data) = 0;
		virtual bool is_enable(ThreadBase *thread, NetBase *net) = 0;
		virtual void pack_binding(Packer &pack, void *data) {}
	protected:
		int id;
		bool immediate;
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
			void write_reports(ThreadBase *thread, Output &output);
		protected:
			virtual void write_reports_content(ThreadBase *thread, Output &output) = 0;
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

		int fire_transition(Thread *thread, int transition_id);


		virtual NetDef *get_def() { return def; }

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
		NetDef *def;
		std::queue<Transition*> actives;
		int running_transitions;
		pthread_mutex_t *mutex;
		Transition *transitions;

		void *data;
		int flags;
};

}

#endif // CAILIE_NET_H
