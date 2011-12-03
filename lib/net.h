
#ifndef CAILIE_NET_H
#define CAILIE_NET_H

#include <queue>
#include <stdio.h>
#include "output.h"

#define CA_NOT_ENABLED 0
#define CA_TRANSITION_FIRED 1
#define CA_TRANSITION_FIRED_WITH_MODULE 2

class CaNet;
class CaNetDef;
class CaThread;
class CaUnpacker;

typedef int(CaEnableFn)(CaThread *, CaNet *);
typedef bool(CaEnableCheckFn)(CaThread *, CaNet *);
typedef CaNet * (CaSpawnFn)(CaThread *, CaNetDef *, int id, CaNet *);
typedef void (CaNetFinalizerFn)(CaThread *, CaNet *, CaNet *, void *);

class CaTransition {
	public:
		CaTransition() : active(false) {}

		CaEnableFn *enable_fn;
		CaEnableCheckFn *enable_check_fn;
		int id;

		bool is_active() { return active; }
		void set_active(bool value) { active = value; }

		int fire(CaThread *thread, CaNet *net) { return enable_fn(thread, net); }

		bool is_enable(CaThread *thread, CaNet *net) { return enable_check_fn(thread, net); }

	protected:
		bool active;
};

class CaNetDef {

	public:
		CaNetDef(int index, int id, int transitions_count, CaSpawnFn *spawn_fn, bool local, bool autohalt);
		~CaNetDef();

		CaNet *spawn(CaThread *thread, int id, CaNet *parent_net);
		int get_id() const { return id; }
		int get_index() const { return index; }
		bool is_local() const { return local; }
		void register_transition(int i, int id,
			CaEnableFn *enable_fn, CaEnableCheckFn *enable_check_fn);
		CaTransition * get_transition(int transition_id);

		CaTransition * copy_transitions();
		int get_transitions_count() { return transitions_count; }
		bool is_autohalt() { return autohalt; }
	protected:
		int index;
		int id;
		int transitions_count;
		CaTransition *transitions;
		CaSpawnFn *spawn_fn;
		bool local;
		bool autohalt;
};

class CaNet {
	public:
		CaNet(int id, int main_process_id, CaNetDef *def, CaThread *thread, CaNet *parent_net);
		virtual ~CaNet();

		int get_id() const { return id; }
		int get_main_process_id() const { return main_process_id; }
		int get_def_id() const { return def->get_id(); }
		int get_def_index() const { return def->get_index(); }
		bool is_local() const { return def->is_local(); }

		/* Lock for working with active_units */
		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void write_reports(CaThread *thread, CaOutput &output);
		virtual void write_reports_content(CaThread *thread, CaOutput &output) = 0;
		virtual void receive(int place, CaUnpacker &unpacker) = 0;
		int fire_transition(CaThread *thread, int transition_id);

		CaTransition * pick_active_transition();
		bool has_active_transition() { return !actives.empty(); }

		void activate_transition(CaTransition *tr) {
			if (tr->is_active()) {
				return;
			}
			tr->set_active(true);
			actives.push(tr);
		}

		CaNet * get_parent_net() {
			return parent_net;
		}

		void activate_all_transitions();
		void activate_transition_by_pos_id(int pos_id);

		int decr_ref_count() { return --ref_count; }

		void set_finalizer(CaNetFinalizerFn *finalizer_fn, void *data) {
			this->finalizer_fn = finalizer_fn;
			this->data = data;
		}

		void finalize(CaThread *thread);

		/* Monitoring of running transitions' number
		   it is used for autohalt, if autohalt is disabled
           this methods don't have to be called */
		void inc_running_transitions() { running_transitions++; }
		void dec_running_transitions() { running_transitions--; }
		int get_running_transitions() { return running_transitions; }

		bool is_autohalt() { return def->is_autohalt(); }

		bool is_something_enabled(CaThread *thread);
	protected:
		std::queue<CaTransition*> actives;
		int running_transitions;
		CaNetDef *def;
		pthread_mutex_t mutex;
		int id;
		int main_process_id;
		CaTransition *transitions;
		int ref_count;

		CaNet *parent_net;
		CaNetFinalizerFn *finalizer_fn;
		void *data;

};

#endif // CAILIE_NET_H
