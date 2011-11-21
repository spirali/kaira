
#ifndef CAILIE_NET_H
#define CAILIE_NET_H

#include <queue>
#include <stdio.h>
#include "output.h"

class CaNet;
class CaNetDef;
class CaThread;
class CaUnpacker;

typedef int(CaEnableFn)(CaThread *, CaNet *);
typedef CaNet * (CaSpawnFn)(CaThread *, CaNetDef *, int id);

class CaTransition {
	public:
		CaTransition() : active(false) {}

		CaEnableFn *enable_fn;
		int id;

		bool is_active() { return active; }
		void set_active(bool value) { active = value; }

		bool fire(CaThread *thread, CaNet *net) { return enable_fn(thread, net); }

	protected:
		bool active;
};

class CaNetDef {

	public:
		CaNetDef(int index, int id, int transitions_count, CaSpawnFn *spawn_fn);
		~CaNetDef();

		CaNet *spawn(CaThread *thread, int id);
		int get_id() const { return id; }
		int get_index() const { return index; }
		void register_transition(int i, int id, CaEnableFn *enable_fn);
		CaTransition * get_transition(int transition_id);

		CaTransition * copy_transitions();
		int get_transitions_count() { return transitions_count; }

	protected:
		int index;
		int id;
		int transitions_count;
		CaTransition *transitions;
		CaSpawnFn *spawn_fn;
};

class CaNet {
	public:
		CaNet(int id, int main_process_id, CaNetDef *def, CaThread *thread);
		virtual ~CaNet();

		int get_id() const { return id; }
		int get_main_process_id() const { return main_process_id; }
		int get_def_id() const { return def->get_id(); }
		int get_def_index() const { return def->get_index(); }

		bool is_process_informed(int process_id) {
			return informed_processes[process_id];
		}
		void set_informed_process(int process_id) {
			informed_processes[process_id] = true;
		}

		/* Lock for working with active_units */
		void lock() { pthread_mutex_lock(&mutex); }
		void unlock() { pthread_mutex_unlock(&mutex); }

		void write_reports(CaThread *thread, CaOutput &output);
		virtual void write_reports_content(CaThread *thread, CaOutput &output) = 0;
		virtual void receive(int place, CaUnpacker &unpacker) = 0;
		void fire_transition(CaThread *thread, int transition_id);

		CaTransition * pick_active_transition();

		void activate_transition(CaTransition *tr) {
			if (tr->is_active()) {
				return;
			}
			tr->set_active(true);
			actives.push(tr);
		}

		void activate_all_transitions();
		void activate_transition_by_pos_id(int pos_id);

	protected:
		std::queue<CaTransition*> actives;
		CaNetDef *def;
		pthread_mutex_t mutex;
		int id;
		int main_process_id;
		CaTransition *transitions;

		bool *informed_processes;
};

#endif // CAILIE_NET_H
