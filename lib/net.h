
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
		CaNetDef(int id, int transitions_count, CaSpawnFn *spawn_fn);
		~CaNetDef();

		CaNet *spawn(CaThread *thread, int id);

		int get_id() const { return id; }
		void register_transition(int i, int id, CaEnableFn *enable_fn);
		void activate_all_transitions(CaNet *net);

		void activate_transition_by_pos_id(CaNet *net, int pos_id);

		CaTransition * get_transition(int transition_id);

	protected:
		int id;
		int transitions_count;
		CaTransition *transitions;
		CaSpawnFn *spawn_fn;
};

class CaNet {
	public:
		CaNet(int id, int main_process_id, CaNetDef *def);
		virtual ~CaNet();
		
		int get_id() const { return id; }
		int get_main_process_id() const { return main_process_id; }

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

		void activate_transition_by_pos_id(int pos_id) {
			def->activate_transition_by_pos_id(this, pos_id);
		}

	protected:
		std::queue<CaTransition*> actives;
		CaNetDef *def;
		pthread_mutex_t mutex;
		int id;
		int main_process_id;
};

#endif // CAILIE_NET_H
