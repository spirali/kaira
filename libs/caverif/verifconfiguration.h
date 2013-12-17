#ifndef CAVERIF_VERIFCONFIGURATION_H
#define CAVERIF_VERIFCONFIGURATION_H

#include <packing.h>
#include <set>
#include <list>

namespace cass {

struct Arc;
class State;
struct Action;
struct ActionCompare;
typedef std::set<Action, ActionCompare> WorkSet;

enum ActionType {
	ActionFire,
	ActionFinish,
	ActionReceive
};

struct Action
{
	ActionType type;
	int process;
	union {
		struct {
			ca::TransitionDef *transition_def;
		} fire;
		struct {
			int source;
			int edge_id;
		} receive;
	} data;

	void print(const std::string &start, const std::string &end) const {
		switch (type) {
			case ActionFire: {
				printf("%s(fire %d %d)%s", start.c_str(), process, data.fire.transition_def->get_id(), end.c_str());
			} break;
			case ActionReceive: {
				printf("%s(receive %d %d %d)%s", start.c_str(), process, data.receive.source, data.receive.edge_id, end.c_str());
			} break;
		}
	}
};

struct ActionCompare
{
	bool operator () (const Action &a1, const Action &a2) const {
		if (a1.process == a2.process) {
			if (a1.type == a2.type) {
				switch (a1.type) {
					case ActionFire:
						return a1.data.fire.transition_def->get_id() <
								a2.data.fire.transition_def->get_id();
					case ActionReceive:
						if (a1.data.receive.source == a2.data.receive.source) {
							return a1.data.receive.edge_id < a2.data.receive.edge_id;
						} else {
							return a1.data.receive.source < a2.data.receive.source;
						}
				}
			} else {
				return a1.type < a2.type;
			}
		} else {
			return a1.process < a2.process;
		}
	}
};

class VerifConfiguration {
	public:
		virtual bool compare(const Arc &arc1,const Arc &arc2) = 0;
		virtual void compute_successors(const Action &a, std::list<Action> &successors, State &s) = 0;
		virtual bool is_dependent(const Action &a1, const Action &a2, State &s) = 0;
		virtual void pack_final_marking(ca::NetBase *net, ca::Packer &packer) = 0;
		virtual ~VerifConfiguration() {};

		bool is_predecesor(const Action &pred, const Action &succ, State &s, bool debug)
		{
			std::list<Action> successors;
			std::set<Action, ActionCompare> analyzed;
			compute_successors(pred, successors, s);
			while(successors.size())
			{
				if (is_dependent(succ, successors.front(), s)) {
					if (debug) {
						successors.front().print(" IS DEP ON ", " PRE OF ");
					}
					return true;
				}
				if (!analyzed.count(successors.front())) {
					compute_successors(successors.front(), successors, s);
					analyzed.insert(successors.front());
				}
				successors.pop_front();
			}
			return false;
		}

		bool is_visible(const Action &a)
		{
			return visible.count(a);
		}

		bool is_transition_analyzed(int transition_id)
		{
			return ignored_transitions.count(transition_id) == 0;
		}

	protected:
		std::set<Action, ActionCompare> visible;
		std::set<int> ignored_transitions;
};

}

#endif // CAVERIF_VERIFCONFIGURATION_H
