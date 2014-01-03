#ifndef CAVERIF_ACTION_H
#define CAVERIF_ACTION_H

namespace cass {

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


typedef std::set<Action, ActionCompare> ActionSet;

}

#endif
