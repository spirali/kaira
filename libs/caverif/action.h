#ifndef CAVERIF_ACTION_H
#define CAVERIF_ACTION_H

namespace cass {

enum ActionType {
	ActionFire,
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
};

inline std::ostream& operator<<(std::ostream &os, const Action &a){
	switch (a.type) {
		case ActionFire: {
			os << "<F " << a.process << " " << a.data.fire.transition_def->get_id() << ">";
		} break;
		case ActionReceive: {
			os << "<R " << a.process << " " << a.data.receive.source << " " << a.data.receive.edge_id << ">";
		} break;
	}
	return os;
}

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
					default:
						return false;
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

inline std::ostream& operator<<(std::ostream& os, const std::set<Action, ActionCompare> &actions)
{
	os << "{";
	for (ActionSet::const_iterator i = actions.begin(); i != actions.end(); i++) {
		os << *i;
		if (++i != actions.end()) {
			os << ", ";
		}
		i--;
	}
	os << "}";
	return os;
}

}

#endif
