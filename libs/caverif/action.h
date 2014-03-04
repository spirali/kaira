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

	std::string to_string() const {
		std::ostringstream ss;
		switch (type) {
			case ActionFire: {
				ss << "<F " << process << " " << data.fire.transition_def->get_id() << ">";
			} break;
			case ActionReceive: {
				ss << "<R " << process << " " << data.receive.source << " " << data.receive.edge_id << ">";
			} break;
		}
		return ss.str();
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

}

#endif
