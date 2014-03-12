#ifndef CAVERIF_VERIFCONFIGURATION_H
#define CAVERIF_VERIFCONFIGURATION_H

#include <packing.h>
#include "action.h"

namespace cass {

struct Arc;
class State;

class VerifConfiguration {
	public:
		virtual bool compare(const Arc &arc1,const Arc &arc2) = 0;
		virtual void compute_successors(
						const Action &a,
						std::deque<Action> &queue,
						ActionSet &processed,
						const std::vector<bool> &receive_blocked,
						const std::vector<int> &enabled_priorities,
						std::vector<int> &marking) = 0;
		virtual bool is_dependent(const Action &a1, const Action &a2, const std::vector<int> &marking) = 0;
		virtual std::vector<int> get_marking(State *s) = 0;
		virtual bool is_enabled(int transition_id, int process_id, const std::vector<int> &marking, int ignored_place) = 0;
		virtual bool is_visible(const Action &a) = 0;
		virtual void pack_final_marking(ca::NetBase *net, ca::Packer &packer) = 0;
		virtual ~VerifConfiguration() {};

		bool is_transition_analyzed(int transition_id)
		{
			return ignored_transitions.find(transition_id) == \
				ignored_transitions.end();
		}

	protected:
		std::set<int> ignored_transitions;
};

}

#endif // CAVERIF_VERIFCONFIGURATION_H
