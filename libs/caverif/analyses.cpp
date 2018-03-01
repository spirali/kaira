#include "statespace.h"

#include <string>

using namespace cass;

bool cfg::write_dot = false;
bool cfg::write_statespace = false;
bool cfg::analyse_deadlock = false;
bool cfg::analyse_transition_occurence = false;
bool cfg::analyse_final_marking = false;
bool cfg::analyse_cycle = false;
bool cfg::partial_order_reduction = true;
bool cfg::silent = false;
bool cfg::debug = false;
bool cfg::balance = true;
bool cfg::only_singletons = false;
size_t cfg::all_subset_max_size = 10;
std::string cfg::project_name;

static void args_callback(char c, char *optarg, void *data)
{
	if (c == 'V') {
		if (!strcmp(optarg, "dot")) {
			cfg::write_dot = true;
			return;
		}

		if (!strncmp(optarg, "debug", 5)) {
			cfg::debug = true;
			return;
		}
		if (!strcmp(optarg, "deadlock")) {
			cfg::analyse_deadlock = true;
			return;
		}
		if (!strcmp(optarg, "tchv")) {
			cfg::analyse_transition_occurence = true;
			return;
		}
		if (!strcmp(optarg, "fmarking")) {
			cfg::analyse_final_marking = true;
			return;
		}
		if (!strcmp(optarg, "cycle")) {
			cfg::analyse_cycle = true;
			return;
		}
		if (!strcmp(optarg, "disable-por")) {
			cfg::partial_order_reduction = false;
			return;
		}
		if (!strcmp(optarg, "notbalance")) {
			cfg::balance = false;
			return;
		}
		if (!strcmp(optarg, "silent")) {
			cfg::silent = true;
			return;
		}
		if (!strcmp(optarg, "only-singletons")) {
			cfg::only_singletons = true;
			return;
		}
		std::string str = "all-subset-max-size";
		if (!strncmp(optarg, str.c_str(), str.size())) {
			char *s = optarg;
			while ((*s) != 0 && (*s) != '=') { s++; }
			if ((*s) == 0) {
				fprintf(stderr, "Invalid format of -V%s\n", str.c_str());
				exit(1);
			}
			if ((*s) == '=') { s++; }

			cfg::all_subset_max_size = std::stoi(s);
			return;
		}
		if (!strcmp(optarg, "write-statespace")) {
			cfg::write_statespace = true;
			return;
		}

		fprintf(stderr, "Invalid argument in -V\n");
		exit(1);
	}
}

void cass::init(int argc, char **argv, std::vector<ca::Parameter*> &parameters, bool tracing)
{
	char s[1024];
	strncpy(s, argv[0], 1024);
	cfg::project_name = basename(s); // basename can modify its argument
	init(argc, argv, parameters, tracing, "V:", args_callback);
}

void Core::add_control_line(std::stringstream &s, const NextNodeInfo &nninfo)
{
	switch (nninfo.action) {
		case ActionFire: {
			s << nninfo.data.fire.process_id;
			s << " T ";
			ca::TransitionDef *t = net_def->get_transition_def(nninfo.data.fire.transition_id);
			if (t->get_name().size() > 0) {
				std::string name = t->get_name();
				std::replace(name.begin(), name.end(), '\n', '_');
				s << name;
			} else {
				s << "#" << nninfo.data.fire.transition_id;
			}
			break;
		}
		case ActionReceive:
			s << nninfo.data.receive.process_id;
			s << " R " << nninfo.data.receive.source_id;
			break;
	}
	s << std::endl;
}

