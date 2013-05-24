#ifndef CASIMRUN_RUNCONFIGURATION_H
#define CASIMRUN_RUNCONFIGURATION_H

#include "tracelog.h"

namespace casr {

class RunConfiguration {
	public:
		virtual ca::IntTime packet_time(int source_id, int target_id, size_t size) = 0;

};

}

#endif // CASIMRUN_RUNCONFIGURATION_H
