#ifndef CASIMRUN_RUNCONFIGURATION_H
#define CASIMRUN_RUNCONFIGURATION_H

#include "tracelog.h"

namespace casr {

class Context;

class RunConfiguration {
	public:
		virtual ca::IntTime packet_time(
				Context &context, int source_id, int target_id, size_t size) = 0;
		virtual ~RunConfiguration() { };

};

}

#endif // CASIMRUN_RUNCONFIGURATION_H
