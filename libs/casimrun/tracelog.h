#ifndef CASIMRUN_TRACELOG_H
#define CASIMRUN_TRACELOG_H

#include <tracelog.h>

namespace casr {
	class ControlledTimeTraceLog : public ca::TraceLog {
		public:
			ControlledTimeTraceLog(int process_id, int thread_id, size_t size);

			void set_basetime(ca::IntTime value) {
				basetime = value;
				settedtime = 0;
			}

			void set_time(ca::IntTime value) {
				settedtime = value;
			}

			ca::IntTime get_time() {
				return basetime;
			}

			ca::IntTime get_relative_time() {
				return get_current_time() - starttime;
			}

			static void init();
		protected:
			void write_time();
			ca::IntTime basetime;
			ca::IntTime starttime;
			ca::IntTime settedtime;
	};
}

#endif // CASIMRUN_TRACELOG_H
