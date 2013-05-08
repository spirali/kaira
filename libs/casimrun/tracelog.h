
#include <tracelog.h>

namespace casr {
	class ControlledTimeTraceLog : public ca::TraceLog {
		public:
			ControlledTimeTraceLog(int process_id, int thread_id, size_t size);
			void set_basetime(ca::IntTime value) { basetime = value; }
			void set_end_time(ca::IntTime value) { force_end_time = value; }

			void set_realtime_mode() {
				force_end_time = ca::MAX_INT_TIME;
			}

			static void init();
		protected:
			void write_time();
			ca::IntTime basetime;
			ca::IntTime starttime;

			ca::IntTime force_end_time;
	};
}
