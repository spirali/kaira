
#include "tracelog.h"

using namespace casr;

void ControlledTimeTraceLog::init()
{
	initial_time.tv_sec = 0;
	initial_time.tv_nsec = 0;
}

ControlledTimeTraceLog::ControlledTimeTraceLog(int process_id, int thread_id, size_t size)
	: TraceLog(process_id, thread_id, size), basetime(0), starttime(0)
{
	settedtime = 0;
}

void ControlledTimeTraceLog::write_time()
{
	char c = *(pos - 1);
	switch(c) {
		case 'T': // Transition fire
		case 'R': // Receive
			/*starttime = get_current_time();
			write_uint64(basetime);
			return;*/
		case 'F':
		case 'M':
		case 'X':
		{
			ca::IntTime t = get_current_time();
			if (settedtime == ca::MAX_INT_TIME) {
				basetime += t - starttime;
			} else {
			    basetime += settedtime;
				settedtime = ca::MAX_INT_TIME;
			}
			starttime = t;
			write_uint64(basetime);
			return;
		}
		case 'I': // Receive
			write_uint64(basetime);
			return;
		case 'S': // Spawn
			write_uint64(0);
			return;
		default:
			printf("ControlledTimeTraceLog: Internal error\n");
			exit(-1);
	}
}
