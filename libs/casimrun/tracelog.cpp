
#include "tracelog.h"

using namespace casr;

void ControlledTimeTraceLog::init()
{
	initial_time.tv_sec = 0;
	initial_time.tv_nsec = 0;
}

ControlledTimeTraceLog::ControlledTimeTraceLog(int process_id, int thread_id, size_t size)
	: TraceLog(process_id, thread_id, size)
{
}

void ControlledTimeTraceLog::write_time()
{
	char c = *(pos - 1);
	switch(c) {
		case 'T': // Transition fire
		case 'R': // Receive
			starttime = get_current_time();
			write_uint64(basetime);
			return;
		case 'I': // Receive
			write_uint64(basetime);
			return;
		case 'F':
		case 'M':
		case 'X':
			write_uint64(basetime + get_current_time() - starttime);
			return;
		case 'S': // Spawn
			write_uint64(0);
			return;
		default:
			printf("ControlledTimeTraceLog: Internal error\n");
			exit(-1);
	}
}
