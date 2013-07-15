
#ifndef CAILIE_TRACER_H
#define CAILIE_TRACER_H

#include <string>
#include <vector>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "clock.h"

namespace ca {

class TraceLog {

	public:

		TraceLog(int process_id, int thread_id, size_t size);
		virtual ~TraceLog();

		void event_net_spawn(int net_id);
		void event_net_quit();
		void event_transition_fired(int transition_id);
		void event_transition_finished_begin();
		void event_end();
		void event_send_part1();
		void event_send_part2(int target, size_t size, int edge_id);
		void event_send_part2(const std::vector<int> &target, size_t size, int edge_id);
		void event_receive(int from_process);
		void event_idle();

		void trace_token_add(int place_id, void *pointer);
		void trace_token_remove(int place_id, void *pointer);

		void trace_value(const int value);
		void trace_value(const double value);
		void trace_value(const std::string &str);

		static void write_head(const std::string &name);

		static IntTime get_current_time()
		{
			struct timespec time;
			if (clock_gettime(CLOCK_MONOTONIC, &time)) {
				perror("TraceLog::get_current_time");
				exit(-1);
			}
			return time_diff(initial_time, time);
		}

		// For ControlledTracelog
		virtual void set_time(ca::IntTime time) {}
		virtual IntTime get_relative_time() { return 0; }


	protected:

		void check_size(size_t size) {
			if (pos + size >= end) { overflow(); }
		}

		void write_char(char c) {
			*(pos++) = c;
		}

		void write_uint64(uint64_t value) {
			memcpy(pos, &value, sizeof(uint64_t));
			pos += sizeof(uint64_t);
		}

		void write_int32(int32_t value) {
			memcpy(pos, &value, sizeof(int32_t));
			pos += sizeof(int32_t);
		}

		void write_pointer(void *p) {
			memcpy(pos, &p, sizeof(void*));
			pos += sizeof(void*);
		}

		void write_string(const std::string &str) {
			size_t s = str.size();
			memcpy(pos, str.data(), s);
			pos += s;
			*(pos) = 0;
			pos++;
		}

		void write_double(const double value) {
			memcpy(pos, &value, sizeof(double));
			pos += sizeof(double);
		}

		void write_key_value(const std::string &key, const std::string &value);
		void write_buffer();
		void overflow();

		virtual void write_time() = 0;

		char * buffer;
		char * pos;
		char * end;
		FILE * file;

		static struct timespec initial_time;
};

class RealTimeTraceLog : public TraceLog
{
	public:
		RealTimeTraceLog(int process_id, int thread_id, size_t size) :
			TraceLog(process_id, thread_id, size) {}
		static void init();
	protected:
		void write_time() {
			write_uint64(get_current_time());
		}
};

}



#endif // CAILIE_TRACER_H
