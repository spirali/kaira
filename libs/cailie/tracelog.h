
#ifndef CAILIE_TRACER_H
#define CAILIE_TRACER_H

#include <string>
#include <time.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>


class CaTraceLog {

	public:

		CaTraceLog(size_t size, const std::string &filename);
		~CaTraceLog();

		void event_net_spawn(int net_id);
		void event_net_halt();
		void event_transition_fired(int transition_id);
		void event_transition_finished();
		void event_receive();

		void trace_token(int place_id, void *pointer, const std::string &value);
		void trace_token(int place_id, void *pointer);

		static void init();
		static void write_head(const std::string &name);
	protected:

		void check_size(size_t size) { if (pos + size >= end) { overflow(); } }
		void write_char(char c) { *(pos++) = c; }

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


		void write_buffer();

		void write_time();
		void overflow();

		char * buffer;
		char * pos;
		char * end;

		FILE * file;

		static struct timespec initial_time;

};

#endif // CAILIE_TRACER_H
