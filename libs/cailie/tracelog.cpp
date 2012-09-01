
#include "tracelog.h"
#include "output.h"

#include <stdlib.h>

struct timespec CaTraceLog::initial_time;

void CaTraceLog::init()
{
	if (clock_gettime(CLOCK_MONOTONIC, &initial_time)) {
		perror("CaTraceLog::init");
		exit(-1);
	}
}

CaTraceLog::CaTraceLog(size_t size, const std::string &filename)
{
	buffer = (char*) malloc(size);
	pos = buffer;
	end = buffer + size;

	file = fopen(filename.c_str(), "w");
	if (file == NULL) {
		perror("CaTraceLog::CaTraceLog");
		exit(-1);
	}
	setbuf(file, NULL); // Unbuffered file
}

CaTraceLog::~CaTraceLog()
{
	write_buffer();
	fclose(file);
	free(buffer);
}

void CaTraceLog::write_time()
{
	struct timespec time;

	if (clock_gettime(CLOCK_MONOTONIC, &time)) {
		perror("CaTraceLog::write_time");
		exit(-1);
	}

	uint64_t t = (time.tv_sec - initial_time.tv_sec) * 10e9;
	t += time.tv_nsec - initial_time.tv_nsec;
	write_uint64(t);
}

void CaTraceLog::event_net_spawn(int net_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t));
	write_char('S');
	write_time();
	write_int32(net_id);
}

void CaTraceLog::event_net_halt()
{
	check_size(1 + sizeof(uint64_t));
	write_char('H');
	write_time();
}

void CaTraceLog::event_transition_fired(int transition_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t));
	write_char('T');
	write_time();
	write_int32(transition_id);
}

void CaTraceLog::event_transition_finished()
{
	check_size(1 + sizeof(uint64_t));
	write_char('F');
	write_time();
}

void CaTraceLog::event_receive()
{
	check_size(1 + sizeof(uint64_t));
	write_char('R');
	write_time();
}

void CaTraceLog::trace_token_add(int place_id, void *pointer)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*));
	write_char('t');
	write_int32(place_id);
	write_pointer(pointer);
}

void CaTraceLog::trace_token_remove(int place_id, void *pointer)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*));
	write_char('r');
	write_int32(place_id);
	write_pointer(pointer);
}

void CaTraceLog::trace_int(const int value)
{
	check_size(1 + sizeof(uint32_t) );
	write_char('i');
	write_int32(value);
}

void CaTraceLog::trace_double(const double value)
{
	check_size(1 + sizeof(double) );
	write_char('d');
	write_double(value);
}

void CaTraceLog::trace_string(const std::string &str)
{
	check_size(1 + str.size() + 1);
	write_char('s');
	write_string(str);
}

void CaTraceLog::overflow()
{
	if (pos == buffer) {
		fprintf(stderr, "Tracelog: Attempt to write too large event, set bigger size of tracelog\n");
		abort();
	}
	write_buffer();
	pos = buffer;
}

void CaTraceLog::write_buffer()
{
	fwrite(buffer, 1, pos - buffer, file);
}
