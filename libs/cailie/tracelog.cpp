
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

	if (filename != "") {
		file = fopen(filename.c_str(), "w");
		if (file == NULL) {
			perror("CaTraceLog::CaTraceLog");
			exit(-1);
		}
		setbuf(file, NULL); // Unbuffered file
	} else {
		file = NULL;
	}
	// TODO: Alloc check
}

CaTraceLog::~CaTraceLog()
{
	if (file) {
		write_buffer();
		fclose(file);
	}
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

void CaTraceLog::event_net_spawn(int net_id, int instance_id, int parent_net)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t) * 3);
	write_char('S');
	write_time();
	write_int32(net_id);
	write_int32(instance_id);
	write_int32(parent_net);
}

void CaTraceLog::event_transition_fired(int instance_id, int transition_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t) * 2 + 1);
	write_char('T');
	write_time();
	write_int32(instance_id);
	write_int32(transition_id);
	write_char(0);
}

void CaTraceLog::event_transition_finished()
{
	check_size(1 + sizeof(uint64_t));
	write_char('F');
	write_time();
}

void CaTraceLog::event_receive(int instance_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t));
	write_char('R');
	write_time();
	write_int32(instance_id);
}

void CaTraceLog::trace_token(int place_id, void *pointer, const std::string &value)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*) + 1 + value.size());
	write_char('t');
	write_int32(place_id);
	write_pointer(pointer);
	write_string(value);
}

void CaTraceLog::trace_token(int place_id, void *pointer)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*));
	write_char('s');
	write_int32(place_id);
	write_pointer(pointer);
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
