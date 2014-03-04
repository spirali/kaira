
#include "cailie.h"
#include "tracelog.h"
#include "output.h"

#include <unistd.h>

using namespace ca;

struct timespec TraceLog::initial_time;

TraceLog::TraceLog(int process_id, int thread_id, size_t size)
{
	buffer = (char*) malloc(size);
	// TODO: Alloc test

	pos = buffer;
	end = buffer + size;

	if (process_id == 0) {
		FILE *f = fopen("trace.kth", "w");
		if (f == NULL) {
			perror("trace.kth");
			exit(-1);
		}
		ca::write_header(f, process_count);
		fclose(f);
	}

	std::stringstream filename;
	filename << "trace-" << process_id << "-" << thread_id << ".ktt";

	file = fopen(filename.str().c_str(), "w");
	if (file == NULL) {
		perror("TraceLog::TraceLog");
		exit(-1);
	}
	setbuf(file, NULL); // Unbuffered file

	write_key_value("KairaThreadTrace", "1");
	char hostname[1024];
	if (gethostname(hostname, 1024)) {
		perror("TraceLog::TraceLog");
		exit(-1);
	}
	write_key_value("hostname", hostname);
	char tmp[200];
	snprintf(tmp, 200, "%lli%09li",
		(long long int) initial_time.tv_sec, initial_time.tv_nsec);
	write_key_value("inittime", tmp);
	write_key_value("", ""); // Terminate config section
}

TraceLog::~TraceLog()
{
	write_buffer();
	fclose(file);
	free(buffer);
}

void TraceLog::write_key_value(const std::string &key, const std::string &value)
{
	check_size(key.size() + value.size() + 2);
	write_string(key);
	write_string(value);
}

void TraceLog::event_net_spawn(int net_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t));
	write_char('S');
	write_time();
	write_int32(net_id);
}

void TraceLog::event_net_quit()
{
	check_size(1 + sizeof(uint64_t));
	write_char('Q');
	write_time();
}

void TraceLog::event_idle()
{
	check_size(1 + sizeof(uint64_t));
	write_char('I');
	write_time();
}

void TraceLog::event_transition_fired(int transition_id)
{
	check_size(1 + sizeof(uint64_t) + sizeof(int32_t));
	write_char('T');
	write_time();
	write_int32(transition_id);
}

void TraceLog::event_transition_finished_begin()
{
	check_size(1 + sizeof(uint64_t));
	write_char('F');
	write_time();
}

void TraceLog::event_end()
{
	check_size(1 + sizeof(uint64_t));
	write_char('X');
	write_time();
}

void TraceLog::event_send_part1()
{
	check_size(1 + sizeof(uint64_t));
	write_char('M');
	write_time();
}

void TraceLog::event_send_part2(int target, size_t size, int edge_id)
{
	check_size(sizeof(int32_t) * 3 + sizeof(uint64_t));
	write_uint64(size);
	write_int32(edge_id);
	write_int32(1);
	write_int32(target);
}

void TraceLog::event_send_part2(const std::vector<int> &targets, size_t size, int edge_id)
{
	check_size((sizeof(int32_t) * (targets.size() + 2)) + sizeof(uint64_t));
	write_uint64(size);
	write_int32(edge_id);
	write_int32(targets.size());
	for (size_t i = 0; i < targets.size(); i++) {
		write_int32(targets[i]);
	}
}

void TraceLog::event_receive(int from_process)
{
	check_size(1 + sizeof(int32_t) + sizeof(uint64_t));
	write_char('R');
	write_time();
	write_int32(from_process);
}

void TraceLog::trace_token_add(int place_id, void *pointer)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*));
	write_char('t');
	write_pointer(pointer);
	write_int32(place_id);
}

void TraceLog::trace_token_remove(int place_id, void *pointer)
{
	check_size(1 + sizeof(uint32_t) + sizeof(void*));
	write_char('r');
	write_pointer(pointer);
	write_int32(place_id);
}

void TraceLog::trace_value(const int value)
{
	check_size(1 + sizeof(int32_t) );
	write_char('i');
	write_int32(value);
}

void TraceLog::trace_value(const double value)
{
	check_size(1 + sizeof(double) );
	write_char('d');
	write_double(value);
}

void TraceLog::trace_value(const std::string &str)
{
	check_size(1 + str.size() + 1);
	write_char('s');
	write_string(str);
}

void TraceLog::overflow()
{
	if (pos == buffer) {
		fprintf(stderr, "Tracelog: Attempt to write too large event, set bigger size of tracelog\n");
		abort();
	}
	write_buffer();
	pos = buffer;
}

void TraceLog::write_buffer()
{
	fwrite(buffer, 1, pos - buffer, file);
}

void RealTimeTraceLog::init()
{
	if (clock_gettime(CLOCK_MONOTONIC, &initial_time)) {
		perror("TraceLog::init");
		exit(-1);
	}
}
