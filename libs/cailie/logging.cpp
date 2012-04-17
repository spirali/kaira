
#include <stdarg.h>
#include "logging.h"

CaLogger::CaLogger(const std::string &logname, int log_id)
{
	char str[40];
	snprintf(str, 40, "%s.%i", logname.c_str(), log_id);
	file = fopen(str, "w");
	fprintf(file, "KairaLog 0.2\n");
}

CaLogger::~CaLogger()
{
	fclose(file);
}

void CaLogger::log_time()
{
	  struct timespec time;
	  clock_gettime(CLOCK_MONOTONIC, &time);
	  fprintf(file, "%ld.%ld\n", time.tv_sec, time.tv_nsec);
	  // TODO: Detect by configure size of time_t and availability of CLOCK_MONOTONIC
}

void CaLogger::log_string(const std::string &str)
{
	fputs(str.c_str(), file);
}

void CaLogger::log_int(int i)
{
	fprintf(file, "%i", i);
}

void CaLogger::log(const char *form, ...) {
	va_list arg;
	va_start(arg,form);
	vfprintf(file, form, arg);
	va_end(arg);
}

void CaLogger::write(CaOutputBlock *block) {
	block->write(file);
}

void CaLogger::flush()
{
	fflush(file);
}

/*
void CaLogger::log_token_add(CaUnit *unit, int place_id, const std::string &token_name)
{
	fprintf(file, "A%s %i %s\n", unit->path.as_string().c_str(), place_id, token_name.c_str());
	fflush(file);
}

void CaLogger::log_token_remove(CaUnit *unit, int place_id, const std::string &token_name)
{
	fprintf(file, "R%s %i %s\n", unit->path.as_string().c_str(), place_id, token_name.c_str());
}

void CaLogger::log_transition_start(CaUnit *unit, int transition_id)
{
	log_time();
	fprintf(file, "S%s %i\n", unit->path.as_string().c_str(), transition_id);
	flush();
}

void CaLogger::log_transition_end(CaUnit *unit, int transition_id)
{
	log_time();
	fprintf(file, "E%s %i\n", unit->path.as_string().c_str(), transition_id);
	flush();
}
*/

void CaLogger::log_receive()
{
	log_time();
	fputs("C\n", file);
}
