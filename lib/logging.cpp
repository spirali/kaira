
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

void CaLogger::log_token_add(int iid, int place_id, const std::string &token_name)
{
	fprintf(file, "A%i %i %s\n", iid, place_id, token_name.c_str());
	fflush(file);
}

void CaLogger::log_token_remove(int iid, int place_id, const std::string &token_name)
{
	fprintf(file, "R%i %i %s\n", iid, place_id, token_name.c_str());
}

void CaLogger::log_transition_start(int iid, int transition_id)
{
	log_time();
	fprintf(file, "S%i %i\n", iid, transition_id);
	flush();
}

void CaLogger::log_transition_end(int iid, int transition_id)
{
	log_time();
	fprintf(file, "E%i %i\n", iid, transition_id);
	flush();
}

void CaLogger::log_receive()
{
	log_time();
	fputs("C\n", file);
}
