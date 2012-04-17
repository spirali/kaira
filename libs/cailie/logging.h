
#ifndef CAILIE_LOGGING_H
#define CAILIE_LOGGING_H

#include <stdio.h>
#include <string>
#include "output.h"

class CaLogger {
	public:
		CaLogger(const std::string &logname, int process_id);
		~CaLogger();

		void log_place_changes_begin();
		void log_place_changes_end();
		void log(const char *form, ...);
		void log_string(const std::string &str);
		void log_int(int i);
	/*	void log_token_add(CaUnit *unit, int place_id, const std::string &token_name);
		void log_token_remove(CaUnit *unit, int place_id, const std::string &token_name);
		void log_transition_start(CaUnit *unit, int transition_id);
		void log_transition_end(CaUnit *unit, int transition_id);*/
		void log_receive();
		void flush();
		void write(CaOutputBlock *block);
		void log_time();
		FILE * get_file() { return file; }
	protected:
		FILE *file;
};

#endif
