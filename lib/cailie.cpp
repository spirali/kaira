
#include <iostream>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>
#include <assert.h>
#include <stdarg.h>
#include "cailie.h"
#include "cailie_internal.h"

#ifndef CA_MPI

#include "cailie_threads.h"
#include "cailie_sim.h"
static std::string module_name = "threads";

#else

#include <mpi.h>
#include "cailie_mpi.h"
static std::string module_name = "mpi";

#endif // CA_MPI

NodeToProcessFn *ca_node_to_process;
int ca_process_count = 0;
const char *ca_project_description_string = NULL;
int ca_log_on = 0;

CaContext::CaContext(int node, CaProcess *process)
{
	_node = node;
	_process = process;
	_halt_flag = false;
}

void CaContext::_init(int iid, int instances, void *places, RecvFn *recv_fn, ReportFn *report_fn)
{
	_iid = iid;
	_instances = instances;
	_recv_fn = recv_fn;
	_places = places;
	_report_fn = report_fn;
}

void CaContext::quit()
{
	halt();
	_process->quit(this);
}

void CaContext::halt()
{
  _halt_flag = true;
  _process->context_halted(this);
}

size_t CaContext::_get_reserved_prefix_size() {
	return _process->get_reserved_prefix_size();
}

CaLogger * CaContext::_get_logger() {
	return _process->get_logger();
}

CaJob * CaContext::_get_jobs()
{
	std::vector<CaTransition>::iterator i = _transitions.begin();
	if (i == _transitions.end()) {
		return NULL;
	}

	CaJob *job = new CaJob(this, *i);

	for (++i; i != _transitions.end(); ++i) {
		job = new CaJob(this, *i, job);
	}

	return job;
}

void CaContext::_register_transition(int id, TransitionFn *fn)
{
	_transitions.push_back(CaTransition(id, fn));
}


bool CaContext::_find_transition(int id, CaTransition &transition)
{
	std::vector<CaTransition>::iterator i;
	for (i = _transitions.begin(); i != _transitions.end(); i++) {
		if (id == i->get_id()) {
			transition = *i;
			return true;
		}
	}
	return false;
}

/*static int ca_recv(const CaContextsMap &contexts)
{
	recv(ctx, recv_fn, data);
}*/

CaProcess::CaProcess(int process_id) : _process_id(process_id) {}

void CaProcess::init_log()
{
	if (_logger)
		return;
	_logger = new CaLogger(_process_id);

	int lines = 1;
	for (const char *c = ca_project_description_string; (*c) != 0; c++) {
		if ((*c) == '\n') {
			lines++;
		}
	}

	_logger->log("Description %i\n", lines);
	_logger->log_string(ca_project_description_string);
	_logger->log_string("\n");
	_logger->flush();
}

CaProcess::~CaProcess() {
	if (_logger) {
		delete _logger;
	}
}


void CaProcess::start_scheduler() {

	if (ca_log_on) {
		init_log();
	}

	CaContextsMap::iterator i;

	CaJob *first = NULL;
	CaJob *last = NULL;
	for (i = _contexts.begin(); i != _contexts.end(); i++) {
		CaJob *jobs = i->second->_get_jobs();
		if (jobs) {
			if (first) {
				last->next = jobs;
			} else {
				first = jobs;
				last = jobs;
			}
			while (last->next) {
				last = last->next;
			}
		}
	}

	if (first == NULL) {
		return;
	}

	for (;;) {
		CaJob *job = first;
		CaJob *prev = NULL;
		do {
			if (job->call()) {
				recv();
				if (_running_nodes == 0) {
					goto cleanup;
				}

				if (job->next != NULL) { // job is not last job
					if (prev == NULL) {
						first = job->next;
					} else {
						prev->next = job->next;
					}
					job->next = NULL;
					last->next = job;
					last = job;
				}
				job = first;
				prev = NULL;
			} else {
				prev = job;
				job = job->next;
			}
		} while(job);

		while(!recv()) { idle(); }
		if (_running_nodes == 0) {
			goto cleanup;
		}
	}

	cleanup:
	while (first) {
		CaJob *job = first->next;
		delete first;
		first = job;
	}
}

void ca_project_description(const char *str) {
	ca_project_description_string = str;
}

void ca_main(int nodes_count, InitFn *init_fn) {
	CaModule *m = NULL;
	#ifdef CA_MPI
	if (module_name == "mpi") {
		m = new CaMpiModule();
	}
	#else // CA_MPI
	if (module_name == "threads") {
		m = new CaThreadsModule();
	}
	if (module_name == "sim") {
		m = new CaSimModule();
	}
	#endif
	if (m == NULL) {
		fprintf(stderr, "Unknown module '%s'\n", module_name.c_str());
		exit(-1);
	}
	m->main(nodes_count, init_fn);
}

void ca_set_node_to_process(NodeToProcessFn *fn)
{
	ca_node_to_process = fn;
}

void ca_send(CaContext *ctx, int node, int data_id, CaPacker &packer)
{
	ctx->_get_process()->send(ctx, node, data_id, packer.get_buffer(), packer.get_size());
}

static int ca_set_argument(int params_count, const char **param_names, int **param_data, char *name, char *value)
{
	int t;
	for (t = 0; t < params_count; t++) {
		if (!strcmp(name, param_names[t])) {
			char *err = NULL;
			int i = strtol(value, &err, 10);
			if (*err != '\0') {
				printf("Invalid parameter value\n");
				exit(1);
			}
			(*param_data[t]) = i;
			return t;
		}
	}
	printf("Unknown parameter '%s'\n", name);
	exit(1);
}

void ca_parse_args(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs)
{
	size_t t;
	int c;
	struct option longopts[] = {
		{ "help",	0,	NULL, 'h' },
		{ "processes",	1,	NULL, 'r' },
		{ NULL,		0,	NULL,  0}
	};

	bool setted[params_count];
	for (t = 0; t < params_count; t++) {
		setted[t] = false;
	}

	#ifdef CA_MPI
	MPI_Init(&argc, &argv);
	#endif

	while ((c = getopt_long (argc, argv, "hp:m:r:l", longopts, NULL)) != -1)
		switch (c) {
			case 'm': {
				module_name = optarg;
			} break;
			case 'h': {
				size_t max_len = 0;
				for (t = 0; t < params_count; t++) {
					if (max_len > strlen(param_names[t])) {
						max_len = strlen(param_names[t]);
					}
				}
				for (t=0; t < params_count; t++) {
					printf("Parameters:\n");
					printf("%s", param_names[t]);
					for (size_t s = strlen(param_names[t]); s < max_len; s++) { printf(" "); }
					printf(" - %s\n", param_descs[t]);
				}
				exit(0);
			}
			case 'r': {
			      ca_process_count = atoi(optarg);
			      break;
			}
			case 'p': {
				char str[strlen(optarg) + 1];
				strcpy(str, optarg);
				char *s = str;
				while ( (*s) != 0 && (*s) != '=') { s++; }
				if ((*s) == 0) {
					printf("Invalid format of -p\n");
					exit(1);
				}
				*s = 0;
				s++;
				int r = ca_set_argument(params_count, param_names, param_data, str, s);
				setted[r] = true;
			} break;
			case 'l': {
				ca_log_on = 1;
			} break;

			case '?':
			default:
				exit(1);
			}
	bool exit_f = false;
	for (t = 0; t < params_count; t++) {
		if (!setted[t]) {
			exit_f = true;
			printf("Mandatory parameter '%s' required\n", param_names[t]);
		}
	}
	if (exit_f) { exit(1); }
}

std::string ca_int_to_string(int i)
{
	std::stringstream osstream;
	osstream << i;
	return osstream.str();
}

CaOutput::~CaOutput()
{
	while (!_stack.empty()) {
		delete _stack.top();
		_stack.pop();
	}
}

void CaOutput::child(const std::string & name)
{
	CaOutputBlock *block = new CaOutputBlock(name);
	_stack.push(block);
}

CaOutputBlock * CaOutput::back()
{
	CaOutputBlock *block = _stack.top();
	_stack.pop();
	if (!_stack.empty()) {
		CaOutputBlock *parent = _stack.top();
		parent->add_child(block);
	}
	return block;
}

static void find_and_replace(std::string &s, const char c, const std::string replace)
{
	size_t i;
	while ((i = s.find(c)) != std::string::npos)
	{
		s.replace(i, 1, replace);
	}
}

void CaOutput::set(const std::string & name, const std::string & value)
{
	std::string v = value;
	find_and_replace(v, '&', "&amp;");
	find_and_replace(v, '<', "&lt;");
	find_and_replace(v, '>', "&gt;");
	find_and_replace(v, '\n', "\\n");
	find_and_replace(v, '\t', "\\t");
	find_and_replace(v, '\r', "\\r");
	_set(name, v);
}

void CaOutput::_set(const std::string & name, const std::string & value)
{
	assert(!_stack.empty());
	CaOutputBlock *block = _stack.top();
	block->set(name, value);
}

void CaOutput::set(const std::string & name, const int value)
{
	_set(name, ca_int_to_string(value));
}

void CaOutputBlock::set(const std::string &name, const std::string & value)
{
	std::pair<std::string,std::string> p(name, value);
	_attributes.push_back(p);
}

void CaOutputBlock::write(FILE *file)
{
	fprintf(file,"<%s", _name.c_str());

	std::vector<std::pair<std::string, std::string> >::iterator i;
	for (i = _attributes.begin(); i != _attributes.end(); i++) {
		fprintf(file, " %s='%s'", (*i).first.c_str(), (*i).second.c_str());
	}

	if (_children.size() > 0) {
		fprintf(file,">");
		std::vector<CaOutputBlock*>::iterator i;
		for (i = _children.begin(); i != _children.end(); i++) {
			(*i)->write(file);
		}
		fprintf(file,"</%s>", _name.c_str());
	} else {
		fprintf(file, " />");
	}
}

CaOutputBlock::~CaOutputBlock()
{
		std::vector<CaOutputBlock*>::iterator i;
		for (i = _children.begin(); i != _children.end(); i++) {
			delete (*i);
		}
}

CaPacker::CaPacker(size_t size)
{
	buffer = (char*) malloc(size);
	//FIXME: ALLOC_TEST
	buffer_pos = buffer;
	this->size = size;
}

CaPacker::CaPacker(size_t size, size_t reserved)
{
	buffer = (char*) malloc(size + reserved);
	//FIXME: ALLOC_TEST
	buffer_pos = buffer + reserved;
	this->size = size;
}

CaLogger::CaLogger(int process_id)
{
	char str[40];
	snprintf(str, 40, "log-%i", process_id);
	file = fopen(str, "w");
	fprintf(file, "KairaLog 0.1\n");
}

CaLogger::~CaLogger()
{
	fclose(file);
}

void CaLogger::log_time()
{

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

void CaLogger::flush()
{
	fflush(file);
}

void CaLogger::log_token_add(int iid, int place_id, const std::string &token_name)
{
	fprintf(file, "A%i %i %s\n", iid, place_id, token_name.c_str());
}

void CaLogger::log_token_remove(int iid, int place_id, const std::string &token_name)
{
	fprintf(file, "R%i %i %s\n", iid, place_id, token_name.c_str());
}
