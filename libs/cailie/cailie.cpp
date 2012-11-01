
#ifdef CA_MPI
#include <mpi.h>
#endif

#include "cailie.h"
#include "listener.h"
#include "utils.h"
#include "parameters.h"

#include <getopt.h>
#include <assert.h>
#include <stdarg.h>

int ca_threads_count = 1;
const char *ca_project_description_string = NULL;
int ca_listen_port = -1;
int ca_block_on_start = 0;
CaNetDef **defs;
int defs_count = 0;
CaNet *master_net = NULL;
CaListener *ca_listener = NULL;
std::vector<CaParameter*> ca_parameters;

size_t ca_trace_log_size = 0;

#ifdef CA_SHMEM
CaProcess **processes = NULL;
int ca_process_count = 1;
#endif

#ifdef CA_MPI
CaProcess *process = NULL;
#endif


void ca_project_description(const char *str) {
	ca_project_description_string = str;
}

static void check_parameters()
{
	bool exit_flag = false;
	for (size_t t = 0; t < ca_parameters.size(); t++) {
		if (!ca_parameters[t]->check_mode_before_run()) {
			exit_flag = true;
		}
	}
	if (exit_flag) {
		exit(1);
	}
}

int ca_main()
{
	check_parameters();

	#ifdef CA_MPI
	CaServiceMessage *m = (CaServiceMessage*) alloca(sizeof(CaServiceMessage));
	m->type = CA_SM_WAKE;
	process->broadcast_packet(
		CA_TAG_SERVICE,
		m,
		sizeof(CaServiceMessage),
		process->get_thread(0),
		0);
	process->start_and_join();
	process->clear();
	MPI_Barrier(MPI_COMM_WORLD);
	#endif

	#ifdef CA_SHMEM

	if (ca_listener != NULL) {
        pthread_barrier_t start_barrier;
		ca_listener->set_processes(ca_process_count, processes);

		if (ca_block_on_start) {
			pthread_barrier_init(&start_barrier, NULL, 2);
			ca_listener->set_start_barrier(&start_barrier);
		}

		ca_listener->start();

		if (ca_block_on_start) {
			pthread_barrier_wait(&start_barrier);
			pthread_barrier_destroy(&start_barrier);
		}
	}

	for (int t = 0; t < ca_process_count; t++) {
		processes[t]->start();
	}

	for (int t = 0; t < ca_process_count; t++) {
		processes[t]->join();
	}

	for (int t = 0; t < ca_process_count; t++) {
		processes[t]->clear();
	}

	if (ca_listener != NULL) {
		delete ca_listener;
		ca_listener = NULL;
	}
	#endif

	return 0;
}

void ca_finalize()
{
	#ifdef CA_SHMEM
	if (processes) {
		for (int t = 0; t < ca_process_count; t++) {
			delete processes[t];
		}
		free(processes);
	}
	#endif

	#ifdef CA_MPI
	int process_id;
	MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
	if(process_id == 0) {
		CaServiceMessage *m =
		(CaServiceMessage *) alloca(sizeof(CaServiceMessage));
		m->type = CA_SM_EXIT;
		process->broadcast_packet(
			CA_TAG_SERVICE,
			m,
			sizeof(CaServiceMessage),
			process->get_thread(0),
			0);
	}
	MPI_Finalize();
	delete process;
	#endif

	if (defs_count > 0) {
		for (int i = 0 ; i < defs_count ; i++) {
			delete defs[i];
		}
		free(defs);
	}
}

void ca_init(int argc, char **argv, std::vector<CaParameter*> &parameters)
{
	CaTraceLog::init();
	size_t t;
	int c;
	struct option longopts[] = {
		{ "help",	0,	NULL, 'h' },
		{ "threads",	1,	NULL, 't' },
		{ NULL,		0,	NULL,  0}
	};

	atexit(ca_finalize);
	ca_parameters = parameters;

	while ((c = getopt_long (argc, argv, "hp:t:l:s:br:T:", longopts, NULL)) != -1)
		switch (c) {
			case 'h': {
				size_t max_len = 0;
				for (t = 0; t < parameters.size(); t++) {
					if (max_len > parameters[t]->get_name().size()) {
						max_len = parameters[t]->get_name().size();
					}
				}
				printf("Parameters:\n");
				for (t=0; t < parameters.size(); t++) {
					printf("%s", parameters[t]->get_name().c_str());
					for (size_t s = parameters[t]->get_name().size();
							s < max_len; s++) { printf(" "); }
					printf(" - %s\n", parameters[t]->get_description().c_str());
				}
				exit(0);
			}
			case 't': {
			      ca_threads_count = atoi(optarg);
			      break;
			}
			case 'r': {
					#ifdef CA_MPI
					fprintf(stderr, "Argument -r is not allowed for MPI backend, please use `mpirun -np X`");
					exit(-1);
					#else
					ca_process_count = atoi(optarg);
					break;
					#endif
			}
			case 'p': {
				char str[strlen(optarg) + 1];
				strcpy(str, optarg);
				char *s = str;
				while ( (*s) != 0 && (*s) != '=') { s++; }
				if ((*s) == 0) {
					fprintf(stderr, "Invalid format of -p\n");
					exit(1);
				}
				*s = 0;
				s++;
				ca_set_parameter(parameters, str, s);
			} break;
			case 's': {
				if (!strcmp(optarg, "auto")) {
					ca_listen_port = 0;
					break;
				}
				ca_listen_port = atoi(optarg);
				if (ca_listen_port == 0) {
					fprintf(stderr, "Invalid value for -s\n");
					exit(1);
				}
				break;
			}
			case 'b': {
				ca_block_on_start = 1;
			} break;

			case 'T': {
				ca_trace_log_size = ca_parse_size_string(optarg);
				if (ca_trace_log_size == 0) {
					fprintf(stderr, "Invalid trace log size\n");
					exit(1);
				}
			} break;
			case '?':
			default:
				exit(1);
			}

	#ifdef CA_MPI
	int provided;
	int target;
	if (ca_threads_count == 1) {
		target = MPI_THREAD_SINGLE;
	} else {
		target = MPI_THREAD_MULTIPLE;
	}

	MPI_Init_thread(&argc, &argv, target, &provided);
	if (target > provided) {
		fprintf(stderr, "MPI_Init_thread: Insufficient support of threads in MPI implementaion.\n"
			"This program can be run with one thread per MPI process or\n"
			"MPI implementation has to support MPI_THREAD_MULTIPLE.\n");
		exit(1);
	}
	#endif

	#ifdef CA_SHMEM
	if (ca_listen_port != -1) {
		ca_listener = new CaListener();
		ca_listener->init(ca_listen_port);
		if (ca_listen_port == 0) {
			printf("%i\n", ca_listener->get_port());
			fflush(stdout);
		}
		if (ca_block_on_start) {
			ca_listener->wait_for_connection();
		}
	}
	#endif
}

void ca_setup(int _defs_count, CaNetDef **_defs)
{
	defs_count = _defs_count;
	defs = (CaNetDef**) malloc(sizeof(CaNetDef*) * defs_count);
	memcpy(defs, _defs, sizeof(CaNetDef*) * defs_count);

	#ifdef CA_MPI
		int process_count, process_id;
		MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
		MPI_Comm_size(MPI_COMM_WORLD, &process_count);
		process = new CaProcess(process_id, process_count, ca_threads_count, defs_count, defs);
		if(process_id > 0){
			while(true){
				process->wait();
			}
		}
	#endif

	#ifdef CA_SHMEM
	processes = (CaProcess**) malloc(sizeof(CaProcess*) * ca_process_count);
	for (int t = 0; t < ca_process_count; t++) {
		processes[t] = new CaProcess(t, ca_process_count, ca_threads_count, defs_count, defs);
	}
	#endif
}


void ca_spawn_net(int def_id)
{
	#ifdef CA_SHMEM
	for (int t = 0; t < ca_process_count; t++) {
		CaNet *net = processes[t]->spawn_net(processes[t]->get_thread(0), def_id, false);
		net->unlock();
		if (t == 0) {
			master_net = net;
		}
	}
	#endif

	#ifdef CA_MPI
	CaThread *thread = process->get_thread(0);
	CaNet *net = process->spawn_net(thread, def_id, true);
	net->unlock();
	master_net = net;
	#endif
}

CaNet * ca_get_main_net()
{
	return master_net;
}

CaProcess * ca_get_first_process()
{
	#ifdef CA_MPI
	return process;
	#endif

	#ifdef CA_SHMEM
	return processes[0];
	#endif

}

std::vector<int> ca_range(int from, int upto)
{
	std::vector<int> v;
	int t;
	for (t = from; t <= upto; t++) {
		v.push_back(t);
	}
	return v;
}

void ca_write_header(FILE *out, int process_count, int threads_count)
{
	int lines = 1;
	for (const char *c = ca_project_description_string; (*c) != 0; c++) {
		if ((*c) == '\n') {
			lines++;
		}
	}

	CaOutput output;
	output.child("header");
	output.set("pointer-size", (int) sizeof(void*));
	output.set("process-count", process_count);
	output.set("threads-count", threads_count);
	output.set("description-lines", lines);
	CaOutputBlock *block = output.back();
	block->write(out);
	delete block;
	fputs("\n", out);
	fputs(ca_project_description_string, out);
	fputs("\n", out);
}

size_t ca_hash_string(std::string &v) {
	std::string::const_iterator i;
	size_t r = 37 * v.size();
    int j = 0;
	for (i = v.begin(); i != v.end() && j < 256; i++, j++) {
		r = r * 101 + (*i);
	}
	return r;
}

size_t ca_hash(void *v, size_t size, size_t h) {
	char *vv = (char *) v;
	for (size_t t = 0; t < size; t++) {
		h = h * 101 + vv[t];
	}
	return h;
}

size_t ca_hash_double(double v) {
	return ca_hash(&v, sizeof(double));
}

size_t ca_hash_float(float v) {
	return ca_hash(&v, sizeof(float));
}
