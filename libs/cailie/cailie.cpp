
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

namespace ca {

int process_count = 1;
const char *project_description_string = NULL;
int listen_port = -1;
int block_on_start = 0;
NetDef **defs;
int defs_count = 0;
Net *master_net = NULL;
Listener *listener = NULL;
std::vector<Parameter*> parameters;

size_t tracelog_size = 0;

#ifdef CA_SHMEM
Process **processes = NULL;
bool sequential_run = false;
#endif

#ifdef CA_MPI
Process *process = NULL;
#endif

}

using namespace ca;

void ca::project_description(const char *str) {
	project_description_string = str;
}

void ca::check_parameters()
{
	bool exit_flag = false;
	for (size_t t = 0; t < parameters.size(); t++) {
		if (!parameters[t]->check_mode_before_run()) {
			exit_flag = true;
		}
	}
	if (exit_flag) {
		exit(1);
	}
}

int ca::main()
{

	#ifdef CA_MPI
	ServiceMessage *m = (ServiceMessage*) malloc(sizeof(ServiceMessage));
	m->type = CA_SM_WAKE;
	process->broadcast_packet(
		CA_TAG_SERVICE,
		m,
		sizeof(ServiceMessage),
		0);
	process->start(false);
	process->clear();
	MPI_Barrier(MPI_COMM_WORLD);
	#endif

	#ifdef CA_SHMEM

	if (listener != NULL) {
        pthread_barrier_t start_barrier;
		listener->set_processes(process_count, processes);

		if (block_on_start) {
			pthread_barrier_init(&start_barrier, NULL, 2);
			listener->set_start_barrier(&start_barrier);
		}

		listener->start();

		if (block_on_start) {
			pthread_barrier_wait(&start_barrier);
			pthread_barrier_destroy(&start_barrier);
		}
	}

	if (sequential_run) {
		bool quit = false;
		while(!quit) {
			for (int t = 0; t < process_count; t++) {
				Thread *thread = processes[t]->get_thread();
				thread->run_one_step();
				if (processes[t]->quit_flag) {
					quit = true;
					break;
				}
			}
		}
	} else { // Normal run
		for (int t = 0; t < process_count; t++) {
			processes[t]->start(true);
		}

		for (int t = 0; t < process_count; t++) {
			processes[t]->join();
		}

		for (int t = 0; t < process_count; t++) {
			processes[t]->clear();
		}
	}

	if (listener != NULL) {
		delete listener;
		listener = NULL;
	}
	#endif

	return 0;
}

static void finalize()
{
	#ifdef CA_SHMEM
	if (processes) {
		for (int t = 0; t < process_count; t++) {
			delete processes[t];
		}
		free(processes);
	}
	#endif

	#ifdef CA_MPI
	int process_id;
	MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
	if(process_id == 0) {
		ServiceMessage *m =
		(ServiceMessage *) malloc(sizeof(ServiceMessage));
		m->type = CA_SM_EXIT;
		process->broadcast_packet(
			CA_TAG_SERVICE,
			m,
			sizeof(ServiceMessage),
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
	if (listener != NULL) {
		delete listener;
	}
}

void ca::init(int argc,
			 char **argv,
			 std::vector<Parameter*> &parameters,
			 bool tracing,
			 const std::string &extra_args,
			 void (extra_args_callback)(char, char*, void*),
			 void *extra_args_data)
{
	RealTimeTraceLog::init();
	size_t t;
	int c;
	struct option longopts[] = {
		{ "help",	0,	NULL, 'h' },
		{ NULL,		0,	NULL,  0}
	};

	atexit(finalize);
	ca::parameters = parameters;
	std::string all_args = std::string("hp:t:l:s:br:T:S") + extra_args;
	while ((c = getopt_long (argc, argv, all_args.c_str(), longopts, NULL)) != -1)
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
			case 'r': {
					#ifdef CA_MPI
					fprintf(stderr,
							"Argument -r is not allowed for MPI backend, "
							"use `mpirun -np %s`\n",
							argv[0]);
					exit(-1);
					#else
					process_count = atoi(optarg);
					if (process_count < 1) {
						fprintf(stderr, "Invalid number of processes\n");
						exit(-1);
					}
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
				set_parameter(parameters, str, s);
			} break;
			case 's': {
				if (!strcmp(optarg, "auto")) {
					listen_port = 0;
					break;
				}
				listen_port = atoi(optarg);
				if (listen_port == 0) {
					fprintf(stderr, "Invalid value for -s\n");
					exit(1);
				}
				break;
			}
			case 'b': {
				block_on_start = 1;
			} break;

			case 'T': {
				tracelog_size = parse_size_string(optarg);
				if (tracelog_size == 0) {
					fprintf(stderr, "Invalid trace log size\n");
					exit(1);
				}
			} break;

			case 'S': {
				#ifdef CA_MPI
				fprintf(stderr, "Sequential run is not possible for the MPI version\n");
				exit(1);
				#endif
				#ifdef CA_SHMEM
				sequential_run = true;
				#endif
			} break;

			case '?':
			default:
				if (extra_args_callback != NULL) {
					extra_args_callback(c, optarg, extra_args_data);
				} else {
					exit(1);
				} break;
			}

	if (tracing && tracelog_size == 0) {
		fprintf(stderr, "Size of tracelog buffer was not specified.\n"
				"Use parameter -T; e.g. -T2M for 2MB tracelog buffer.\n");
		exit(1);
	}

	#ifdef CA_MPI
	MPI_Init(&argc, &argv);
	MPI_Comm_size(MPI_COMM_WORLD, &process_count);
	#endif

	#ifdef CA_SHMEM
	Process::init_collective_operations(process_count);
	if (listen_port != -1) {
		listener = new Listener();
		listener->init(listen_port);
		if (listen_port == 0) {
			printf("%i\n", listener->get_port());
			fflush(stdout);
		}
		if (block_on_start) {
			listener->wait_for_connection();
		}
	}
	#endif
}

void ca::setup(int _defs_count, NetDef **_defs, bool start_process)
{
	defs_count = _defs_count;
	defs = (NetDef**) malloc(sizeof(NetDef*) * defs_count);
	memcpy(defs, _defs, sizeof(NetDef*) * defs_count);

	if (start_process) {
		#ifdef CA_MPI
			int process_id;
			MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
			process = new Process(process_id, process_count, defs_count, defs);
			if(process_id > 0){
				while(true){
					process->wait();
				}
			}
		#endif

		#ifdef CA_SHMEM
		processes = (Process**) malloc(sizeof(Process*) * process_count);
		for (int t = 0; t < process_count; t++) {
			processes[t] = new Process(t, process_count, defs_count, defs);
		}
		#endif
	}
}


void ca::spawn_net(int def_id)
{
	check_parameters();

	#ifdef CA_SHMEM
	for (int t = 0; t < process_count; t++) {
		Net *net = processes[t]->spawn_net(def_id, false);
		if (t == 0) {
			master_net = net;
		}
	}
	#endif

	#ifdef CA_MPI
	master_net = process->spawn_net(def_id, true);
	#endif
}

Net * ca::get_main_net()
{
	return master_net;
}

Process * ca::get_first_process()
{
	#ifdef CA_MPI
	return process;
	#endif

	#ifdef CA_SHMEM
	return processes[0];
	#endif
}

void ca::write_header(FILE *out, int process_count)
{
	int lines = 1;
	for (const char *c = project_description_string; (*c) != 0; c++) {
		if ((*c) == '\n') {
			lines++;
		}
	}

	Output output(out);
	output.child("header");
	output.set("pointer-size", (int) sizeof(void*));
	output.set("process-count", process_count);
	output.set("description-lines", lines);
	output.back();
	fputs("\n", out);
	fputs(project_description_string, out);
	fputs("\n", out);
}
