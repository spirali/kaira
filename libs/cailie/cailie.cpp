
#include "cailie.h"
#include "listener.h"

#include <getopt.h>
#include <assert.h>
#include <stdarg.h>

#ifdef CA_MPI
#include <mpi.h>
#endif

int ca_threads_count = 1;
const char *ca_project_description_string = NULL;
int ca_listen_port = -1;
int ca_block_on_start = 0;
CaNetDef **defs;
int defs_count;
CaNet *master_net = NULL;

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

static CaListener * ca_init_listener(int process_count, CaProcess **processes)
{
	CaListener *listener = new CaListener(process_count, processes);
	pthread_barrier_t start_barrier;

	listener->init(ca_listen_port);
	if (ca_listen_port == 0) {
		printf("%i\n", listener->get_port());
		fflush(stdout);
	}

	if (ca_block_on_start) {
		pthread_barrier_init(&start_barrier, NULL, 2);
		listener->set_start_barrier(&start_barrier);
	}

	listener->start();

	if (ca_block_on_start) {
		pthread_barrier_wait(&start_barrier);
		pthread_barrier_destroy(&start_barrier);
	}

	return listener;
}

int ca_main()
{
	#ifdef CA_MPI
	CaServiceMessage *m = (CaServiceMessage*) alloca(sizeof(CaServiceMessage));
	m->type = CA_SM_WAKE;
	process->broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessage), process->get_thread(0), 0);
	process->start_and_join();
	process->clear();
	MPI_Barrier(MPI_COMM_WORLD);
	#endif

	#ifdef CA_SHMEM
	CaListener *listener = NULL;

	if (ca_listen_port != -1) {
		listener = ca_init_listener(ca_process_count, processes);
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

	if (listener != NULL) {
		delete listener;
	}
	#endif

	return 0;
}

static int ca_set_argument(int params_count, const char **param_names, int **param_data, char *name, char *value)
{
	int t;
	for (t = 0; t < params_count; t++) {
		if (!strcmp(name, param_names[t])) {
			char *err = NULL;
			int i = strtol(value, &err, 10);
			if (*err != '\0') {
				fprintf(stderr, "Invalid parameter value\n");
				exit(1);
			}
			(*param_data[t]) = i;
			return t;
		}
	}
	fprintf(stderr, "Unknown parameter '%s'\n", name);
	exit(1);
}

void ca_finalize()
{
	#ifdef CA_SHMEM
	for (int t = 0; t < ca_process_count; t++) {
		delete processes[t];
	}
	free(processes);
	#endif

	#ifdef CA_MPI
	int process_id;
	MPI_Comm_rank(MPI_COMM_WORLD, &process_id);
	if(process_id == 0) {
		CaServiceMessage *m =
		(CaServiceMessage *) alloca(sizeof(CaServiceMessage));
		m->type = CA_SM_EXIT;
		process->broadcast_packet(CA_TAG_SERVICE, m, sizeof(CaServiceMessage), process->get_thread(0), 0);
	}
	MPI_Finalize();
	delete process;
	#endif

	for (int i = 0 ; i < defs_count ; i++) {
		delete defs[i];
	}
	free(defs);
}

void ca_init(int argc, char **argv, size_t params_count, const char **param_names, int **param_data, const char **param_descs)
{
	size_t t;
	int c;
	struct option longopts[] = {
		{ "help",	0,	NULL, 'h' },
		{ "threads",	1,	NULL, 't' },
		{ NULL,		0,	NULL,  0}
	};

	bool setted[params_count];
	for (t = 0; t < params_count; t++) {
		setted[t] = false;
	}

	atexit(ca_finalize);

	while ((c = getopt_long (argc, argv, "hp:t:l:s:br:", longopts, NULL)) != -1)
		switch (c) {
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
				int r = ca_set_argument(params_count, param_names, param_data, str, s);
				setted[r] = true;
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
			case '?':
			default:
				exit(1);
			}
	bool exit_f = false;
	for (t = 0; t < params_count; t++) {
		if (!setted[t]) {
			exit_f = true;
			fprintf(stderr, "Mandatory parameter '%s' required\n", param_names[t]);
		}
	}
	if (exit_f) { exit(1); }

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


void ca_spawn_toplevel_net(int def_id)
{
	#ifdef CA_SHMEM
	int net_id = processes[0]->new_net_id();
	for (int t = 0; t < ca_process_count; t++) {
		CaNet *net = processes[t]->spawn_net(processes[t]->get_thread(0), def_id, net_id, NULL, false);
		net->unlock();
		if (t == 0) {
			master_net = net;
		}
	}
	#endif

	#ifdef CA_MPI
	CaThread *thread = process->get_thread(0);
	CaNet *net = process->spawn_net(thread, def_id, process->new_net_id(), NULL, true);
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
