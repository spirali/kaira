#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sched.h>

#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>

#include "cailie.h"
#include "cailie_internal.h"
#include "cailie_threads.h"

#define HALT_COMMAND -1
#define START_LOG_COMMAND -2
#define STOP_LOG_COMMAND -3
#define BARRIERS_COMMAND -4

struct CaThreadsPacket {
	int target_node;
	int data_id;
	size_t size;
	CaThreadsPacket *next;
};

struct CaThreadsData {
	CaThreadsProcess *process;
	InitFn *init_fn;
};

struct CaThreadsBarriers {
	pthread_barrier_t *barrier1;
	pthread_barrier_t *barrier2;
	pthread_barrier_t *barrier3;
};

struct CaThreadsListenData {
	CaThreadsProcess *process;
	int listen_socket;
	pthread_barrier_t *start_barrier;
};

CaThreadsProcess::CaThreadsProcess(CaThreadsModule *module, int process_id) :
  CaProcess(process_id), _module(module), _packet(NULL) {
		pthread_mutex_init(&_lock, NULL);
}

CaThreadsProcess::~CaThreadsProcess()
{
    pthread_mutex_destroy(&_lock);
}

size_t CaThreadsProcess::get_reserved_prefix_size()
{
	return sizeof(CaThreadsPacket);
}

void CaThreadsProcess::queue_add(CaThreadsPacket *packet)
{
	pthread_mutex_lock(&_lock);
	packet->next = _packet;
	_packet = packet;
	pthread_mutex_unlock(&_lock);
}

void CaThreadsProcess::send(CaContext *ctx, int target, int data_id, void *data, size_t size)
{
	CaThreadsPacket *packet = (CaThreadsPacket *) data;
	packet->target_node = target;
	packet->data_id = data_id;
	packet->size = size;
	_module->get_process(ca_node_to_process(target))->queue_add(packet);
}

void CaThreadsProcess::send_to_all(CaContext *ctx, int data_id, const void *data, size_t size)
{
	int t;
	int source = ctx?ctx->node():-1;
	size_t prefix = get_reserved_prefix_size();
 	for (t=0; t < _module->get_nodes_count(); t++) {
		if (t == source) {
			continue; // Don't send the message to self
		}
		char *d = (char*) malloc(prefix + size);
		// ALLOCTEST
		memcpy(d + prefix, data, size);
		send(ctx, t, data_id, d, size);
	}
}

void CaThreadsProcess::send_to_all_processes(int data_id, const void *data, size_t size)
{
	int t;
	size_t prefix = get_reserved_prefix_size();
 	for (t=0; t < ca_process_count; t++) {
		CaThreadsPacket *packet = (CaThreadsPacket *) malloc(prefix + size);
		// ALLOCTEST
		memcpy(packet + 1, data, size);
		packet->target_node = -1;
		packet->data_id = data_id;
		packet->size = size;
		_module->get_process(t)->queue_add(packet);
	}
}

void CaThreadsProcess::quit(CaContext *ctx)
{
	int dummy;
	send_to_all(ctx, HALT_COMMAND, &dummy, 0);
}


void CaThreadsProcess::start_logging(CaContext *ctx, const std::string& logname)
{
	init_log(logname);
	send_to_all(ctx, START_LOG_COMMAND, logname.c_str(), logname.size() + 1);
}

void CaThreadsProcess::stop_logging(CaContext *ctx)
{
	stop_log();
	int dummy;
	send_to_all(ctx, STOP_LOG_COMMAND, &dummy, 0);
}

void CaThreadsProcess::add_context(CaContext* ctx)
{
    _contexts[ctx->node()] = ctx;
}

void CaThreadsProcess::start(InitFn *init_fn)
{
	CaContextsMap::iterator i;
	for (i = _contexts.begin(); i != _contexts.end(); ++i) {
	     init_fn(i->second);
	}
	_running_nodes = _contexts.size();
	start_scheduler();
}

static void * ca_threads_starter(void *data)
{
	CaThreadsData *d = (CaThreadsData*) data;
	d->process->start(d->init_fn);
	return NULL;
}

void CaThreadsProcess::process_packets(CaThreadsPacket *packet)
{
	while(packet) {
		int node = packet->target_node;
		if (packet->data_id < 0) {
			if (packet->data_id == HALT_COMMAND) {
				_module->get_context(node)->halt();
			} else if (packet->data_id == START_LOG_COMMAND) {
				init_log((char*) (packet + 1));
			} else if (packet->data_id == STOP_LOG_COMMAND) {
				stop_log();
			} else if (packet->data_id == BARRIERS_COMMAND) {
				CaThreadsBarriers *barriers = (CaThreadsBarriers*) (packet + 1);
				process_packets(packet->next);
				pthread_barrier_wait(barriers->barrier1);
				recv();
				pthread_barrier_wait(barriers->barrier2);
				pthread_barrier_wait(barriers->barrier3);
				free(packet);
				return;
			}
		} else {
			_module->get_context(node)->_call_recv_fn(packet->data_id, packet + 1, packet->size);
		}
		CaThreadsPacket *p = packet->next;
		free(packet);
		packet = p;
	}
}

int CaThreadsProcess::recv()
{
	CaThreadsPacket *packet;
	pthread_mutex_lock(&_lock);
	packet = _packet;
	_packet = NULL;
	pthread_mutex_unlock(&_lock);

	if (packet == NULL) {
		return 0;
	}

	if (_logger) {
	    _logger->log_receive();
	}
	process_packets(packet);
	return 1;
}

static int init_listen_socket(int port)
{
	int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (sock == -1) {
		perror("socket");
		exit(-1);
	}

	struct sockaddr_in sockname;
	sockname.sin_family = AF_INET;
	sockname.sin_port = htons(port);
	sockname.sin_addr.s_addr = INADDR_ANY;
	if (bind(sock, (struct sockaddr *)&sockname, sizeof(sockname)) < 0) {
		perror("bind");
		exit(-1);
	}
	if (listen(sock, 1) < 0) {
		perror("listen");
		exit(-1);
	}
	if (port != 0) {
		return sock;
	}

	// If -s was "auto" then print port number at stdout 
	socklen_t len = sizeof(sockname);
	if (getsockname(sock, (struct sockaddr *) &sockname, &len) < 0) {
		perror("getsockname");
		exit(-1);
	}
	printf("%i\n", ntohs(sockname.sin_port));
	fflush(stdout);

	return sock;
}

#define LINE_LENGTH_LIMIT 4096
int CaThreadsModule::process_commands(FILE *comm_in, FILE *comm_out)
{
	char line[LINE_LENGTH_LIMIT];
	for(;;) {
		fflush(comm_out);
		char *s = fgets(line, LINE_LENGTH_LIMIT, comm_in);
		if (s == NULL) {
			return -1;
		}

		// remove \r and \n from the end
		size_t t = strlen(s) - 1;
		while(t > 0 && (s[t] == '\n' || s[t] == '\r')) { s[t] = 0; t--; }

		if (!strcmp(line, "QUIT")) {
			for (t = 0; t < nodes_count; t++) {
				_contexts[t]->halt();
			}
			return 0; 
		}

		if (!strcmp(line, "DETACH")) { 
			return 0; 
		}

		if (!strcmp(line, "REPORTS")) {
			write_reports(comm_out);
			continue;
		}
		if (strcmp(line, "FIRE") > 0) {
			int transition_id, iid;
			if (2 != sscanf(line, "FIRE %i %i", &transition_id, &iid)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			fire_transition(transition_id, iid);
			int t;
			for (t = 0; t < ca_process_count; t++) {
				_processes[t]->recv();
			}
			fprintf(comm_out, "Ok\n");
			continue;
		}
		fprintf(comm_out, "Unknown command\n");
	}
}

void CaThreadsModule::write_reports(FILE *out)
{
	int t;
	for (t = 0; t < ca_process_count; t++) {
		_processes[t]->write_report(out);
	}
}

void CaThreadsModule::fire_transition(int transition_id, int iid)
{
	CaContextsMap::iterator i;
	CaTransition transition;
	int t;
	for (t = 0; t < nodes_count; t++) {
		CaContext *ctx = _contexts[t];
		if (ctx->iid() == iid && ctx->_find_transition(transition_id, transition) && !ctx->_check_halt_flag()) {
				transition.call(ctx);
		}
	}
}

static void * ca_threads_listener(void *data)
{
	CaThreadsListenData *ldata = (CaThreadsListenData*) data;

	for(;;) {
		int client = accept(ldata->listen_socket, NULL, NULL);

		if (client < 0) {
			perror("accept");
		}

		FILE *comm_in = fdopen(client, "r");
		FILE *comm_out = fdopen(client, "w");
		if (comm_in == NULL || comm_out == NULL) {
			perror("ERROR");
			exit(-1);
		}
		setlinebuf(stdout); 
		/* Because simulator typically runs with redirected stdout, 
			we need to switch back to more expected behavior as in normal
			 run of program in console */

		ca_write_header(comm_out);

		/* Init barriers */
		pthread_barrier_t barrier1;
		pthread_barrier_t barrier2;
		pthread_barrier_t barrier3;

		pthread_barrier_init(&barrier1, NULL, ca_process_count);
		pthread_barrier_init(&barrier2, NULL, ca_process_count + 1);
		pthread_barrier_init(&barrier3, NULL, ca_process_count + 1);

		CaThreadsBarriers barriers;
		barriers.barrier1 = &barrier1;
		barriers.barrier2 = &barrier2;
		barriers.barrier3 = &barrier3;

		ldata->process->send_to_all_processes(BARRIERS_COMMAND, &barriers, sizeof(CaThreadsBarriers));

		if (ldata->start_barrier) {
			pthread_barrier_wait(ldata->start_barrier);
			ldata->start_barrier = NULL;
		}

		/* Wait for all process */
		pthread_barrier_wait(&barrier2);
		ldata->process->get_module()->process_commands(comm_in, comm_out);
		pthread_barrier_wait(&barrier3);

		pthread_barrier_destroy(&barrier1);
		pthread_barrier_destroy(&barrier2);
		pthread_barrier_destroy(&barrier3);
		close(client);
	}
}

int CaThreadsModule::main(int nodes, InitFn *init_fn)
{
	if (ca_process_count < 1 || ca_process_count > nodes) {
		ca_process_count = nodes;
	}

	assert(nodes > 0);
	nodes_count = nodes;
	int t;
	pthread_t threads[nodes];
	CaThreadsData data[nodes];

	CaThreadsProcess *processes[ca_process_count];
	_processes = processes;

	CaThreadsListenData ldata;
	if (ca_listen_port != -1) {
		ldata.listen_socket = init_listen_socket(ca_listen_port);
	}

	for (t = 0; t < ca_process_count; t++) {
	    processes[t] = new CaThreadsProcess(this, t);
	}

	CaContext *contexts[nodes];
	this->_contexts = contexts;
	for (t = 0; t < nodes; t++) {
		CaThreadsProcess *process = processes[ca_node_to_process(t)];
		contexts[t] = new CaContext(t, process);
		process->add_context(contexts[t]);
	}

	pthread_barrier_t start_barrier;

	if (ca_block_on_start) {
		pthread_barrier_init(&start_barrier, NULL, 2);
		ldata.start_barrier = &start_barrier;
	} else {
		ldata.start_barrier = NULL;
	}

	if (ca_listen_port != -1) {
		ldata.process = processes[0];
		pthread_t listen_thread;
		pthread_create(&listen_thread, NULL, ca_threads_listener, &ldata);
	}

	if (ca_block_on_start) {
		pthread_barrier_wait(&start_barrier);
		pthread_barrier_destroy(&start_barrier);
	}

	for (t = 0; t < ca_process_count; t++) {
		data[t].process = processes[t];
		data[t].init_fn = init_fn;
		/* FIXME: Check returns code */
		pthread_create(&threads[t], NULL, ca_threads_starter, &data[t]);
	}
	
	for (t = 0; t < ca_process_count; t++) {
		pthread_join(threads[t], NULL);
	}

	for (t = 0; t < nodes; t++) {
	    delete contexts[t];
	}

	for (t = 0; t < ca_process_count; t++) {
	    delete processes[t];
	}

	// TODO: destroy nonempty places
	return 0;
}

void CaThreadsProcess::idle() {
	sched_yield();
}
