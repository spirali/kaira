
#include "cailie.h"
#include "listener.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>

using namespace ca;

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
	return sock;
}

void write_status(FILE *out, bool status)
{
	if (status) {
		fprintf(out, "Ok\n");
	} else {
		fprintf(out, "No\n");
	}
}

int Listener::get_port()
{
	// If -s was "auto" then print port number at stdout
	struct sockaddr_in sockname;
	socklen_t len = sizeof(sockname);
	if (getsockname(listen_socket, (struct sockaddr *) &sockname, &len) < 0) {
		perror("getsockname");
		exit(-1);
	}
	return ntohs(sockname.sin_port);
}

void Listener::init(int port)
{
	listen_socket = init_listen_socket(port);
}

void * listener_thread(void *data)
{
	Listener *listener = (Listener*) data;
	listener->main();
	return NULL;
}

void Listener::start()
{
	pthread_create(&thread, NULL, listener_thread, this);
}

void Listener::wait_for_connection()
{
	fd_set s;
	FD_ZERO(&s);
	FD_SET(listen_socket,&s);
	int r = select(listen_socket + 1, &s, NULL, NULL, NULL);
	if (r < 0) {
		perror("Listener::wait_for_connection");
		exit(-1);
	}
}

void Listener::main()
{
	for(;;) {
		int client = accept(listen_socket, NULL, NULL);

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


		/* Init barriers */
		pthread_barrier_t barrier1;
		pthread_barrier_t barrier2;

		pthread_barrier_init(&barrier1, NULL, process_count + 1);
		pthread_barrier_init(&barrier2, NULL, process_count + 1);

		for (int t = 0; t < process_count; t++) {
			processes[t]->send_barriers(&barrier1, &barrier2);
		}

		if (start_barrier) {
			pthread_barrier_wait(start_barrier);
			start_barrier = NULL;
		}

		/* Wait for all process */
		pthread_barrier_wait(&barrier1);
		write_header(comm_out, process_count);

		prepare_state();
		process_commands(comm_in, comm_out);
		cleanup_state();
		pthread_barrier_wait(&barrier2);

		pthread_barrier_destroy(&barrier1);
		pthread_barrier_destroy(&barrier2);
		close(client);
	}
}

static bool check_prefix(const char *s, const char *prefix)
{
	while (*s == *prefix) {
		if (*s == 0) {
			return true;
		}
		s++;
		prefix++;
	}
	if (*prefix == 0) {
		return true;
	}
	return false;
}

#define LINE_LENGTH_LIMIT 4096
void Listener::process_commands(FILE *comm_in, FILE *comm_out)
{
	char line[LINE_LENGTH_LIMIT];
	for(;;) {
		fflush(comm_out);
		char *s = fgets(line, LINE_LENGTH_LIMIT, comm_in);
		if (s == NULL) {
			return;
		}

		// remove \r and \n from the end
		size_t t = strlen(s) - 1;
		while(t > 0 && (s[t] == '\n' || s[t] == '\r')) { s[t] = 0; t--; }
		if (!strcmp(line, "QUIT")) {
			exit(0);
			return;
		}

		if (!strcmp(line, "DETACH")) {
			return;
		}

		if (!strcmp(line, "REPORTS")) {
			state->write_reports(comm_out);
			fprintf(comm_out, "\n");
			fflush(stdout);
			continue;
		}

		if (check_prefix(line, "FIRE")) {
			if (processes[0]->quit_flag) {
				fprintf(comm_out, "Process is terminated\n");
				continue;
			}
			int transition_id;
			int process_id;
			int phases;
			if (3 != sscanf(line, "FIRE %i %i %i", &transition_id, &process_id, &phases)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			if (process_id < 0 || process_id >= process_count) {
				fprintf(comm_out, "There is no such process\n");
				continue;
			}
			TransitionDef *transition_def = state->get_net_def()->get_transition_def(transition_id);
			if (transition_def == NULL) {
				fprintf(comm_out, "Invalid transition\n");
				continue;
			}

			bool result;
			if (phases == 1) {
				result = state->fire_transition_phase1(process_id, transition_def);
			} else {
				result = state->fire_transition_full(process_id, transition_def);
			}
			write_status(comm_out, result);
			continue;
		}

		if (check_prefix(line, "FINISH")) {
			if (processes[0]->quit_flag) {
				fprintf(comm_out, "Process is terminated\n");
				continue;
			}
			int process_id;
			if (1 != sscanf(line, "FINISH %i", &process_id)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			if (process_id < 0 || process_id >= process_count) {
				fprintf(comm_out, "There is no such process\n");
				continue;
			}

			if (!state->is_process_busy(process_id)) {
				fprintf(comm_out, "There is no running transition on the given process\n");
				continue;
			}

			Activation *a = state->get_activations()[process_id];
			if (a->transition_def->is_blocked(a->binding)) {
				fprintf(comm_out, "Transition waits for synchronization of collective transition\n");
				continue;
			}
			state->finish_transition(process_id);
			write_status(comm_out, true);
			continue;
		}

		if (check_prefix(line, "RECEIVE") > 0) {
			int process_id;
			int origin_id;
			if (2 != sscanf(line, "RECEIVE %i %i", &process_id, &origin_id)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			bool result = state->receive(process_id, origin_id);
			write_status(comm_out, result);
			continue;
		}

		fprintf(comm_out, "Unknown command\n");
	}
}

void Listener::prepare_state()
{
	// Process all pending messages
	bool again;
	do {
		again = false;
		for (int s = 0; s < process_count; s++) {
			again = again | processes[s]->get_thread()->process_messages();
		}
	} while (again);

	std::vector<Net*> nets;
	nets.reserve(process_count);

	for (int i = 0; i < process_count; i++) {
		nets.push_back(processes[i]->get_net());
	}
	state = new State(processes[0]->get_net()->get_def(), nets);
}

void Listener::cleanup_state()
{
	delete state;
}


