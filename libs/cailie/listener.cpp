
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


#include "listener.h"
#include "cailie.h"

static int ca_init_listen_socket(int port)
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

int CaListener::get_port()
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

void CaListener::init(int port)
{
	listen_socket = ca_init_listen_socket(port);
}

void * listener_thread(void *data)
{
	CaListener *listener = (CaListener*) data;
	listener->main();
	return NULL;
}

void CaListener::start()
{
	pthread_create(&thread, NULL, listener_thread, this);
}

void CaListener::main()
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

		int threads_count = processes[0]->get_threads_count() * process_count;
		pthread_barrier_init(&barrier1, NULL, threads_count + 1);
		pthread_barrier_init(&barrier2, NULL, threads_count + 1);

		for (int t = 0; t < process_count; t++) {
			processes[t]->send_barriers(&barrier1, &barrier2);
		}

		if (start_barrier) {
			pthread_barrier_wait(start_barrier);
			start_barrier = NULL;
		}

		/* Wait for all process */
		pthread_barrier_wait(&barrier1);
		/* Because there is always at least one thread we can use thread 0 as source of list of networks */
		ca_write_header(comm_out, process_count, threads_count);
		process_commands(comm_in, comm_out);
		pthread_barrier_wait(&barrier2);

		pthread_barrier_destroy(&barrier1);
		pthread_barrier_destroy(&barrier2);
		close(client);
	}
}

#define LINE_LENGTH_LIMIT 4096
void CaListener::process_commands(FILE *comm_in, FILE *comm_out)
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
			fprintf(comm_out, "<processes net-id='%i'>",
				processes[0]->get_net()->get_def_id());
			for (int t = 0; t < process_count; t++) {
				processes[t]->write_reports(comm_out);
			}
			fprintf(comm_out, "</processes>\n");
			fflush(stdout);
			continue;
		}

		if (strcmp(line, "FIRE") > 0) {
			if (processes[0]->quit_flag) {
				fprintf(comm_out, "Process is terminated\n");
				continue;
			}
			int transition_id;
			int process_id;
			if (2 != sscanf(line, "FIRE %i %i", &transition_id, &process_id)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			if (process_id < 0 || process_id >= process_count) {
				fprintf(comm_out, "There is no such process\n");
				continue;
			}
			CaProcess *p = processes[process_id];
			p->fire_transition(transition_id);
			fprintf(comm_out, "Ok\n");
			bool again;
			do {
				again = false;
				for (int s = 0; s < p->get_process_count(); s++) {
					for (int t = 0; t < p->get_threads_count(); t++) {
						again = again | processes[s]->get_thread(t)->process_messages();
					}
				}
			} while (again);
			continue;
		}
		fprintf(comm_out, "Unknown command\n");
	}
}
