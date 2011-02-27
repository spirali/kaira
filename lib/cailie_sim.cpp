
#include "cailie_sim.h"
#include <string.h>
#include <assert.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <unistd.h>

#define LINE_LENGTH_LIMIT 4096

static int init_socket_listen_and_accept()
{
	int sock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (sock == -1) {
		perror("ERROR");
		exit(-1);
	}

	struct sockaddr_in sockname;
	sockname.sin_family = AF_INET;
	sockname.sin_port = htons(0); // pick free port
	sockname.sin_addr.s_addr = INADDR_ANY;
	if (bind(sock, (struct sockaddr *)&sockname, sizeof(sockname)) == -1) {
		perror("ERROR");
		exit(-1);
	}
	if (listen(sock, 1) == -1) {
		perror("ERROR");
		exit(-1);
	}
	socklen_t len = sizeof(sockname);
	if (getsockname(sock, (struct sockaddr *) &sockname, &len) == -1) {
		perror("ERROR");
		exit(-1);
	}
	printf("%i\n", ntohs(sockname.sin_port));
	fflush(stdout);
	int client = accept(sock, NULL, NULL);
	if (client == -1) {
		perror("ERROR");
		exit(-1);
	}
	return client;
}

int CaSimModule::main(int nodes_count, InitFn *init_fn)
{
    CaSimProcess process;
    return process.main(nodes_count, init_fn);
}

int CaSimProcess::main(int nodes_count, InitFn *init_fn) 
{
	int socket = init_socket_listen_and_accept();
	comm_in = fdopen(socket, "r");
	comm_out = fdopen(socket, "w");

	setlinebuf(stdout); 
	/* Because simulator typically runs with redirected stdout, 
	we need to switch back to more expected behavior as in normal
	 run of program in console */

	if (comm_in == NULL || comm_out == NULL) {
		perror("ERROR");
		exit(-1);
	}

	int t;
	for (t=0; t < nodes_count; t++) {
		CaContext *ctx = new CaContext(t, this);
		init_fn(ctx);
		_contexts[t] = ctx;
	}
	run_listener();
	return 0;
}

void CaSimProcess::send(CaContext *ctx, int target, int data_id, void *data, size_t size) 
{
	RecvFn *recv_fn = _contexts[target]->_get_recv_fn();
	recv_fn(_contexts[target]->_get_places(), data_id, data, size);
	free(data);
}

void CaSimProcess::quit(CaContext *ctx)
{
	CaContextsMap::iterator i;
	for (i = _contexts.begin(); i != _contexts.end(); i++) {
		i->second->halt();
	}
}

int CaSimProcess::recv()
{
	/* We dont need this function in simulator, because
		standard transtion scheduler is not started */
}

int CaSimProcess::run_listener()
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
			return 0; 
		}
		if (!strcmp(line, "REPORTS")) {
			CaOutput output;
			CaContextsMap::iterator i;
			output.child("report");
			for (i = _contexts.begin(); i != _contexts.end(); i++) {
				ReportFn *f = i->second->_get_report_fn();
				assert(f != NULL);
				output.child("node");
				f(i->second, i->second->_get_places(), &output);
				output.back();
			}
			CaOutputBlock *block = output.back();
			block->write(comm_out);
			fprintf(comm_out, "\n");
			continue;
		}
		if (strcmp(line, "FIRE") > 0) {
			int transition_id, iid;
			if (2 != sscanf(line, "FIRE %i %i", &transition_id, &iid)) {
				fprintf(comm_out, "Invalid parameters\n");
				continue;
			}
			fire_transition(transition_id, iid);
			fprintf(comm_out, "Ok\n");
			continue;
		}
		error("Unknown command");
	}
}

void CaSimProcess::fire_transition(int transition_id, int iid)
{
	CaContextsMap::iterator i;
	CaTransition t;
	for (i = _contexts.begin(); i != _contexts.end(); i++) {
		if (i->second->iid() == iid && i->second->_find_transition(transition_id, t) && !i->second->_check_halt_flag()) {
				t.call(i->second);
		}
	}
}


