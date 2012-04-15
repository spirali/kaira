
#ifndef CASERVER_CLIENT_H
#define CASERVER_CLIENT_H

#include <stdlib.h>

class CaServer;

class CaClient {
	public:
		CaClient(CaServer &server, int client_socket);
		void run();
	protected:
		void read_message(void *buffer, size_t size);
		int client_socket;
		CaServer &server;
};

#endif // CASERVER_CLIENT_H
