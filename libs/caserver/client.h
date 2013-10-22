
#ifndef CASERVER_CLIENT_H
#define CASERVER_CLIENT_H

#include <stdlib.h>

namespace caserver {

	class CaServer;

	class CaClient {
		public:
			CaClient(CaServer &server, int client_socket);
			void run();
		protected:
			bool read_data(void *buffer, size_t size);
			void send_initial_message();
			int client_socket;
			CaServer &server;
	};

}

#endif // CASERVER_CLIENT_H
