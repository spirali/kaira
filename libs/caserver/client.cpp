
#include <unistd.h>
#include <stdio.h>

#include "client.h"

struct Header {
	int fn;
	size_t size;
};

CaClient::CaClient(CaServer &server, int client_socket) : client_socket(client_socket), server(server)
{

}

void CaClient::run()
{
	Header header;

	for (;;) {
		read_message(&header, sizeof(header));
		void *buffer = malloc(header.size);
		read_message(buffer, header.size);

		free(buffer);
	}
}

void CaClient::read_message(void *buffer, size_t size)
{
	char *p = (char*) buffer;
	do {
		int r = read(client_socket, p, size);
		if (r < 0) {
			perror("read_message");
			exit(-1);
		}
		p += r;
		size -= r;
	} while(size > 0);
}
