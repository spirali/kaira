
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>


#include <unistd.h>
#include <stdio.h>


#include "caserver.h"
#include "client.h"


using namespace caserver;

struct CaCallHeader {
	unsigned int fn;
	size_t size;
};


CaClient::CaClient(CaServer &server, int client_socket)
	: client_socket(client_socket), server(server)
{

}

void CaClient::run()
{
	CaCallHeader header;
	const std::vector<CaPublicFunction> &functions = server.get_functions();

	for (;;) {
		if (!read_data(&header, sizeof(header))) {
			break;
		}

		if (header.fn < 0 || header.fn >= functions.size()) {
			fprintf(stderr, "Invalid function id");
			break;
		}

		char *buffer = new char[header.size];

		if (!read_data(buffer, header.size)) {
			delete [] buffer;
			break;
		}

		ca::Packer packer(ca::PACKER_DEFAULT_SIZE, sizeof(size_t));
	        functions[header.fn].call(buffer, packer);
		delete [] buffer;

		size_t *h = (size_t*) packer.get_buffer();
		*h = packer.get_size() - sizeof(size_t);
		send(client_socket, packer.get_buffer(), packer.get_size(), 0);
		packer.free();
	}
}

bool CaClient::read_data(void *buffer, size_t size)
{
	char *p = (char*) buffer;
	do {
		int r = read(client_socket, p, size);
		if (r == 0) {
			return false;
		}
		if (r < 0) {
			perror("read_message");
			exit(-1);
		}
		p += r;
		size -= r;
	} while(size > 0);
	return true;
}


