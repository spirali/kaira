
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <arpa/inet.h>
#include <sys/types.h>
#include <netinet/in.h>

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

#include "caclient.h"

CaClient::CaClient() : connection_socket(-1)
{
}

CaClient::~CaClient()
{
	close(connection_socket);
}

void CaClient::connect(const std::string &hostname, int port)
{
	struct hostent     *he;
	struct sockaddr_in  addr;

	he = gethostbyname("localhost");
	if (he == NULL) {
		perror("gethostbyname");
		exit(1);
	}

	connection_socket = socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (connection_socket < 0) {
		perror("Socket init");
		exit(1);
	}

	memcpy(&addr.sin_addr, he->h_addr_list[0], he->h_length);
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);

	if (::connect(connection_socket, (struct sockaddr *)&addr, sizeof(addr))) {
		perror("connect");
		exit(1);
	}

	int size;
	read_data(&size, sizeof(int));
	char *buffer = new char[size];
	read_data(buffer, size);
	process_inital_data(buffer, size);
	delete [] buffer;
}

void CaClient::register_function(const std::string &name,
			const std::string &definition,
			int *id)
{
	std::map<std::string, std::pair<std::string, int> >::iterator i = functions.find(name);
	if (i == functions.end()) {
		fprintf(stderr, "Function '%s' is not provided by server API\n", name.c_str());
		exit(1);
	}
	if (i->second.first != definition) {
		fprintf(stderr, "Function '%s' is incompatible API with server\n", name.c_str());
		exit(1);
	}
	*id = i->second.second;
}

void CaClient::read_data(void *buffer, size_t size)
{
	char *p = (char*) buffer;
	do {
		int r = read(connection_socket, p, size);
		if (r < 0) {
			perror("read_message");
			exit(-1);
		}
		p += r;
		size -= r;
	} while(size > 0);
}

void CaClient::process_inital_data(void *buffer, size_t size)
{
	std::vector<bool> checked(functions.size(), false);
	CaUnpacker unpacker(buffer);
	int count = unpacker.unpack_int();
	for (int t = 0; t < count; t++) {
		std::string name = unpacker.unpack_string();
		std::string definition = unpacker.unpack_string();
		functions[name].first = definition;
		functions[name].second = t;
	}
}

void * CaClient::call(int function_id, CaPacker arguments)
{
	CaCallHeader *header = (CaCallHeader*) arguments.get_buffer();
	header->fn = function_id;
	header->size = arguments.get_size() - sizeof(CaCallHeader);
	::send(connection_socket, header, arguments.get_size(), 0);
	free(header);
	size_t size;
	read_data(&size, sizeof(size_t));
	char *buffer = new char[size];
	read_data(buffer, size);
	return buffer;
}
