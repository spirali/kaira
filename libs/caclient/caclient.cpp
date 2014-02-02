
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

using namespace caclient;

CaClient::CaClient() : connection_socket(-1)
{
	setup_host();
}

CaClient::~CaClient()
{
	close(connection_socket);
}

void CaClient::setup_host()
{
	char *v = getenv("CACLIENT_HOST");
	if (v == NULL) {
		fprintf(stderr, "Environment variable CACLIENT_HOST is not set\n");
		exit(-1);
	}

	char buffer[1001];

	if (2 != sscanf(v, "%1000[^:]:%i", buffer, &port) || port < 0 || port > 65535) {
		fprintf(stderr, "Value of environment variable CACLIENT_HOST has invalid format (hostname:port)\n");
		exit(-1);
	}
	hostname = buffer;
}

void CaClient::connect()
{
	struct hostent     *he;
	struct sockaddr_in  addr;

	he = gethostbyname(hostname.c_str());
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
	ca::Unpacker unpacker(buffer);
	int count;
	ca::unpack(unpacker, count);
	for (int t = 0; t < count; t++) {
		std::string name;
		ca::unpack(unpacker, name);
		std::string definition;
		ca::unpack(unpacker, definition);
		functions[name].first = definition;
		functions[name].second = t;
	}
}

void * CaClient::call(int function_id, ca::Packer arguments)
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
