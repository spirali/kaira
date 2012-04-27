
#ifndef CACLIENT_H
#define CACLIENT_H

#include <string>
#include <map>
#include <cailie.h>

struct CaCallHeader {
	unsigned int fn;
	size_t size;
};

#define CA_RESERVED_CALL_PREFIX sizeof(CaCallHeader)

class CaClient {

	public:
		CaClient();
		~CaClient();
		void connect(const std::string &hostname, int port);
		void register_function(const std::string &name,
			const std::string &definition,
			int *id);
		void * call(int function_id, CaPacker arguments);
	protected:
		int connection_socket;
		std::map<std::string, std::pair<std::string, int> > functions; // name -> (definition, id)
		void read_data(void *buffer, size_t size);
		void process_inital_data(void *buffer, size_t size);
};

#endif // CACLIENT_H
