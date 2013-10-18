
#ifndef CACLIENT_H
#define CACLIENT_H

#include <string>
#include <map>
#include <cailie.h>

namespace caclient {

	struct CaCallHeader {
		unsigned int fn;
		size_t size;
	};

	#define CACLIENT_RESERVED_CALL_PREFIX sizeof(CaCallHeader)

	class CaClient {

		public:
			CaClient();
			~CaClient();
			void connect();
			void register_function(const std::string &name,
				const std::string &definition,
				int *id);
			void * call(int function_id, ca::Packer arguments);
		protected:
			void read_data(void *buffer, size_t size);
			void process_inital_data(void *buffer, size_t size);
			void setup_host();

			int connection_socket;
			std::string hostname;
			int port;
			std::map<std::string, std::pair<std::string, int> > functions; // name -> (definition, id)

	};

}

#endif // CACLIENT_H
