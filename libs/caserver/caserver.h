
#ifndef CASERVER_H
#define CASERVER_H


#include <string>
#include <cailie.h>

namespace caserver {

	typedef void (CaPublicFn)(void *, size_t, ca::Packer&);

	class CaPublicFunction {
		public:
			CaPublicFunction(const std::string &name,
				const std::string &definition,
				CaPublicFn *fn) : fn(fn), name(name), definition(definition) {}

			std::string get_name() const {
				return name;
			}

			std::string get_definition() const {
				return definition;
			}

			void call(void *buffer, size_t size, ca::Packer &packer) const {
				fn(buffer, size, packer);
			}

		private:
			CaPublicFn *fn;
			std::string name;
			std::string definition;
	};

	class CaServer {

		public:
		CaServer();
		~CaServer();

		void register_function(const std::string &name,
			const std::string &definition,
			CaPublicFn *fn);

		void run();

		const std::vector<CaPublicFunction> & get_functions() {
			return functions;
		}

		void set_verbose_mode(bool value) {
			verbose = value;
		}

		protected:

		void setup_port();
		void init_listen_socket();

		int listen_socket;
		int port;

		std::vector<CaPublicFunction> functions;

		bool verbose;
	};

}

#endif // CASERVER_H
