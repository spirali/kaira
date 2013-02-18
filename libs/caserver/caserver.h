
#ifndef CASERVER_H
#define CASERVER_H


#include <string>
#include <cailie.h>

typedef ca::Packer (CaPublicFn)(void *);

class CaPublicFunction {
	public:
		CaPublicFunction(const std::string &name,
			const std::string &definition,
			CaPublicFn *fn) : fn(fn), name(name), definition(definition) {}

		std::string get_name() const { return name; }
		std::string get_definition() const { return definition; }
		ca::Packer call(void *buffer) const { return fn(buffer); }
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

	protected:

	void setup_port();
	void init_listen_socket();

	int listen_socket;
	int port;

	std::vector<CaPublicFunction> functions;
};

#endif // CASERVER_H
