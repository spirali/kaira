
#ifndef CASERVER_H
#define CASERVER_H


#include <string>
#include <cailie.h>

typedef void (CaPublicFn)(CaUnpacker &, CaPacker &);

class CaPublicFunction {
	public:
		CaPublicFunction(const std::string &name,
			const std::string &definition,
			CaPublicFn *fn) : name(name), definition(definition), fn(fn) {}
	private:
		std::string name;
		std::string definition;
		CaPublicFn *fn;
};

class CaServer {

	public:
	CaServer();
	~CaServer();

	void register_function(const std::string &name,
		const std::string &definition,
		CaPublicFn *fn);

	void run();

	protected:

	void setup_port();
	void init_listen_socket();

	int listen_socket;
	int port;

	std::vector<CaPublicFunction> functions;
};

#endif // CASERVER_H
