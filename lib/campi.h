
#ifndef CA_MPI_H
#define CA_MPI_H

#include <mpi.h>

enum {
	CA_MPI_TAG_TOKENS = 1
};

struct CaPacket {
	int unit_id;
	int place_pos;
	size_t tokens_count;
};

class CaMpiRequests {

	public:
		CaMpiRequests();
		~CaMpiRequests();
		void check();
		MPI_Request * new_request(char *data);

	protected:
		MPI_Request *requests;
		char **requests_data;
		size_t requests_count;
		size_t requests_capacity;
};

#endif
