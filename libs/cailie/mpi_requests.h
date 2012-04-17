
#ifndef CA_MPI_REQUESTS_H
#define CA_MPI_REQUESTS_H

#include <mpi.h>

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
