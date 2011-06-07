
#ifndef CA_MPI_H
#define CA_MPI_H

#include <mpi.h>

enum {
	CA_MPI_TAG_TOKENS = 1,
	CA_MPI_TAG_QUIT
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

void ca_mpi_send_to_all(void *buffer, int size, MPI_Datatype datatype, int tag, int process_count);

#endif
