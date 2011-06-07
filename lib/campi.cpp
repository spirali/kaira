
#include "campi.h"
#include <stdlib.h>

void ca_mpi_send_to_all(void *buffer, int size, MPI_Datatype datatype, int tag, int process_count)
{
	for (int t = 0; t < process_count; t++) {
		MPI_Send(buffer, size, datatype, t, tag, MPI_COMM_WORLD);
	}
}

CaMpiRequests::CaMpiRequests()
{
	requests_capacity = 4;
	requests = (MPI_Request *) malloc(sizeof(MPI_Request) * requests_capacity);
	requests_data = (char**) malloc(sizeof(void*) * requests_capacity);
	// FIXME: Alloc test
	requests_count = 0;
}

CaMpiRequests::~CaMpiRequests()
{
	free(requests);
	free(requests_data);
}

void CaMpiRequests::check()
{
	while (requests_count > 0) {
		int flag, index;
		MPI_Testany(requests_count, requests, &index, &flag, MPI_STATUS_IGNORE);
		if (flag) {
			--requests_count;
			free(requests_data[index]);
			requests_data[index] = requests_data[requests_count];
			requests[index] = requests[requests_count];
		} else {
			return;
		}
	}
}

MPI_Request * CaMpiRequests::new_request(char *data)
{
	if (requests_count == requests_capacity) {
		requests_capacity *= 2;
		requests = (MPI_Request *) realloc(requests, requests_capacity * sizeof(MPI_Request));
		requests_data = (char**) realloc(requests_data, requests_capacity * sizeof(void*));
		// FIXME: Alloc test
	}
	requests_data[requests_count] = data;
	return &requests[requests_count++];
}
