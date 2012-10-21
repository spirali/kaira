
#include "mpi_requests.h"
#include <stdlib.h>


CaMpiRequests::CaMpiRequests()
{
	requests_capacity = 4;
	requests = (MPI_Request *) malloc(sizeof(MPI_Request) * requests_capacity);
	request_data_pointer = (CaRequestData**) malloc(sizeof(CaRequestData*) * requests_capacity);
	// FIXME: Alloc test
	requests_count = 0;
}

CaMpiRequests::~CaMpiRequests()
{
	free(requests);
	free(request_data_pointer);
}

void CaMpiRequests::check()
{
	while (requests_count > 0) {
		int flag, index;
		MPI_Testany(requests_count, requests, &index, &flag, MPI_STATUS_IGNORE);
		if (flag) {
			--requests_count;
			if(request_data_pointer[index]->decrease_counter() == 0) {
				delete request_data_pointer[index];
			}
			request_data_pointer[index] = request_data_pointer[requests_count];
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
		request_data_pointer = (CaRequestData**) realloc(request_data_pointer, requests_capacity * sizeof(CaRequestData*));
		// FIXME: Alloc test
	}

	if(!data) {
		if(requests_count == 0) {
			fprintf(stderr, "There is no previous data\n");
			exit(-1);
		}
		request_data_pointer[requests_count] = request_data_pointer[requests_count-1];
		request_data_pointer[requests_count]->increase_counter();
	} else {
		request_data_pointer[requests_count] = new CaRequestData(data);
	}

	return &requests[requests_count++];
}
