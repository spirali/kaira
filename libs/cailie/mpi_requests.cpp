
#include "mpi_requests.h"
#include <stdlib.h>
#include <stdio.h>


CaMpiRequests::CaMpiRequests()
{
	requests_capacity = 4;
	requests = (MPI_Request *) malloc(sizeof(MPI_Request) * requests_capacity);
	requests_data = (CaRequestData**) malloc(sizeof(CaRequestData*) * requests_capacity);
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
			requests_data[index]->dec_refcounter();
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
		requests_data = (CaRequestData**) realloc(requests_data, requests_capacity * sizeof(CaRequestData*));
		// FIXME: Alloc test
	}

	if(data == NULL) {
		if(requests_count == 0) {
			fprintf(stderr, "Internal error: CaMpiRequests::new_request: "
								"data == NULL and requests_count == 0\n");
			exit(-1);
		}
		requests_data[requests_count] = requests_data[requests_count-1];
		requests_data[requests_count]->inc_refcounter();
	} else {
		requests_data[requests_count] = new CaRequestData(data);
	}

	return &requests[requests_count++];
}
