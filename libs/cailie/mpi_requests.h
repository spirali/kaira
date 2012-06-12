
#ifndef CA_MPI_REQUESTS_H
#define CA_MPI_REQUESTS_H

#include <stdlib.h>
#include <mpi.h>

class CaRequestData {

	public:
		CaRequestData(char *data) { request_data = data; data_counter = 1; };
		~CaRequestData() { free(request_data); }
		void increase_counter() { data_counter++; };
		int decrease_counter() { return --data_counter; };

	private:
		char *request_data;
		int data_counter;

};

class CaMpiRequests {

	public:
		CaMpiRequests();
		~CaMpiRequests();
		void check();
		MPI_Request * new_request(char *data, bool with_previous_data);

	protected:
		MPI_Request *requests;
		CaRequestData **request_data_pointer;
		size_t requests_count;
		size_t requests_capacity;

};

#endif
