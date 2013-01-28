
#ifndef CA_MPI_REQUESTS_H
#define CA_MPI_REQUESTS_H

#include <stdlib.h>
#include <mpi.h>

namespace ca {

class RequestData {

	public:
		RequestData(char *data) : data(data), ref_counter(1) {};
		void inc_refcounter() { ref_counter++; };
		void dec_refcounter() { if (--ref_counter == 0) { free(data); delete this; } };
	private:
		char *data;
		int ref_counter;

};

class MpiRequests {

	public:
		MpiRequests();
		~MpiRequests();
		void check();
		MPI_Request * new_request(char *data);

	protected:
		MPI_Request *requests;
		RequestData **requests_data;
		size_t requests_count;
		size_t requests_capacity;

};

}

#endif
