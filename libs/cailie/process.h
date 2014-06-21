
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include <set>
#include "messages.h"
#include "net.h"
#include "packing.h"
#include "packet.h"

#ifdef CA_MPI
#include "mpi_requests.h"
#endif

#define CA_TAG_TOKENS 0
#define CA_TAG_SERVICE 1

namespace ca {

enum ServiceMessageType { CA_SM_QUIT, CA_SM_NET_CREATE, CA_SM_WAKE , CA_SM_EXIT };
class Process;
class Thread;
struct ServiceMessage {
	ServiceMessageType type;
};

struct ServiceMessageNetCreate : ServiceMessage {
	int def_index;
};

struct Tokens {
	int edge_id;
	int tokens_count;
};

const size_t RESERVED_PREFIX = sizeof(Tokens);

struct UndeliverMessage {
	void *data;
};

#ifdef CA_SHMEM
struct ShmemPacket : public Packet {
	int tag;
	ShmemPacket *next;
};
#endif

class Process {
	public:
		Process(int process_id, int process_count, int defs_count, NetDef **defs);
		virtual ~Process();
		void start(bool own_thread);
		void join();
		void clear();
		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);
		void quit_all();
		void quit();

		Net *spawn_net(int def_index, bool globally);


		Thread *get_thread() {
			return thread;
		}

		void send(int target, Net * net, int edge_id, int tokens_count,
			  Packer &packer, Thread *thread);
		void send_multicast(const std::vector<int> &targets, Net *net, int edge_id,
			int tokens_count, Packer &packer, Thread *thread);

		void collective_scatter_root(int transition_id, const void *data, size_t size);
		void collective_scatter_nonroot(int transition_id, int root, void *out, size_t size);
		void collective_scatterv_root(int transition_id, const void *data, int *sizes, int *displs);
		void collective_scatterv_nonroot(int transition_id, int root, void *out, size_t size);


		void collective_gather_root(int transition_id, const void *data, size_t size, void *out);
		void collective_gather_nonroot(int transition_id, int root, const void *data, size_t size);
		void collective_gatherv_root(int transition_id, const void *data, int size, void *out, int *sizes, int *displs);
		void collective_gatherv_nonroot(int transition_id, int root, const void *data, int size);

		void collective_allgather(int transition_id, const void *data, size_t size, void *out);
		void collective_allgatherv(int transition_id, const void *data, int size, void *out, int *sizes, int *displs);


		void collective_bcast_root(int transition_id, const void *data, size_t size);
		void collective_bcast_nonroot(int transition_id, int root, void *out, size_t size);

		void collective_barrier(int transition_id);

		void process_service_message(Thread *thread, ServiceMessage *smsg);
		bool process_packet(Thread *thread, int from_process, int tag, void *data);
		int process_packets(Thread *thread);

		#ifdef CA_SHMEM
		void add_packet(int from_process, int tag, void *data, size_t size);
		static void init_collective_operations(int process_count);
		#endif

		#ifdef CA_MPI
		void wait();
		#endif

		void broadcast_packet(int tag, void *data, size_t size, int exclude = -1);
		void write_header(FILE *file);

		int get_process_count() const {
			return process_count;
		}

		int get_process_id() const {
			return process_id;
		}

		Net * get_net() const {
			return net;
		}

		bool quit_flag;
	protected:

		struct EarlyMessage {
			int from_process;
			void *data;
		};

		Net *net;
		int process_id;
		int process_count;
		int defs_count;
		NetDef **defs;
		Thread *thread;
		std::vector<EarlyMessage> too_early_message;
		/*memory of net's id which wasn't created, but was quit*/
		bool net_is_quit;

		#ifdef CA_SHMEM
		pthread_mutex_t packet_mutex;
		ShmemPacket *packets;

		/* Collective communication */
		const void *collective_data;
		const int *collective_displs;
		static pthread_mutex_t collective_mutex;
		static int collective_root;
		static int collective_transition_id;
		static pthread_barrier_t collective_barrier1;
		static pthread_barrier_t collective_barrier2;

		void setup_collective_operation(int transition_id, bool use_root, int root);
		#endif
};

}

#endif
