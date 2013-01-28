
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include <set>
#include "messages.h"
#include "net.h"
#include "packing.h"
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
	int place_index;
	int tokens_count;
	int msg_id;
};

const size_t RESERVED_PREFIX = sizeof(Tokens);

struct UndeliverMessage {
	void *data;
};

#ifdef CA_SHMEM
class Packet {
	public:
	int tag;
	void *data;

	Packet *next;
};
#endif

class Process {
	public:
		Process(int process_id, int process_count, int threads_count, int defs_count, NetDef **defs);
		virtual ~Process();
		void start();
		void join();
		void start_and_join();
		void clear();
		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);

		int get_threads_count() const { return threads_count; }
		int get_process_count() const { return process_count; }
		int get_process_id() const { return process_id; }
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id);

		void quit_all(Thread *thread);
		void quit();

		Net * get_net() { return net; }

		Net * spawn_net(Thread *thread, int def_index, bool globally);

		Thread *get_thread(int id);

		bool quit_flag;

		void multisend(int target, Net * net, int place, int tokens_count, const Packer &packer, Thread *thread);
		void multisend_multicast(const std::vector<int> &targets, Net *net, int place, int tokens_count, const Packer &packer, Thread *thread);

		void process_service_message(Thread *thread, ServiceMessage *smsg);
		void process_packet(Thread *thread, int tag, void *data);
		int process_packets(Thread *thread);

		#ifdef CA_SHMEM
		void add_packet(int tag, void *data);
		#endif

		#ifdef CA_MPI
		void wait();
		#endif

		void broadcast_packet(int tag, void *data, size_t size, Thread *thread, int exclude = -1);
		void write_header(FILE *file);
	protected:

		Net *net;
		int process_id;
		int process_count;
		int threads_count;
		int defs_count;
		NetDef **defs;
		Thread *threads;
		std::vector<void* > too_early_message;
		/*memory of net's id which wasn't created, but was quit*/
		bool net_is_quit;

		#ifdef CA_SHMEM
		pthread_mutex_t packet_mutex;
		Packet *packets;
		#endif
};

}

#endif
