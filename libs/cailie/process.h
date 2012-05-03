
#ifndef CAILIE_PROCESS_H
#define CAILIE_PROCESS_H

#include <pthread.h>
#include <vector>
#include <map>
#include <set>
#include "messages.h"
#include "net.h"
#include "packing.h"
#include "logging.h"

#ifdef CA_MPI
#include "mpi_requests.h"
#endif

#define CA_RESERVED_PREFIX sizeof(CaTokens)

#define CA_TAG_TOKENS 0
#define CA_TAG_SERVICE 1

enum CaServiceMessageType { CA_SM_QUIT, CA_SM_NET_CREATE, CA_SM_NET_HALT, CA_SM_WAKE , CA_SM_EXIT, CA_SM_ERROR};
class CaProcess;
class CaThread;
struct CaServiceMessage {
	CaServiceMessageType type;
};

struct CaServiceMessageNetCreate : CaServiceMessage {
	int net_id;
	int def_index;
};

struct CaServiceMessageNetHalt : CaServiceMessage {
	int net_id;
};

struct CaServiceMessageError : CaServiceMessage {
	int net_id;
};

struct CaTokens {
	int place_index;
	int net_id;
	int tokens_count;
};

struct CaUndeliverMessage {
	int net_id;
	void *data;
};

#ifdef CA_SHMEM
class CaPacket {
	public:
	int tag;
	void *data;

	CaPacket *next;
};
#endif

class CaProcess {
	public:
		CaProcess(int process_id, int process_count, int threads_count, int defs_count, CaNetDef **defs);
		virtual ~CaProcess();
		void start();
		void join();
		void start_and_join();
		void clear();
		void inform_new_network(CaNet *net, CaThread *thread);
		void inform_halt_network(int net_id, CaThread *thread);
		void send_barriers(pthread_barrier_t *barrier1, pthread_barrier_t *barrier2);
		void update_net_id_counters(int net_id);
		bool is_future_id(int net_id);

		int get_threads_count() const { return threads_count; }
		int get_process_count() const { return process_count; }
		int get_process_id() const { return process_id; }
		void write_reports(FILE *out) const;
		void fire_transition(int transition_id, int instance_id);

		void quit_all(CaThread *thread);
		void quit() { quit_flag = true; }
		void halt(CaThread *thread, CaNet *net);


		void start_logging(const std::string &logname);
		void stop_logging();

		CaNet * spawn_net(CaThread *thread, int def_index, int id, CaNet *parent_net, bool globally);

		int new_net_id();

		CaThread *get_thread(int id);

		bool quit_flag;

		void multisend(int target, CaNet * net, int place, int tokens_count, const CaPacker &packer, CaThread *thread);
		void multisend_multicast(const std::vector<int> &targets, CaNet *net, int place, int tokens_count, const CaPacker &packer, CaThread *thread);

		void process_service_message(CaThread *thread, CaServiceMessage *smsg);
		void process_packet(CaThread *thread, int tag, void *data);
		int process_packets(CaThread *thread);

		#ifdef CA_SHMEM
		void add_packet(int tag, void *data);
		#endif

		#ifdef CA_MPI
		void wait();
		#endif

		void broadcast_packet(int tag, void *data, size_t size, CaThread *thread, int exclude = -1);
	protected:

		void autohalt_check(CaNet *net);

		int process_id;
		int process_count;
		int threads_count;
		int defs_count;
		CaNetDef **defs;
		CaThread *threads;
		int id_counter;
		pthread_mutex_t counter_mutex;
		int *process_id_counter;
		std::map<int, std::vector<void* > > too_early_message;
		/*memory of net's id which wasn't created, but was halted*/
		std::set<int > halted_net;

		#ifdef CA_SHMEM
		pthread_mutex_t packet_mutex;
		CaPacket *packets;
		#endif
};

class CaThread {
	public:
		CaThread();
		~CaThread();
		int get_id() { return id; }
		void start();
		void join();
		void clear();
		void run_scheduler();

		int get_process_id() { return process->get_process_id(); }
		int get_process_count() { return process->get_process_count(); }
		int get_threads_count() { return process->get_threads_count(); }
		#ifdef CA_MPI
		CaMpiRequests * get_requests() { return &requests; }
		#endif

		void add_message(CaThreadMessage *message);
		bool process_thread_messages();
		int process_messages();
		void clean_thread_messages();
		void process_message(CaThreadMessage *message);
		void quit_all();

		void halt(CaNet *net) { process->halt(this, net); }

		void send(int target, CaNet *net, int place, const CaPacker &packer) {
			process->multisend(target, net, place, 1, packer, this);
		}
		void multisend(int target, CaNet *net, int place, int tokens_count, const CaPacker &packer) {
			process->multisend(target, net, place, tokens_count, packer, this);
		}
		void send_multicast(const std::vector<int> &targets, CaNet *net, int place, const CaPacker &packer) {
			process->multisend_multicast(targets, net, place, 1, packer, this);
		}
		void multisend_multicast(const std::vector<int> &targets, CaNet *net, int place, int tokens_count, const CaPacker &packer) {
			process->multisend_multicast(targets, net, place, tokens_count, packer, this);
		}
		CaProcess * get_process() const { return process; }

		void init_log(const std::string &logname);
		void close_log() { if (logger) { delete logger; logger = NULL; } }

		CaNet * spawn_net(int def_index, CaNet *parent_net);
		CaNet * get_net(int id);
		CaNet * remove_net(int id);
		/*
		void log_transition_start(CaUnit *unit, int transition_id) {
			if (logger) { logger->log_transition_start(unit, transition_id); }
		}

		void log_transition_end(CaUnit *unit, int transition_id) {
			if (logger) { logger->log_transition_end(unit, transition_id); }
		}

		void log_token_add(CaUnit *unit, int place_id, const std::string &token_string) {
			if (logger) { logger->log_token_add(unit, place_id, token_string); }
		}

		void log_token_remove(CaUnit *unit, int place_id, const std::string &token_string) {
			if (logger) { logger->log_token_remove(unit, place_id, token_string); }
		}

		void log_unit_status(CaUnit *unit, int def_id) {
		//	if (logger) { unit->log_status(logger, process->get_def(def_id)); }
		}
		*/

		void add_network(CaNet *net) {
			nets.push_back(net);
		}

		/*
		void start_logging(const std::string &logname) { process->start_logging(logname); }
		void stop_logging() { process->stop_logging(); }
		*/

		int get_nets_count() { return nets.size(); }
		const std::vector<CaNet*> & get_nets() { return nets; }

		void set_process(CaProcess *process, int id) { this->process = process; this->id = id; }

		CaNet *last_net() { return nets[nets.size() - 1]; }

	protected:
		CaProcess *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		CaThreadMessage *messages;
		std::vector<CaNet*> nets;
		int id;

		#ifdef CA_MPI
		CaMpiRequests requests;
		#endif

		CaLogger *logger;
};

#endif
