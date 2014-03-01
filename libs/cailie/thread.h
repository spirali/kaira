
#ifndef CAILIE_THREAD_H
#define CAILIE_THREAD_H

#include "process.h"
#include "tracelog.h"
#include "messages.h"
#include <limits.h>

namespace ca {

class ThreadBase {
	public:
		ThreadBase(int id = -1, TraceLog *tracelog = NULL)
			: id(id), tracelog(tracelog) {}

		virtual ~ThreadBase() {}

		virtual void quit_all() = 0;
		virtual int get_process_count() const = 0;
		virtual int get_threads_count() const = 0;
		virtual int get_process_id() const = 0;


		virtual void send(int target, NetBase *net, int edge_id,
						int tokens_count, const Packer &packer) = 0;
		virtual void send_multicast(const std::vector<int> &targets, NetBase *net,
			int edge_id, int tokens_count, const Packer &packer) = 0;

		virtual void collective_scatter_root(int transition_id, const void *data, size_t size) {}
		virtual void collective_scatter_nonroot(int transition_id, int root, void *out, size_t size) {}
		virtual void collective_scatterv_root(int transition_id, const void *data, int *sizes, int *displs) {}
		virtual void collective_scatterv_nonroot(int transition_id, int root, void *out, size_t size) {}


		virtual void collective_gather_root(int transition_id, const void *data, size_t size, void *out) {}
		virtual void collective_gather_nonroot(int transition_id, int root, const void *data, size_t size) {}
		virtual void collective_gatherv_root(int transition_id, const void *data, int size,
				void *out, int *sizes, int *displs) {}
		virtual void collective_gatherv_nonroot(int transition_id, int root, const void *data, int size) {}

		virtual void collective_bcast_root(int transition_id, const void *data, size_t size) {}
		virtual void collective_bcast_nonroot(int transition_id, int root, void *out, size_t size) {}

		/* For simulated run */

		/* Methods for simulated run where packet with fake size can be sent */
		virtual void send(int target, NetBase *net, int edge_id,
						int tokens_count, const Packer &packer, size_t fake_size) {}
		virtual void send_multicast(const std::vector<int> &targets, NetBase *net,
			int edge_id, int tokens_count, const Packer &packer, size_t fake_size) {};

		virtual int collective_bindings(TransitionDef *transition_def, std::vector<void*> &bindings) {}
		/* End of methods for sumulated run */

		int get_id() {
				return id;
		}

		TraceLog* get_tracelog() {
				return tracelog;
		}

	protected:
		int id;
		TraceLog *tracelog;
};

class Thread : public ThreadBase {
	public:
		Thread();
		~Thread();
		void start();
		void join();
		void run_scheduler();
		void run_one_step();

		void add_message(ThreadMessage *message);
		bool process_thread_messages();
		int process_messages();
		void clean_thread_messages();
		void process_message(ThreadMessage *message);
		Net * spawn_net(int def_index);
		void quit_all();

		#ifdef CA_MPI
		MpiRequests * get_requests() {
			return &requests;
		}
		#endif

		void send(int target, NetBase *net, int edge_id, int tokens_count, const Packer &packer) {
			// Thread can be run only over standard nets so we can safely cast
			process->send(target, static_cast<Net*>(net),
					edge_id, tokens_count, packer, this);
		}
		void send_multicast(const std::vector<int> &targets, NetBase *net,
			int edge_id, int tokens_count, const Packer &packer)
		{
			// Thread can be run only over standard nets so we can safely cast
			process->send_multicast(targets, static_cast<Net*>(net),
					edge_id, tokens_count, packer, this);
		}

		void collective_scatter_root(int transition_id, const void *data, size_t size) {
			process->collective_scatter_root(transition_id, data, size);
		}

		void collective_scatter_nonroot(int transition_id, int root, void *out, size_t size) {
			process->collective_scatter_nonroot(transition_id, root, out, size);
		}

		void collective_scatterv_root(int transition_id, const void *data, int *sizes, int *displs) {
			process->collective_scatterv_root(transition_id, data, sizes, displs);
		}

		void collective_scatterv_nonroot(int transition_id, int root, void *out, size_t size) {
			process->collective_scatterv_nonroot(transition_id, root, out, size);
		}

		void collective_gather_root(int transition_id, const void *data, size_t size, void *out) {
			process->collective_gather_root(transition_id, data, size, out);
		}

		void collective_gather_nonroot(int transition_id, int root, const void *data, size_t size) {
			process->collective_gather_nonroot(transition_id, root, data, size);
		}

		void collective_gatherv_root(int transition_id, const void *data, int size, void *out, int *sizes, int *displs) {
			process->collective_gatherv_root(transition_id, data, size, out, sizes, displs);
		}

		void collective_gatherv_nonroot(int transition_id, int root, const void *data, int size) {
			process->collective_gatherv_nonroot(transition_id, root, data, size);
		}

		void collective_bcast_root(int transition_id, const void *data, size_t size) {
			process->collective_bcast_root(transition_id, data, size);
		}

		void collective_bcast_nonroot(int transition_id, int root, void *out, size_t size) {
			process->collective_bcast_nonroot(transition_id, root, out, size);
		}

		Process * get_process() const {
			return process;
		}

		int get_process_id() const {
				return process->get_process_id();
		}

		int get_process_count() const {
				return process->get_process_count();
		}

		int get_threads_count() const {
				return process->get_threads_count();
		}

		void set_process(Process *process, int id) {
			this->process = process;
			this->id = id;
		}

		void set_tracelog(TraceLog *tracelog) {
				this->tracelog = tracelog;
		}

	protected:
		Process *process;
		pthread_t thread;
		pthread_mutex_t messages_mutex;
		ThreadMessage *messages;

		#ifdef CA_MPI
		MpiRequests requests;
		#endif
};

}

#endif
