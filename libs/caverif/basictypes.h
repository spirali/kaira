
#ifndef CAVERIF_BASICTYPES_H
#define CAVERIF_BASICTYPES_H

#include "net.h"
#include "thread.h"
#include "token.h"
#include <deque>

namespace cass {

	struct Packet {
		int from_process;
		size_t size;
		void *data;
	};

	class Net : public ca::NetBase
	{
		public:
			virtual Net *copy() = 0;
			virtual void pack(ca::Packer &pack) = 0;
			void activate_transition_by_pos_id(int pos_id) {}
	};

	typedef bool (NetEqFn)(Net *, Net*);

	class Thread : public ca::ThreadBase {
		public:
			Thread(std::deque<Packet> *packets, int process_id, int thread_id)
				: packets(packets),
				  process_id(process_id),
                  thread_id(thread_id),
				  quit(false) {
			}
			void quit_all() { quit = true; }
			int get_process_count() const;
			int get_threads_count() const { return 1; }
			int get_process_id() const { return process_id; }

			void set(int process_id, int thread_id) {
				this->process_id = process_id;
				this->thread_id = thread_id;
			}

			void send(int target,
					  ca::NetBase *net,
                      int place_index,
                      const ca::Packer &packer) {
				multisend(target, net, place_index, 1, packer);
			}
			void multisend(int target,
						   ca::NetBase *net,
						   int place_index,
						   int tokens_count,
                           const ca::Packer &packer) {
				std::vector<int> a(1);
				a[0] = target;
				multisend_multicast(a, net, place_index, tokens_count, packer);
			}

			void send_multicast(const std::vector<int> &targets,
							    ca::NetBase *net,
                                int place_index,
                                const ca::Packer &packer) {
				multisend_multicast(targets, net, place_index, 1, packer);
			}
			void multisend_multicast(const std::vector<int> &targets, ca::NetBase *net,
				int place_index, int tokens_count, const ca::Packer &packer);
			bool get_quit_flag() { return quit; }
		protected:
			std::deque<Packet> *packets;
			int process_id;
			int thread_id;
			bool quit;
	};

	template<typename T>
	class TokenList : public ca::TokenList<T> {
		public:
			TokenList() : ca::TokenList<T>() {}

			TokenList(TokenList &place) : ca::TokenList<T>() {
				if (place.token == NULL) {
					return;
				}
				ca::Token<T> *t = place.token;
				do {
					this->add(t->value);
					t = t->next;
				} while (t != place.token);

			}
	};
}


#endif // CAVERIF_BASICTYPES_H
