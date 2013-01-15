
#ifndef CAVERIF_BASICTYPES_H
#define CAVERIF_BASICTYPES_H

#include "net.h"
#include "thread.h"
#include "place.h"
#include <deque>

namespace cass {

	struct Packet {
		size_t size;
		void *data;
	};

	class Net : public CaNetBase
	{
		public:
			virtual Net *copy() = 0;
			virtual bool is_equal(const Net &net) const = 0;
			virtual size_t hash() = 0;
			void activate_transition_by_pos_id(int pos_id) {}
	};

	typedef bool (NetEqFn)(Net *, Net*);

	class Thread : public CaThreadBase {
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
					  CaNetBase *net,
                      int place_index,
                      const CaPacker &packer) {
				multisend(target, net, place_index, 1, packer);
			}
			void multisend(int target,
						   CaNetBase *net,
						   int place_index,
						   int tokens_count,
                           const CaPacker &packer) {
				std::vector<int> a(1);
				a[0] = target;
				multisend_multicast(a, net, place_index, tokens_count, packer);
			}

			void send_multicast(const std::vector<int> &targets,
							    CaNetBase *net,
                                int place_index,
                                const CaPacker &packer) {
				multisend_multicast(targets, net, place_index, 1, packer);
			}
			void multisend_multicast(const std::vector<int> &targets, CaNetBase *net,
				int place_index, int tokens_count, const CaPacker &packer);
			bool get_quit_flag() { return quit; }
		protected:
			std::deque<Packet> *packets;
			int process_id;
			int thread_id;
			bool quit;
	};

	template<typename T>
	class Place : public CaPlace<T> {
		public:
			Place() : CaPlace<T>() {}

			Place(Place &place) : CaPlace<T>() {
				if (place.token == NULL) {
					return;
				}
				CaToken<T> *t = place.token;
				do {
					this->add(t->value);
					t = t->next;
				} while (t != place.token);

			}

			size_t hash(size_t (hash_fn)(const T&)) const {
				if (this->token == NULL) {
					return 0;
				}
				CaToken<T> *t = this->token;
				size_t h = 0;
				do {
					h += hash_fn(t->value);
					h += h << 10;
					h ^= h >> 6;
					t = t->next;
				} while (t != this->token);
				h += h << 3;
				h ^= h >> 11;
				h += h << 15;
				return h;
			}

			bool operator==(const Place<T> &rhs) const {
				if (rhs.tokens_count != this->tokens_count) {
					return false;
				}
				if (this->tokens_count == 0) {
					return true;
				}
				CaToken<T> *t1 = this->token;
				CaToken<T> *t2 = rhs.token;
				do {
					if (t1->value != t2->value) {
						return false;
					}
					t1 = t1->next;
					t2 = t2->next;
				} while (t1 != this->token);
				return true;
			}
	};
}


#endif // CAVERIF_BASICTYPES_H
