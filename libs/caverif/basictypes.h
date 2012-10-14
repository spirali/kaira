
#ifndef CAVERIF_BASICTYPES_H
#define CAVERIF_BASICTYPES_H

#include "net.h"
#include "thread.h"
#include "place.h"

namespace cass {


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
			Thread(Net **nets, int process_id, int thread_id)
				: nets(nets), process_id(process_id), thread_id(thread_id) {}
			void quit_all() {}
			int get_process_count() const;
			int get_threads_count() const { return 1; }
			int get_process_id() const { return process_id; }
			Net * get_net(int process_id) {
				if (process_id < 0 || process_id >= get_process_count()) {
					fprintf(stderr, "Invalid process_id %i\n", process_id);
					exit(-1);
				}
				return nets[process_id];
			}

			void set(int process_id, int thread_id) {
				this->process_id = process_id;
				this->thread_id = thread_id;
			}

		protected:
			Net **nets;
			int process_id;
			int thread_id;
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
					add(t->value);
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
