
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
		void activate_transition_by_pos_id(int pos_id) {}
	};

	typedef bool (NetEqFn)(Net *, Net*);

	class Thread : public CaThreadBase {
		void quit_all() {}
		int get_process_count() const { return 1; }
		int get_threads_count() const { return 1; }
		int get_process_id() const { return 0; }
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

			size_t hash() const {
				return 1;
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
