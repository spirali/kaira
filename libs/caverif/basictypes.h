
#ifndef CAVERIF_BASICTYPES_H
#define CAVERIF_BASICTYPES_H

#include "net.h"
#include "thread.h"
#include "token.h"
#include "state.h"
#include <deque>

namespace cass {

	class Net : public ca::NetBase
	{
		public:
			virtual void pack(ca::Packer &pack) = 0;
			void activate_transition_by_pos_id(int pos_id) {}
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
