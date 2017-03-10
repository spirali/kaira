
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
			virtual Net* copy_without_tokens() = 0;
			virtual void pack(ca::Packer &pack) = 0;
			virtual void unpack(ca::Unpacker &pack) = 0;
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

	class VerifThread : public ca::ThreadBase {
		public:
			VerifThread(int process_id): process_id(process_id) {}
			int get_process_id() const {
				return process_id;
			}
			int get_process_count() const {
				return ca::process_count;
			}
			int get_threads_count() const {
				return 1;
			}
			void quit_all() { return; }
			void send(int target, ca::NetBase* net, int edge_id, int token_count, ca::Packer &packer) { return; }
			void send_multicast(const std::vector<int> &targets, ca::NetBase *net,
					int edge_id, int tokens_count, ca::Packer &packer) { return; }
		protected:
			int process_id;
	};

	struct Activation : public ca::Activation
		{
			Activation(ca::TransitionDef *transition_def, ca::Binding *binding)
				: ca::Activation(transition_def, binding) {

				ca::Packer packer;
				transition_def->pack_binding(packer, binding);
				packed_binding = packer.get_buffer();
				packed_binding_size = packer.get_size();
			}

			Activation(const Activation &a) : ca::Activation(a) {
				packed_binding_size = a.packed_binding_size;
				packed_binding = malloc(packed_binding_size);
				memcpy(packed_binding, a.packed_binding, packed_binding_size);
			}

			Activation& operator=(const Activation &a) {
				if (this != &a) {
					free(packed_binding);

					ca::Activation::operator=(a);
					packed_binding_size = a.packed_binding_size;
					packed_binding = malloc(a.packed_binding_size);
					memcpy(packed_binding, a.packed_binding, a.packed_binding_size);
				}
				return *this;
			}

			~Activation() {
				free(packed_binding);
			}

			size_t packed_binding_size;
			void *packed_binding;
		};
}


#endif // CAVERIF_BASICTYPES_H
