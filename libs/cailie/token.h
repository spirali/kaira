#ifndef CAILIE_TOKEN_H
#define CAILIE_TOKEN_H

#include <vector>
#include <string>
#include "tracelog.h"
#include "packing.h"

namespace ca {

template<typename T> class Token {

	public:
		Token() : prev(NULL), next(NULL) {}
		Token(const T &value) : value(value), prev(NULL), next(NULL) {}

		void remove() {
			next->prev = prev;
			prev->next = next;
		}

		T value;
		Token<T> *prev;
		Token<T> *next;
};


template<typename T> class TokenList {

	public:
		TokenList() : token(NULL), tokens_count(0) {}
		~TokenList() {
			clear();
		}

		TokenList(const TokenList<T> &list) {
			token = NULL;
			tokens_count = 0;
			copy_tokens(list);
		}

		TokenList(TokenList<T> &list, bool overtake) {
			if (overtake) {
				this->token = list.token;
				this->tokens_count = list.tokens_count;
				list.token = NULL;
				list.tokens_count = 0;
			} else {
				token = NULL;
				tokens_count = 0;
				copy_tokens(list);
			}
		}

		void overtake(TokenList<T> &list) {
			if (list.token == NULL) {
				return;
			}

			if (token == NULL) {
				this->token = list.token;
				this->tokens_count = list.tokens_count;
				list.token = NULL;
				list.tokens_count = 0;
				return;
			}

			Token<T> *tbegin = list.begin();
			Token<T> *tlast = list.last();
			Token<T> *my_tbegin = begin();
			Token<T> *my_tlast = last();

			tlast->next = my_tbegin;
			my_tbegin->prev = tlast;

			my_tlast->next = tbegin;
			tbegin->prev = my_tlast;

			tokens_count += list.tokens_count;
			list.token = NULL;
			list.tokens_count = 0;
		}

		void copy_tokens(const TokenList<T> &list) {
			Token<T> *t;
			for (t = list.begin(); t != NULL; t = list.next(t)) {
				add(t->value);
			}
		}

		void add(const T &value) {
			Token<T> *t = new Token<T>(value);
			add_token(t);
		}

		void add(const std::vector<T> &values) {
			typename std::vector<T>::const_iterator i;
			for (i = values.begin(); i != values.end(); i++) {
				add(*i);
			}
		}

		void add_token(Token<T> *t) {
			if (token) {
				Token<T> *p = token->prev;
				t->next = token;
				t->prev = p;
				token->prev = t;
				p->next = t;
			} else {
				token = t;
				t->next = t;
				t->prev = t;
			}
			tokens_count++;
		}

		void remove(Token<T> *t)
		{
			if (t == token) {
				token = t->next;
				if (t == token)
					token = NULL;
			}
			t->remove();
			tokens_count--;
		}

		std::vector<T> to_vector_and_clear() {
			std::vector<T> v;
			if (token) {
				v.reserve(tokens_count);
				Token<T> *t = token;
				do {
					v.push_back(t->value);
					Token<T> *next = t->next;
					delete t;
					t = next; } while(t != token);
				token = NULL;
				tokens_count = 0;
			}
			return v;
		}

		std::vector<T> to_vector() const {
			std::vector<T> v;
			if (token) {
				v.reserve(tokens_count);
				Token<T> *t = token;
				do {
					v.push_back(t->value);
					t = t->next;
				} while(t != token);
			}
			return v;
		}

		void clear() {
			if (token) {
				Token<T> *t = token;
				do {
					Token<T> *next = t->next;
					delete t;
					t = next;
				} while(t != token);
				token = NULL;
				tokens_count = 0;
			}
		}

		void pack(Packer &packer) const {
			packer << tokens_count;
			pack_tokens(packer);

		}

		void pack_tokens(Packer &packer) const {
			if (token) {
				Token<T> *t = token;
				do {
					packer << t->value;
					t = t->next;
				} while (t != token);
			}
		}

		Token<T> * begin() const {
			return token;
		}

		size_t size() const {
			return tokens_count;
		}

		Token<T> * last() const {
			return token->prev;
		}

		Token<T> * last(int s) {
			Token<T> *t = token;
			for (int i = 0; i < s; s++) {
				t = t->prev;
			}
			return t;
		}

		bool is_empty() const {
			return token == NULL;
		}

		Token<T> * next(Token<T> *token) const {
			if (token != last()) {
				return token->next;
			} else {
				return NULL;
			}
		}

		T first_value() const {
			return token->value;
		}

		TokenList<T> & operator= (const TokenList &other)
		{
			if (this != &other)
			{
				clear();
				copy_tokens(other);
			}
			return *this;
		}

	protected:
		Token<T> *token;
		size_t tokens_count;
};

}

#endif
