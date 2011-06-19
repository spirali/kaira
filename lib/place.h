
#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include <vector>

template<class T> class CaToken {
	public:
		CaToken(const T &element) { this->element = element; }

		void remove() {
			next->prev = prev;
			prev->next = next;
		}

		T element;
		CaToken<T> *prev;
		CaToken<T> *next;
};

template<class T> class CaPlace {
	public:
		CaPlace() : token(NULL), tokens_count(0) {}

		void add(const T &element) {
			CaToken<T> *t = new CaToken<T>(element);
			if (token) {
				CaToken<T> *p = token->prev;
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
		void add_all(const std::vector<T> &elements) {
			typename std::vector<T>::const_iterator i;
			for (i = elements.begin(); i != elements.end(); i++) {
				add(*i);
			}
		}

		void remove(CaToken<T> *t) {
			if (t == token) {
				token = t->next;
				if (t == token)
					token = NULL;
			}
			t->remove();
			delete t;
			tokens_count--;
		}

		std::vector<T> to_vector_and_clear() {
			std::vector<T> v;
			if (token) {
				v.reserve(tokens_count);
				CaToken<T> *t = token;
				do {
					v.push_back(t->element);
					CaToken<T> *next = t->next;
					delete t;
					t = next;
				} while(t != token);
				token = NULL;
				tokens_count = 0;
			}
			return v;
		}

		void clear() {
			if (token) {
				CaToken<T> *t = token;
				do {
					CaToken<T> *next = t->next;
					delete t;
					t = next;
				} while(t != token);
				token = NULL;
				tokens_count = 0;
			}
		}

		CaToken<T> * begin() { return token; }

		size_t size() { return tokens_count; }

		CaToken<T> *last() { return token->prev; }

	protected:
		/* This is naive implementation, it needs benchmarks
			to choose correct implementation */
		CaToken<T> *token;
		int tokens_count;
};

#endif
