
#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include <vector>
#include <string>
#include "tracelog.h"

template<class T> class CaToken {

	public:
		CaToken(const T &value) : value(value) {}

		void remove() {
			next->prev = prev;
			prev->next = next;
		}

		void trace(CaTraceLog *tracelog, int place_id, bool new_token) {
			tracelog->trace_token(place_id, this, new_token);
		}

		void trace(CaTraceLog *tracelog, int place_id, int n, void (*fncs[])(CaTraceLog *, const T&)) {
			tracelog->trace_token(place_id, this, true);
			for (int i = 0; i < n; i++) {
				fncs[i](tracelog, value);
			}
		}

		T value;
		CaToken<T> *prev;
		CaToken<T> *next;
};

template<class T> class CaPlace {

	public:
		CaPlace() : token(NULL), tokens_count(0) {}
		~CaPlace() { clear(); }

		void add(CaToken<T> *t) {
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

		void add(const T &value) {
			CaToken<T> *t = new CaToken<T>(value);
			add(t);
		}

		void add(const T &value, CaTraceLog *tracelog, int place_id)
		{
			CaToken<T> *t = new CaToken<T>(value);
			t->trace(tracelog, place_id, true);
			add(t);
		}

		void add(const T &value, CaTraceLog *tracelog, int place_id, int n, void (*fncs[]) (CaTraceLog *, const T&))
		{
			CaToken<T> *t = new CaToken<T>(value);
			t->trace(tracelog, place_id, n, fncs);
			add(t);
		}

		void add_all(const std::vector<T> &values) {
			typename std::vector<T>::const_iterator i;
			for (i = values.begin(); i != values.end(); i++) {
				add(*i);
			}
		}

		void add_all(const std::vector<T> &values,
					CaTraceLog *tracelog,
					int place_id,
					int n,
					void (*fncs[]) (CaTraceLog *, const T&)) {
			typename std::vector<T>::const_iterator i;
			for (i = values.begin(); i != values.end(); i++) {
				add(*i, tracelog, place_id, n, fncs);
			}
		}

		void remove(CaToken<T> *t)
		{
			if (t == token) {
				token = t->next;
				if (t == token)
					token = NULL;
			}
			t->remove();
			tokens_count--;
		}

		void remove(CaToken<T> *t, CaTraceLog *tracelog, int place_id)
		{
			t->trace(tracelog, place_id, false);
			remove(t);
		}

		std::vector<T> to_vector_and_clear() {
			std::vector<T> v;
			if (token) {
				v.reserve(tokens_count);
				CaToken<T> *t = token;
				do {
					v.push_back(t->value);
					CaToken<T> *next = t->next;
					delete t;
					t = next; } while(t != token);
				token = NULL;
				tokens_count = 0;
			}
			return v;
		}

		std::vector<T> to_vector() {
			std::vector<T> v;
			if (token) {
				v.reserve(tokens_count);
				CaToken<T> *t = token;
				do {
					v.push_back(t->value);
					t = t->next;
				} while(t != token);
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
		CaToken<T> * last() { return token->prev; }
		bool is_empty() { return token == NULL; }
		T first_value() { return token->value; }

	protected:
		/* This is a naive implementation, it needs benchmarks
			to choose correct implementation */
		CaToken<T> *token;
		int tokens_count;
};

#endif
