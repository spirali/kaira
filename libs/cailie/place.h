#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include <vector>
#include <string>
#include "tracelog.h"

namespace ca {

template<typename T> class Token {

	public:
		Token(const T &value) : value(value) {}

		void remove() {
			next->prev = prev;
			prev->next = next;
		}

		void trace_add_token(TraceLog *tracelog, int place_id) {
			tracelog->trace_token_add(place_id, this);
		}

		void trace_remove_token(TraceLog *tracelog, int place_id) {
					tracelog->trace_token_remove(place_id, this);
		}

		void trace(TraceLog *tracelog, int place_id, int n, void (*fncs[])(TraceLog *, const T&)) {
			tracelog->trace_token_add(place_id, this);
			for (int i = 0; i < n; i++) {
				fncs[i](tracelog, value);
			}
		}

		T value;
		Token<T> *prev;
		Token<T> *next;
};

template<typename T> class Place {

	public:
		Place() : token(NULL), tokens_count(0) {}
		~Place() { clear(); }

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

		void add_token(Token<T> *t, TraceLog *tracelog, int place_id)
		{
			t->trace(tracelog, place_id);
			add_token(t);
		}


		void add_token(Token<T> *t, TraceLog *tracelog, int place_id, int n, void (*fncs[]) (TraceLog *, const T&))
		{
			t->trace(tracelog, place_id, n, fncs);
			add_token(t);
		}

		void add(const T &value) {
			Token<T> *t = new Token<T>(value);
			add_token(t);
		}

		void add(const T &value, TraceLog *tracelog, int place_id)
		{
			Token<T> *t = new Token<T>(value);
			add_token(t, tracelog, place_id);
		}

		void add(const T &value, TraceLog *tracelog, int place_id, int n, void (*fncs[]) (TraceLog *, const T&))
		{
			Token<T> *t = new Token<T>(value);
			add_token(t, tracelog, place_id, n, fncs);
		}

		void add_all(const std::vector<T> &values) {
			typename std::vector<T>::const_iterator i;
			for (i = values.begin(); i != values.end(); i++) {
				add(*i);
			}
		}

		void add_all(const std::vector<T> &values,
					TraceLog *tracelog,
					int place_id,
					int n,
					void (*fncs[]) (TraceLog *, const T&)) {
			typename std::vector<T>::const_iterator i;
			for (i = values.begin(); i != values.end(); i++) {
				add(*i, tracelog, place_id, n, fncs);
			}
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

		void remove(Token<T> *t, TraceLog *tracelog, int place_id)
		{
			t->trace_remove_token(tracelog, place_id);
			remove(t);
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

		Token<T> * begin() const { return token; }
		size_t size() const { return tokens_count; }
		Token<T> * last() const { return token->prev; }
		bool is_empty() const { return token == NULL; }
		T first_value() const { return token->value; }

	protected:
		/* This is a naive implementation, it needs benchmarks
			to choose correct implementation */
		Token<T> *token;
		int tokens_count;
};

}
#endif
