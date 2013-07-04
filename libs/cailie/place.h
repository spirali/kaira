#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include "token.h"
#include <map>

namespace ca {

template <typename T> class Place {
	public:
		void overtake(TokenList<T> &list) {
			token_list.overtake(list);
		}

		void overtake(Place<T> &place) {
			place.put_into(token_list);
		}

		void put_into(TokenList<T> &list) {
			list.overtake(token_list);
		}

		void copy_tokens(TokenList<T> &list) {
			token_list.copy_tokens(list);
		}

		void add(const T &value) {
			token_list.add(value);
		}


		void add(const std::vector<T> &values) {
			token_list.add(values);
		}

		void add_token(Token<T> *t) {
			token_list.add_token(t);
		}

		void remove(Token<T> *t)
		{
			token_list.remove(t);
		}

		size_t size() {
			return token_list.size();
		}

		Token<T>* begin() {
			return token_list.begin();
		}

		Token<T>* last() {
			return token_list.last();
		}

		Token<T>* next(Token<T> *t) {
			return token_list.next(t);
		}

		void pack(Packer &packer) {
			ca::pack(packer, this->token_list);
		}

	protected:
		TokenList<T> token_list;
};


template <typename T> class PlaceWithSource : public Place<T>
{
	public:

		void set_default_source(int source) {
			default_source = source;
		}

		void overtake(TokenList<T> &list) {
			overtake(list, default_source);
		}

		void overtake(TokenList<T> &list, int source) {
			for (Token<T> *t = list.begin(); t != NULL; t = list.next(t)) {
				sources[t] = source;
			}
			this->token_list.overtake(list);
		}

		void overtake(PlaceWithSource<T> &place) {
			place.put_into(this->token_list);
			place.sources.clear();
			sources = place.sources;
		}

		void sorted_put_into(TokenList<T> &list) {
			std::vector<std::pair<Token<T>*,int> > v(sources.begin(), sources.end());
			std::sort(v.begin(), v.end(), sort_helper);
			typename std::vector<std::pair<Token<T>*,int> >::iterator i;
			for (i = v.begin(); i != v.end(); i++) {
				remove(i->first);
				list.add_token(i->first);
			}
			sources.clear();
		}

		void copy_tokens(TokenList<T> &list) {
			this->token_list.copy_tokens(list);
		}

		void add(const T &value) {
			add(value, default_source);
		}

		void add(const std::vector<T> &values) {
			add(values, default_source);
		}

		void add(const T &value, int source) {
			this->token_list.add(value);
			sources[this->token_list.last()] = source;
		}

		void add(const std::vector<T> &values, int source) {
			for (int i = 0; i < values.size(); i++) {
				add(values[i], source);
			}
		}

		void add_token(Token<T> *t) {
			add_token(t, default_source);
		}

		void add_token(Token<T> *t, int source) {
			sources[t] = source;
			this->token_list.add_token(t);
		}

		void remove(Token<T> *t)
		{
			sources.erase(t);
			this->token_list.remove(t);
		}

		int get_source(Token<T> *t)
		{
			return sources[t];
		}

		std::vector<int> get_sources()
		{
			std::vector<int> result;
			for (Token<T> *t = this->token_list.begin();
			     t != NULL;
				 t = this->token_list.next(t)) {
				result.push_back(sources[t]);
			}
			return result;
		}

		void pack(Packer &packer)
		{
			ca::pack(packer, this->token_list);
			for (Token<T> *t = this->token_list.begin();
				 t != NULL;
				 t = this->token_list.next(t)) {
				ca::pack(packer, (sources[t]));
			}
		}

	protected:
		std::map<Token<T>*, int> sources;
		int default_source;

		static int sort_helper(const std::pair<Token<T>*, int> &p1,
							   const std::pair<Token<T>*, int> &p2) {
			return p1.second < p2.second;
		}
};

template<typename T> void pack(Packer &packer, Place<T> &place)
{
	place.pack(packer);
}

template<typename T> void pack(Packer &packer, PlaceWithSource<T> &place)
{
	place.pack(packer);
}

}

#endif // CAILIE_PLACE_H
