
#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include <vector>

template<class T> class CaPlace {
	public:
		CaPlace() {}

		void add(T element) { list.push_back(element); }
		void remove_at(int pos) { list.erase(list.begin() + pos); }
		T get_at(int pos) { return list[pos]; }
		void clear() { list.clear(); }
		size_t size() { return list.size(); }
		T operator[] (size_t pos) { return list[pos]; }
		std::vector<T> as_vector() { return list; }

	protected:
		/* This is naive implementation, it needs benchmarks
			to choose correct implementation */
		std::vector<T> list;

};

#endif
