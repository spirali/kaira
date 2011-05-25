
#ifndef CAILIE_PLACE_H
#define CAILIE_PLACE_H

#include <vector>

template<class T> class CaPlace {
	public:
		CaPlace() {}

		void add(const T &element) { list.push_back(element); }
		void add_all(const std::vector<T> &elements) {
			typename std::vector<T>::const_iterator i;
			for (i = elements.begin(); i != elements.end(); i++) {
				add(*i);
			}
		}
		void remove_at(int pos) { list.erase(list.begin() + pos); }
		T get_at(int pos) { return list[pos]; }

		std::vector<T> to_vector_and_clear() {
			std::vector<T> l = list;
			clear();
			return l;
		}

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
