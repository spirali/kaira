
#ifndef CAILIE_PATH_H
#define CAILIE_PATH_H

#include <string>

class CaPath {
	public:
		CaPath(int *nodes);
		CaPath(const CaPath &path);
		CaPath(int count, ...);
		CaPath(const char *string);
		~CaPath();

		CaPath & operator= (const CaPath & path);
		bool operator== (const CaPath & path);

		int depth() const;
		CaPath apply(int levelup, int count, ...);

		std::string as_string() const;
	protected:
		int *nodes;
		void set_data(int *nodes);
};

extern CaPath caRoot;

#endif
