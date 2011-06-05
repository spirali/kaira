
#ifndef CAILIE_PATH_H
#define CAILIE_PATH_H

#include <string>

class CaProcess;

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

		int last_component() const;
		int owner_id(CaProcess *process, int unit_id) const;
	protected:
		int *nodes;
		void set_data(int *nodes);
};

class CaMultiPath {

    public:
        class Iterator {
            public:
                Iterator(const CaMultiPath &mpath);
                ~Iterator();
                bool has_next() { return has_next_path; }
                CaPath next();

            protected:
                const CaMultiPath &mpath;
                int *nodes;
                bool has_next_path;
        };


		CaMultiPath(const CaMultiPath &mpath);
		CaMultiPath(const std::string &definition, ...);
		~CaMultiPath();
		CaMultiPath & operator= (const CaMultiPath & mpath);

		Iterator get_iterator() { return Iterator(*this); }

	protected:
        size_t args_count() const;
        size_t path_size() const { return definition.size(); }
		std::string definition;
		int *args;
};

extern CaPath caRoot;

#endif
