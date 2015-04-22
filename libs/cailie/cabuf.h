
#ifndef CAILIE_CABUF_H
#define CAILIE_CABUF_H

#include <ios>
#include <sstream>

#include "packing.h"

namespace ca {

class obuf : public std::streambuf {
	private:
		Packer& packer;
		size_t offset_beg;

		obuf(const obuf &caobuf);
		obuf& operator=(obuf &caobuf);

		void set_buffer();

		void update_size();
		size_t count_buffer_size();

		int_type overflow(int input);

	public:
		obuf(Packer& packer);
		~obuf();

		Packer& get_packer();
		int sync();
};

class ibuf : public std::streambuf {
	private:
		Unpacker& unpacker;

		ibuf(const ibuf &caibuf);
		ibuf& operator=(ibuf &caibuf);

		std::streambuf::int_type underflow();

	public:
		ibuf(Unpacker& unpacker);
};

class ostream : public std::ostream {
	private:
		ostream(const ostream &caostream);
		ostream& operator=(ostream &caostream);
		obuf caobuf;

	public:
		ostream(Packer& packer);
		~ostream();

		void sync();
};

class istream : public std::istream {
	private:
		istream(const istream &caistream);
		istream& operator=(istream &caistream);
		ibuf caibuf;

	public:
		istream(Unpacker& unpacker);
		~istream();
};

}

#endif
