
#ifndef CAILIE_CABUF_H
#define CAILIE_CABUF_H

#include <ios>
#include <sstream>

#include "packing.h"

namespace ca {

class Caobuf : public std::streambuf {
	private:
		Packer* packer;
		size_t offset_beg;

		Caobuf(Caobuf &caobuf);
		Caobuf& operator=(Caobuf &caobuf);

		void set_buffer();

		void update_size();
		size_t count_buffer_size();

		int_type overflow(int input);

	public:
		Caobuf(Packer* packer);
		~Caobuf();

		Packer* get_packer();
		int sync();
};

class Caibuf : public std::streambuf {
	private:
		Unpacker* unpacker;

		Caibuf(Caibuf &caibuf);
		Caibuf& operator=(Caibuf &caibuf);

		std::streambuf::int_type underflow();

	public:
		Caibuf(Unpacker* unpacker);
};

class Caostream : public std::ostream {
	private:
		Caostream(Caostream &caostream);
		Caostream& operator=(Caostream &caostream);
		Caobuf* caobuf;

	public:
		Caostream(Packer* packer);
		~Caostream();

		void sync();
};

class Caistream : public std::istream {
	private:
		Caistream(Caistream &caistream);
		Caistream& operator=(Caistream &caistream);
		Caibuf* caibuf;

	public:
		Caistream(Unpacker* unpacker);
		~Caistream();
};

}

#endif
