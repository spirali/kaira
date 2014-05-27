
#ifndef CAILIE_H
#define CAILIE_H

#include <ios>
#include <iostream> // test

#include "packing.h"

namespace ca {

class Caobuf : public std::streambuf {
	private:
		Packer * packer;
		std::size_t buffer_size;

		Caobuf(Caobuf &caobuf);
		Caobuf& operator=(Caobuf &caobuf);

		void set_buffer();

		virtual int_type overflow(int input);
		virtual int sync();
		//virtual std::streamsize xsputn(const char * s, std::streamsize n);

	public:
		Caobuf(std::size_t buffer_size = 100);
		~Caobuf();

		Packer * get_packer() const;
};

class Caibuf : public std::streambuf {
	private:
		Packer * packer;
		std::size_t packer_size;

		Caibuf(Caibuf &caibuf);
		Caibuf& operator=(Caibuf &caibuf);

		virtual int underflow();

	public:
		Caibuf(Packer * packer);
};

}

#endif
