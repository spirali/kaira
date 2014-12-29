
#ifndef CAILIE_CABUF_H
#define CAILIE_CABUF_H

#include <ios>
#include <sstream>
#include <iostream>

#include "packing.h"

namespace ca {

class PackerStream : public std::stringstream {

	public:

	template <typename T>
	std::stringstream & operator<<(const T &a)
	{
		std::cout << "ahoj" << std::endl;
		return *this;
	}
};

class Caobuf : public std::streambuf {
	private:
		Packer * packer;

		Caobuf(Caobuf &caobuf);
		Caobuf& operator=(Caobuf &caobuf);

		void set_buffer();
		void reserve();

		int_type overflow(int input);
		int sync();

	public:
		Caobuf(Packer * packer);
		~Caobuf();

		Packer * get_packer();
};

class Caibuf : public std::streambuf {
	private:
		Unpacker * unpacker;

		Caibuf(Caibuf &caibuf);
		Caibuf& operator=(Caibuf &caibuf);

		std::streambuf::int_type underflow();
		std::streamsize xsgetn(char* s, std::streamsize n);
		int sync();

	public:
		Caibuf(Unpacker * unpacker);
};

}

#endif
