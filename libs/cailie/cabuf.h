
#ifndef CAILIE_CABUF_H
#define CAILIE_CABUF_H

#include <ios>

#include "packing.h"

namespace ca {

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
		Packer * packer;

		Caibuf(Caibuf &caibuf);
		Caibuf& operator=(Caibuf &caibuf);

		int underflow();

	public:
		Caibuf(Packer * packer);
};

}

#endif
