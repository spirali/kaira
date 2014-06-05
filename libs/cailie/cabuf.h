
#ifndef CAILIE_H
#define CAILIE_H

#include <ios>

#include "packing.h"

namespace ca {

class Caobuf : public std::streambuf {
	private:
		const size_t fixed_allocation = 100;

		Packer * packer;

		Caobuf(Caobuf &caobuf);
		Caobuf& operator=(Caobuf &caobuf);

		void set_buffer();
		void reserve();

		virtual int_type overflow(int input);
		virtual int sync();

	public:
		Caobuf(Packer * packer);
		~Caobuf();

		Packer * get_packer();
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
