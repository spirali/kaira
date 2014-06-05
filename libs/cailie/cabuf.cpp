#include "cabuf.h"

using namespace ca;

Caobuf::Caobuf(Packer * packer)
{
	this->packer = packer;

	this->set_buffer();
}

Caobuf::~Caobuf()
{
	this->sync();
}

void Caobuf::set_buffer()
{
	char * buffer_beg = static_cast<char *>(packer->peek());
	char * buffer_end = buffer_beg + (packer->get_reserved_size() - packer->get_size());

	setp(buffer_beg, buffer_end - 1);
}

void Caobuf::reserve()
{
	if (this->pptr() != NULL)
	{
		this->sync();
	}

	this->packer->reserve(this->fixed_allocation);

	this->set_buffer();
}

Caobuf::int_type Caobuf::overflow(int input)
{
	if (input != traits_type::eof())
	{
		*this->pptr() = input;
		this->pbump(1);

		this->reserve();

		return input;
	}
	else return traits_type::eof();
}

int Caobuf::sync()
{
	if (this->pptr() == NULL)
	{
		return EOF;
	}
	else
	{
		char * buffer_pos = static_cast<char *>(packer->peek());
		char * buffer_put = pptr();

		if (buffer_put > buffer_pos)
		{
			packer->move(buffer_put - buffer_pos);
		}

		return 0;
	}
}

Packer * Caobuf::get_packer()
{
	return this->packer;
}

Caibuf::Caibuf(Packer * packer)
{
	this->packer = packer;
	this->packer_size = packer->get_size();

	char * buffer_beg = packer->get_buffer();

	this->setg(buffer_beg, buffer_beg, buffer_beg + this->packer_size);
}

int Caibuf::underflow()
{
	return EOF;
}
