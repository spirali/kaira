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

Caobuf::int_type Caobuf::overflow(int input)
{
	if (input != traits_type::eof())
	{
		*this->pptr() = input;
		this->pbump(1);

		this->sync();

		return input;
	}
	else return traits_type::eof();
}

int Caobuf::sync()	// synchronizes the buffer position in Packer, which also reserves some size
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
			this->set_buffer();
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

	char * buffer_beg = packer->get_buffer();
	char * buffer_end = buffer_beg + packer->get_size();

	setg(buffer_beg, buffer_beg, buffer_end);
}

int Caibuf::underflow()
{
	return EOF;
}
