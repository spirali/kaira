#include "cabuf.h"

using namespace ca;

Caobuf::Caobuf(Packer * packer)
{
	this->packer = packer;

	this->set_buffer();
}

Caobuf::~Caobuf()
{
	this->sync();	// synchronizes and flushes the buffer
}

void Caobuf::set_buffer()
{
	char * buffer_beg = static_cast<char *>(packer->peek());
	char * buffer_end = buffer_beg + (packer->get_reserved_size() - packer->get_size());

	setp(buffer_beg, buffer_end - 1);	// -1 so that overflow doesn't need to be handled specially
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

Caibuf::Caibuf(Unpacker * unpacker)
{
	this->unpacker = unpacker;

	char * buffer_beg = static_cast<char*>(unpacker->peek());

	setg(buffer_beg, buffer_beg, buffer_beg + unpacker->get_size());
}

std::streamsize Caibuf::xsgetn(char* s, std::streamsize n)
{
	this->unpacker->unpack_data(s, n);

	return n;
}

std::streambuf::int_type Caibuf::underflow()
{
	return EOF;
}

int Caibuf::sync()
{
	char* start = this->eback();
	char* current_position = this->gptr();

	if (start != current_position)
	{
		this->unpacker->move(current_position - start);
	}

	char next_byte = *static_cast<char*>(unpacker->peek());

	if (next_byte == '\n')
	{
		unpacker->move(1);
	}

	return 0;
}
