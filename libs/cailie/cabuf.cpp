#include "cabuf.h"

using namespace ca;

Caobuf::Caobuf(std::size_t buffer_size)
{
	this->packer = new Packer(buffer_size);
	this->buffer_size = buffer_size;

	this->set_buffer();
}

Caobuf::~Caobuf()
{
	delete this->packer;
}

void Caobuf::set_buffer()
{
	if (this->pptr() != NULL)
	{
		this->sync();
	}

	char * buffer_pos = static_cast<char *>(packer->peek());
	char * buffer_beg = packer->get_buffer();
	char * buffer_end = buffer_beg + this->buffer_size;

	std::size_t new_size = this->buffer_size;

	if (buffer_pos >= buffer_end)
	{
		this->packer->reserve(this->buffer_size / 2);

		new_size = (this->buffer_size * 3) - this->buffer_size;
		this->buffer_size *= 3;	// the size is added and then multiplied by two

		buffer_pos = static_cast<char *>(packer->peek());
		buffer_beg = packer->get_buffer();
	}

	setp(buffer_pos, buffer_pos + new_size - 1);
}

Caobuf::int_type Caobuf::overflow(int input)
{
	if (input != traits_type::eof())
	{
		*this->pptr() = input;
		this->pbump(1);

		this->set_buffer();

		return input;
	}
	else return traits_type::eof();
}

/*std::streamsize Caobuf::xsputn(const char * s, std::streamsize n)
{
	int put_result = 0;
	std::streamsize total_characters = n;

	while (put_result != EOF && n != 0)
	{
		put_result = this->sputc(*s);
		s++, n--;
	}

	this->sputc('\n');

	return total_characters - n;
}*/

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

Packer * Caobuf::get_packer() const
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
