#include "cabuf.h"

using namespace ca;

obuf::obuf(Packer& packer) : packer(packer)
{
	this->offset_beg = packer.get_size();	// get offset from the buffer beginning
	packer.move(sizeof(size_t));			// reserve space for stream buffer length

	this->set_buffer();
}

obuf::~obuf()
{
	this->sync();	// synchronizes and flushes the buffer
}

void obuf::set_buffer()
{
	char * buffer = static_cast<char *>(packer.peek());
	char * buffer_end = buffer + (packer.get_reserved_size() - packer.get_size());

	if (buffer == buffer_end)
	{
		packer.reserve(1);	// if the packer is full, reallocate it

		buffer = static_cast<char *>(packer.peek());
		buffer_end = buffer + (packer.get_reserved_size() - packer.get_size());
	}

	setp(buffer, buffer_end - 1);	// -1 so that overflow doesn't need to be handled specially
}

size_t obuf::count_buffer_size()
{
	return this->packer.get_size() - (this->offset_beg + sizeof(size_t));
}
void obuf::update_size()
{
	this->sync();
	size_t buffer_size = this->count_buffer_size();

	size_t* size = reinterpret_cast<size_t*>(this->packer.get_buffer() + this->offset_beg);
	*size = buffer_size;
}

obuf::int_type obuf::overflow(int input)
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

int obuf::sync()	// synchronizes the buffer position in Packer, which also reserves some size
{
	if (this->pptr() == NULL)
	{
		return EOF;
	}
	else
	{
		char * buffer_pos = static_cast<char *>(packer.peek());
		char * buffer_put = pptr();

		if (buffer_put > buffer_pos) // move the packer's pointer to match the stream pointer
		{
			packer.move(buffer_put - buffer_pos);
			this->update_size();

			this->set_buffer();
		}

		return 0;
	}
}

Packer& obuf::get_packer()
{
	return this->packer;
}

ibuf::ibuf(Unpacker& unpacker) : unpacker(unpacker)
{
	size_t buffer_size;
	unpacker.direct_unpack(buffer_size);

	char* buffer_beg = static_cast<char*>(unpacker.peek());

	setg(buffer_beg, buffer_beg, buffer_beg + buffer_size);

	unpacker.move(buffer_size);	// skip the whole stream buffer in unpacker
}

std::streambuf::int_type ibuf::underflow()
{
	return EOF;
}

ostream::ostream(Packer& packer) : caobuf(packer)
{
	this->rdbuf(&this->caobuf);
}
ostream::~ostream()
{

}

void ostream::sync()
{
	this->caobuf.sync();	// synchronizes the buffer
}

istream::istream(Unpacker& unpacker) : caibuf(unpacker)
{
	this->rdbuf(&this->caibuf);
}
istream::~istream()
{

}
