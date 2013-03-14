
#ifndef CAILIE_PACKET_H
#define CAILIE_PACKET_H

struct Packet {
	int from_process;
	size_t size;
	void *data;
};

#endif // CAILIE_PACKET_H
